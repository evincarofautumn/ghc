%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcSplice]{Template Haskell splices}

\begin{code}
module TcSplice( tcSpliceExpr, tcSpliceDecls, tcBracket ) where

#include "HsVersions.h"

import HscMain		( compileExpr )
import TcRnDriver	( tcTopSrcDecls )
	-- These imports are the reason that TcSplice 
	-- is very high up the module hierarchy

import qualified Language.Haskell.TH.THSyntax as TH
-- THSyntax gives access to internal functions and data types

import HscTypes		( HscEnv(..) )
import HsSyn		( HsBracket(..), HsExpr(..) )
import Convert		( convertToHsExpr, convertToHsDecls )
import RnExpr		( rnExpr )
import RnEnv		( lookupFixityRn )
import RdrHsSyn		( RdrNameHsExpr, RdrNameHsDecl )
import RnHsSyn		( RenamedHsExpr )
import TcExpr		( tcCheckRho, tcMonoExpr )
import TcHsSyn		( TcExpr, TypecheckedHsExpr, mkHsLet, zonkTopExpr )
import TcSimplify	( tcSimplifyTop, tcSimplifyBracket )
import TcUnify		( Expected, zapExpectedTo, zapExpectedType )
import TcType		( TcType, openTypeKind, mkAppTy, tcSplitSigmaTy )
import TcEnv		( spliceOK, tcMetaTy, bracketOK, tcLookup )
import TcMType		( newTyVarTy, UserTypeCtxt(ExprSigCtxt), zonkTcType, zonkTcTyVar )
import TcHsType		( tcHsSigType )
import TypeRep		( Type(..), PredType(..), TyThing(..) )	-- For reification
import Name		( Name, NamedThing(..), nameOccName, nameModule, isExternalName )
import OccName
import Var		( TyVar, idType )
import Module		( moduleUserString, mkModuleName )
import TcRnMonad
import IfaceEnv		( lookupOrig )

import Class		( Class, classBigSig )
import TyCon		( TyCon, tyConTheta, tyConTyVars, getSynTyConDefn, isSynTyCon, isNewTyCon, tyConDataCons )
import DataCon		( DataCon, dataConTyCon, dataConOrigArgTys, dataConStrictMarks, 
			  dataConName, dataConFieldLabels, dataConWrapId )
import Id		( idName, globalIdDetails )
import IdInfo		( GlobalIdDetails(..) )
import TysWiredIn	( mkListTy )
import DsMeta		( expQTyConName, typeQTyConName, decTyConName, qTyConName, nameTyConName )
import ErrUtils		( Message )
import Outputable
import Unique		( Unique, Uniquable(..), getKey )
import IOEnv		( IOEnv )
import BasicTypes	( StrictnessMark(..), Fixity(..), FixityDirection(..) )
import Module		( moduleUserString )
import Panic		( showException )
import GHC.Base		( unsafeCoerce#, Int(..) )	-- Should have a better home in the module hierarchy
import Monad 		( liftM )
import FastString	( LitString )
import FastTypes	( iBox )
\end{code}


%************************************************************************
%*									*
\subsection{Main interface + stubs for the non-GHCI case
%*									*
%************************************************************************

\begin{code}
tcSpliceDecls :: RenamedHsExpr -> TcM [RdrNameHsDecl]

tcSpliceExpr :: Name 
	     -> RenamedHsExpr
	     -> Expected TcType
	     -> TcM TcExpr

#ifndef GHCI
tcSpliceExpr n e ty = pprPanic "Cant do tcSpliceExpr without GHCi" (ppr e)
tcSpliceDecls e     = pprPanic "Cant do tcSpliceDecls without GHCi" (ppr e)
#else
\end{code}

%************************************************************************
%*									*
\subsection{Quoting an expression}
%*									*
%************************************************************************

\begin{code}
tcBracket :: HsBracket Name -> Expected TcType -> TcM TcExpr
tcBracket brack res_ty
  = getStage 				`thenM` \ level ->
    case bracketOK level of {
	Nothing         -> failWithTc (illegalBracket level) ;
	Just next_level ->

   	-- Typecheck expr to make sure it is valid,
	-- but throw away the results.  We'll type check
	-- it again when we actually use it.
    newMutVar []	 		`thenM` \ pending_splices ->
    getLIEVar				`thenM` \ lie_var ->

    setStage (Brack next_level pending_splices lie_var) (
	getLIE (tc_bracket brack)
    )					`thenM` \ (meta_ty, lie) ->
    tcSimplifyBracket lie 		`thenM_`  

	-- Make the expected type have the right shape
    zapExpectedTo res_ty meta_ty	`thenM_`

	-- Return the original expression, not the type-decorated one
    readMutVar pending_splices		`thenM` \ pendings ->
    returnM (HsBracketOut brack pendings)
    }

tc_bracket :: HsBracket Name -> TcM TcType
tc_bracket (VarBr v) 
  = tcMetaTy nameTyConName
	-- Result type is Var (not Q-monadic)

tc_bracket (ExpBr expr) 
  = newTyVarTy openTypeKind	`thenM` \ any_ty ->
    tcCheckRho expr any_ty	`thenM_`
    tcMetaTy expQTyConName
	-- Result type is Expr (= Q Exp)

tc_bracket (TypBr typ) 
  = tcHsSigType ExprSigCtxt typ		`thenM_`
    tcMetaTy typeQTyConName
	-- Result type is Type (= Q Typ)

tc_bracket (DecBr decls)
  = tcTopSrcDecls decls		`thenM_`
	-- Typecheck the declarations, dicarding the result
	-- We'll get all that stuff later, when we splice it in

    tcMetaTy decTyConName	`thenM` \ decl_ty ->
    tcMetaTy qTyConName		`thenM` \ q_ty ->
    returnM (mkAppTy q_ty (mkListTy decl_ty))
	-- Result type is Q [Dec]
\end{code}


%************************************************************************
%*									*
\subsection{Splicing an expression}
%*									*
%************************************************************************

\begin{code}
tcSpliceExpr name expr res_ty
  = getStage		`thenM` \ level ->
    case spliceOK level of {
	Nothing 	-> failWithTc (illegalSplice level) ;
	Just next_level -> 

    case level of {
	Comp 		       -> tcTopSplice expr res_ty ;
	Brack _ ps_var lie_var ->  

	-- A splice inside brackets
  	-- NB: ignore res_ty, apart from zapping it to a mono-type
	-- e.g.   [| reverse $(h 4) |]
	-- Here (h 4) :: Q Exp
	-- but $(h 4) :: forall a.a 	i.e. anything!

    zapExpectedType res_ty			`thenM_`
    tcMetaTy expQTyConName			`thenM` \ meta_exp_ty ->
    setStage (Splice next_level) (
	setLIEVar lie_var	   $
	tcCheckRho expr meta_exp_ty
    )						`thenM` \ expr' ->

	-- Write the pending splice into the bucket
    readMutVar ps_var				`thenM` \ ps ->
    writeMutVar ps_var ((name,expr') : ps) 	`thenM_`

    returnM (panic "tcSpliceExpr")	-- The returned expression is ignored
    }} 

-- tcTopSplice used to have this:
-- Note that we do not decrement the level (to -1) before 
-- typechecking the expression.  For example:
--	f x = $( ...$(g 3) ... )
-- The recursive call to tcMonoExpr will simply expand the 
-- inner escape before dealing with the outer one

tcTopSplice expr res_ty
  = tcMetaTy expQTyConName		`thenM` \ meta_exp_ty ->

	-- Typecheck the expression
    tcTopSpliceExpr expr meta_exp_ty	`thenM` \ zonked_q_expr ->

	-- Run the expression
    traceTc (text "About to run" <+> ppr zonked_q_expr) 	`thenM_`
    runMetaE zonked_q_expr		`thenM` \ simple_expr ->
  
    let 
	-- simple_expr :: TH.Exp

	expr2 :: RdrNameHsExpr
	expr2 = convertToHsExpr simple_expr 
    in
    traceTc (text "Got result" <+> ppr expr2) 	`thenM_`

    showSplice "expression" 
	       zonked_q_expr (ppr expr2)	`thenM_`

	-- Rename it, but bale out if there are errors
	-- otherwise the type checker just gives more spurious errors
    checkNoErrs (rnExpr expr2)			`thenM` \ (exp3, fvs) ->

    tcMonoExpr exp3 res_ty


tcTopSpliceExpr :: RenamedHsExpr -> TcType -> TcM TypecheckedHsExpr
-- Type check an expression that is the body of a top-level splice
--   (the caller will compile and run it)
tcTopSpliceExpr expr meta_ty
  = checkNoErrs $	-- checkNoErrs: must not try to run the thing
			--	        if the type checker fails!

    setStage topSpliceStage $

	-- Typecheck the expression
    getLIE (tcCheckRho expr meta_ty)	`thenM` \ (expr', lie) ->

	-- Solve the constraints
    tcSimplifyTop lie			`thenM` \ const_binds ->
	
	-- And zonk it
    zonkTopExpr (mkHsLet const_binds expr')
\end{code}


%************************************************************************
%*									*
\subsection{Splicing an expression}
%*									*
%************************************************************************

\begin{code}
-- Always at top level
tcSpliceDecls expr
  = tcMetaTy decTyConName		`thenM` \ meta_dec_ty ->
    tcMetaTy qTyConName    		`thenM` \ meta_q_ty ->
    let
	list_q = mkAppTy meta_q_ty (mkListTy meta_dec_ty)
    in
    tcTopSpliceExpr expr list_q		`thenM` \ zonked_q_expr ->

	-- Run the expression
    traceTc (text "About to run" <+> ppr zonked_q_expr) 	`thenM_`
    runMetaD zonked_q_expr		`thenM` \ simple_expr ->
    -- simple_expr :: [TH.Dec]
    -- decls :: [RdrNameHsDecl]
    handleErrors (convertToHsDecls simple_expr) `thenM` \ decls ->
    traceTc (text "Got result" <+> vcat (map ppr decls))	`thenM_`
    showSplice "declarations"
	       zonked_q_expr (vcat (map ppr decls))		`thenM_`
    returnM decls

  where handleErrors :: [Either a Message] -> TcM [a]
        handleErrors [] = return []
        handleErrors (Left x:xs) = liftM (x:) (handleErrors xs)
        handleErrors (Right m:xs) = do addErrTc m
                                       handleErrors xs
\end{code}


%************************************************************************
%*									*
\subsection{Running an expression}
%*									*
%************************************************************************

\begin{code}
runMetaE :: TypecheckedHsExpr 	-- Of type (Q Exp)
	 -> TcM TH.Exp	-- Of type Exp
runMetaE e = runMeta e

runMetaD :: TypecheckedHsExpr 	-- Of type Q [Dec]
	 -> TcM [TH.Dec]	-- Of type [Dec]
runMetaD e = runMeta e

runMeta :: TypecheckedHsExpr 	-- Of type X
	-> TcM t		-- Of type t
runMeta expr
  = do	{ hsc_env <- getTopEnv
	; tcg_env <- getGblEnv
	; this_mod <- getModule
	; let type_env = tcg_type_env tcg_env
	      rdr_env  = tcg_rdr_env tcg_env
	-- Wrap the compile-and-run in an exception-catcher
	-- Compiling might fail if linking fails
	-- Running might fail if it throws an exception
	; either_tval <- tryM $ do
		{ 	-- Compile it
		  hval <- ioToTcRn (HscMain.compileExpr 
				      hsc_env this_mod 
			              rdr_env type_env expr)
     	  		-- Coerce it to Q t, and run it
		; TH.runQ (unsafeCoerce# hval) }

	; case either_tval of
	      Left exn -> failWithTc (vcat [text "Exception when trying to run compile-time code:", 
				            nest 4 (vcat [text "Code:" <+> ppr expr,
						      text ("Exn: " ++ Panic.showException exn)])])
	      Right v  -> returnM v }
\end{code}

To call runQ in the Tc monad, we need to make TcM an instance of Quasi:

\begin{code}
instance TH.Quasi (IOEnv (Env TcGblEnv TcLclEnv)) where
  qNewName s = do  { u <- newUnique 
		  ; let i = getKey u
		  ; return (TH.mkNameU s i) }

  qReport True msg  = addErr (text msg)
  qReport False msg = addReport (text msg)

  qCurrentModule = do { m <- getModule; return (moduleUserString m) }
  qReify v = reify v
  qRecover = recoverM

  qRunIO io = ioToTcRn io
\end{code}


%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************

\begin{code}
showSplice :: String -> TypecheckedHsExpr -> SDoc -> TcM ()
showSplice what before after
  = getSrcLocM		`thenM` \ loc ->
    traceSplice (vcat [ppr loc <> colon <+> text "Splicing" <+> text what, 
		       nest 2 (sep [nest 2 (ppr before),
				    text "======>",
				    nest 2 after])])

illegalBracket level
  = ptext SLIT("Illegal bracket at level") <+> ppr level

illegalSplice level
  = ptext SLIT("Illegal splice at level") <+> ppr level

#endif 	/* GHCI */
\end{code}


%************************************************************************
%*									*
			Reification
%*									*
%************************************************************************


\begin{code}
reify :: TH.Name -> TcM TH.Info
reify (TH.Name occ (TH.NameG th_ns mod))
  = do	{ name <- lookupOrig (mkModuleName (TH.modString mod))
			     (OccName.mkOccName ghc_ns (TH.occString occ))
	; thing <- tcLookup name
	; reifyThing thing
    }
  where
    ghc_ns = case th_ns of
		TH.DataName  -> dataName
		TH.TcClsName -> tcClsName
		TH.VarName   -> varName

------------------------------
reifyThing :: TcTyThing -> TcM TH.Info
-- The only reason this is monadic is for error reporting,
-- which in turn is mainly for the case when TH can't express
-- some random GHC extension

reifyThing (AGlobal (AnId id))
  = do	{ ty <- reifyType (idType id)
	; fix <- reifyFixity (idName id)
	; let v = reifyName id
	; case globalIdDetails id of
	    ClassOpId cls    -> return (TH.ClassOpI v ty (reifyName cls) fix)
	    other	     -> return (TH.VarI     v ty Nothing fix)
    }

reifyThing (AGlobal (ATyCon tc))   = do { dec <- reifyTyCon tc;  return (TH.TyConI dec) }
reifyThing (AGlobal (AClass cls))  = do { dec <- reifyClass cls; return (TH.ClassI dec) }
reifyThing (AGlobal (ADataCon dc))
  = do	{ let name = dataConName dc
	; ty <- reifyType (idType (dataConWrapId dc))
	; fix <- reifyFixity name
	; return (TH.DataConI (reifyName name) ty (reifyName (dataConTyCon dc)) fix) }

reifyThing (ATcId id _ _) 
  = do	{ ty1 <- zonkTcType (idType id)	-- Make use of all the info we have, even
					-- though it may be incomplete
	; ty2 <- reifyType ty1
	; fix <- reifyFixity (idName id)
	; return (TH.VarI (reifyName id) ty2 Nothing fix) }

reifyThing (ATyVar tv) 
  = do	{ ty1 <- zonkTcTyVar tv
	; ty2 <- reifyType ty1
	; return (TH.TyVarI (reifyName tv) ty2) }

------------------------------
reifyTyCon :: TyCon -> TcM TH.Dec
reifyTyCon tc
  | isSynTyCon tc
  = do	{ let (tvs, rhs) = getSynTyConDefn tc
	; rhs' <- reifyType rhs
	; return (TH.TySynD (reifyName tc) (reifyTyVars tvs) rhs') }

  | isNewTyCon tc
  = do 	{ cxt <- reifyCxt (tyConTheta tc)
	; con <- reifyDataCon (head (tyConDataCons tc))
	; return (TH.NewtypeD cxt (reifyName tc) (reifyTyVars (tyConTyVars tc))
			      con [{- Don't know about deriving -}]) }

  | otherwise	-- Algebraic
  = do	{ cxt <- reifyCxt (tyConTheta tc)
	; cons <- mapM reifyDataCon (tyConDataCons tc)
	; return (TH.DataD cxt (reifyName tc) (reifyTyVars (tyConTyVars tc))
			      cons [{- Don't know about deriving -}]) }

reifyDataCon :: DataCon -> TcM TH.Con
reifyDataCon dc
  = do 	{ arg_tys <- reifyTypes (dataConOrigArgTys dc)
	; let stricts = map reifyStrict (dataConStrictMarks dc)
	      fields  = dataConFieldLabels dc
	; if null fields then
	     return (TH.NormalC (reifyName dc) (stricts `zip` arg_tys))
	  else
	     return (TH.RecC (reifyName dc) (zip3 (map reifyName fields) stricts arg_tys)) }
	-- NB: we don't remember whether the constructor was declared in an infix way

------------------------------
reifyClass :: Class -> TcM TH.Dec
reifyClass cls 
  = do	{ cxt <- reifyCxt theta
	; ops <- mapM reify_op op_stuff
	; return (TH.ClassD cxt (reifyName cls) (reifyTyVars tvs) ops) }
  where
    (tvs, theta, _, op_stuff) = classBigSig cls
    reify_op (op, _) = do { ty <- reifyType (idType op)
			  ; return (TH.SigD (reifyName op) ty) }

------------------------------
reifyType :: TypeRep.Type -> TcM TH.Type
reifyType (TyVarTy tv)	    = return (TH.VarT (reifyName tv))
reifyType (TyConApp tc tys) = reify_tc_app (reifyName tc) tys
reifyType (NewTcApp tc tys) = reify_tc_app (reifyName tc) tys
reifyType (NoteTy _ ty)     = reifyType ty
reifyType (AppTy t1 t2)     = do { [r1,r2] <- reifyTypes [t1,t2] ; return (r1 `TH.AppT` r2) }
reifyType (FunTy t1 t2)     = do { [r1,r2] <- reifyTypes [t1,t2] ; return (TH.ArrowT `TH.AppT` r1 `TH.AppT` r2) }
reifyType ty@(ForAllTy _ _) = do { cxt' <- reifyCxt cxt; 
				 ; tau' <- reifyType tau 
				 ; return (TH.ForallT (reifyTyVars tvs) cxt' tau') }
			    where
				(tvs, cxt, tau) = tcSplitSigmaTy ty
reifyTypes = mapM reifyType
reifyCxt   = mapM reifyPred

reifyTyVars :: [TyVar] -> [TH.Name]
reifyTyVars = map reifyName

reify_tc_app :: TH.Name -> [TypeRep.Type] -> TcM TH.Type
reify_tc_app tc tys = do { tys' <- reifyTypes tys 
			 ; return (foldl TH.AppT (TH.ConT tc) tys') }

reifyPred :: TypeRep.PredType -> TcM TH.Type
reifyPred (ClassP cls tys) = reify_tc_app (reifyName cls) tys
reifyPred p@(IParam _ _)   = noTH SLIT("implicit parameters") (ppr p)


------------------------------
reifyName :: NamedThing n => n -> TH.Name
reifyName thing
  | isExternalName name = mk_varg mod occ_str
  | otherwise	        = TH.mkNameU occ_str (getKey (getUnique name))
  where
    name    = getName thing
    mod     = moduleUserString (nameModule name)
    occ_str = occNameUserString occ
    occ     = nameOccName name
    mk_varg | OccName.isDataOcc occ = TH.mkNameG_d
	    | OccName.isVarOcc  occ = TH.mkNameG_v
	    | OccName.isTcOcc   occ = TH.mkNameG_tc
	    | otherwise		    = pprPanic "reifyName" (ppr name)

------------------------------
reifyFixity :: Name -> TcM TH.Fixity
reifyFixity name
  = do	{ fix <- lookupFixityRn name
	; return (conv_fix fix) }
    where
      conv_fix (BasicTypes.Fixity i d) = TH.Fixity i (conv_dir d)
      conv_dir BasicTypes.InfixR = TH.InfixR
      conv_dir BasicTypes.InfixL = TH.InfixL
      conv_dir BasicTypes.InfixN = TH.InfixN

reifyStrict :: BasicTypes.StrictnessMark -> TH.Strict
reifyStrict MarkedStrict    = TH.IsStrict
reifyStrict MarkedUnboxed   = TH.IsStrict
reifyStrict NotMarkedStrict = TH.NotStrict

------------------------------
noTH :: LitString -> SDoc -> TcM a
noTH s d = failWithTc (hsep [ptext SLIT("Can't represent") <+> ptext s <+> 
				ptext SLIT("in Template Haskell:"),
		 	     nest 2 d])
\end{code}
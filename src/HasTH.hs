{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module HasTH (mkHasInstances) where
import           Control.Monad
import           Language.Haskell.TH
import           Data.Has

mkHasInstances :: Name -> DecsQ
mkHasInstances tyNm = reify tyNm >>= \case
  TyConI (DataD _cxt tyName bndrs _kind [con] _deriv )
    -> mkInstancesForCon (mkConcreteTy tyName bndrs) con

mkInstancesForCon :: Type -> Con -> DecsQ
mkInstancesForCon par = \case
  NormalC conName args -> conInsts conName args
  RecC conName args -> conInsts conName $ (\(nm, bg, ty) -> (bg, ty)) <$> args
  InfixC x conName y -> conInsts conName [x, y]
  ForallC _ _ con -> mkInstancesForCon par con
  GadtC{} -> fail "GADTs are not supported"
  RecGadtC{} -> fail "GADTs are not supported"
  where
    conInsts conName args =
      let nArgs = length args
      in forM (zip [0..] $ map snd args) $ \(i, tv) ->
        do
          vars <- replicateM nArgs (newName "a")
          let conPat = ConP conName (map VarP vars)
              getBod = VarE (vars !! i)
              get = FunD 'Data.Has.getter [Clause [conPat] (NormalB getBod) []]
          funName <- newName "f"
          let funPat = VarP funName
              varEs = map VarE vars
              modBod = foldl AppE (ConE conName)
                $ take i varEs ++ AppE (VarE funName) getBod : drop (i+1) varEs
              mod = FunD 'Data.Has.modifier [Clause [funPat, conPat] (NormalB modBod) []]
          return $ InstanceD Nothing [] (hasT tv par) [get, mod]

appTys :: [Type] -> Type
appTys = foldl1 AppT

hasT :: Type -> Type -> Type
hasT a e = appTys [ConT ''Data.Has.Has, a, e]

mkConcreteTy :: Name -> [TyVarBndr] -> Type
mkConcreteTy nm bndrs = appTys (ConT nm : map getTV bndrs)

getTV :: TyVarBndr -> Type
getTV (PlainTV name)    = VarT name
getTV (KindedTV name _) = VarT name

{-# LANGUAGE InstanceSigs #-}

module UTerm
    ( UTerm(..)
    , Var
    , Bind
    , USubst
    ) where

import Control.Monad.Logic ( Logic )

import Data.Map as Map ( Map, lookup )

type Var = Int
type Bind = Map.Map String Var
type USubst t = Map.Map Var t 

class UTerm t where
    pretty :: t -> String 
    substID :: t -> Bind -> t
    unify :: t -> t -> USubst t -> Logic (USubst t)
    replace :: USubst t -> t -> t

    getTerm :: USubst t  -> Var -> Maybe t
    getTerm subst v = Map.lookup v subst >>= return . replace subst

    



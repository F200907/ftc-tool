{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Util.PrettyUtil (lor, land, arc, mu, lnot, top, bot, tryToSuperscript, tryToSubscript, implies, forAll, exists, mapsTo) where

import Data.Map.Strict (Map, fromList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Prettyprinter

lor :: Doc ann
lor = "∨"

land :: Doc ann
land = "∧"

lnot :: Doc ann
lnot = "¬"

top :: Doc ann
top = "⊤"

bot :: Doc ann
bot = "⊥"

arc :: Doc ann
arc = "⌢"

mu :: Doc ann
mu = "μ"

implies :: Doc ann
implies = "=⇒"

forAll :: Doc ann
forAll = "∀"

exists :: Doc ann
exists = "∃"

mapsTo :: Doc ann
mapsTo = "↦"

instance (Pretty a, Pretty b) => Pretty (Map a b) where
  pretty :: Map a b -> Doc ann
  pretty m = encloseSep "{" "}" "," $ map (\(k, v) -> pretty k <+> mapsTo <+> pretty v) (Map.toList m)

instance (Pretty a) => Pretty (Set a) where
  pretty :: Set a -> Doc ann
  pretty s = encloseSep "{" "}" "," $ map pretty (Set.toList s)

superscriptTable :: Map Char Char
superscriptTable =
  fromList $
    [('a', '\7491'), ('b', '\7495'), ('c', '\7580'), ('d', '\7496'), ('e', '\7497'), ('f', '\7584'), ('g', '\7501'), ('h', '\688'), ('i', '\8305'), ('j', '\690'), ('k', '\7503'), ('l', '\737'), ('m', '\7504'), ('n', '\8319'), ('o', '\7506'), ('p', '\7510'), ('q', '\67493'), ('r', '\691'), ('s', '\738'), ('t', '\7511'), ('u', '\7512'), ('v', '\7515'), ('w', '\695'), ('x', '\739'), ('y', '\696'), ('z', '\7611')]
      ++ [('A', '\7468'), ('B', '\7470'), ('C', '\42994'), ('D', '\7472'), ('E', '\7473'), ('F', '\42995'), ('G', '\7475'), ('H', '\7476'), ('I', '\7477'), ('J', '\7478'), ('K', '\7479'), ('L', '\7480'), ('M', '\7481'), ('N', '\7482'), ('O', '\7484'), ('P', '\7486'), ('Q', '\42996'), ('R', '\7487')]
      ++ [('T', '\7488'), ('U', '\7489'), ('V', '\11389'), ('W', '\7490')]
      ++ [('Y', '\67506')]
      ++ [('0', '\8304'), ('1', '\185'), ('2', '\178'), ('2', '\179'), ('4', '\8308'), ('5', '\8309'), ('6', '\8310'), ('7', '\8311'), ('8', '\8312'), ('9', '\8313')]
      ++ [('+', '\8314'), ('-', '\8315'), ('=', '\8318'), ('(', '\8317'), (')', '\8318')]

tryToSuperscript :: String -> String
tryToSuperscript = map (\x -> fromMaybe x (Map.lookup x superscriptTable))

subscriptTable :: Map Char Char
subscriptTable =
  fromList $
    [('0', '\8320'), ('1', '\8321'), ('2', '\8322'), ('3', '\8323'), ('4', '\8324'), ('5', '\8325'), ('6', '\8326'), ('7', '\8327'), ('8', '\8328'), ('9', '\8329'), ('+', '\8330'), ('-', '\8331'), ('=', '\8332'), ('(', '\8333'), (')', '\8334')]
      ++ [('a', '\8336'), ('e', '\8337'), ('h', '\8341'), ('i', '\7522'), ('j', '\11388'), ('k', '\8342'), ('l', '\8343'), ('m', '\8344'), ('n', '\8345'), ('o', '\8338'), ('p', '\8346')]
      ++ [('r', '\7523'), ('s', '\8347'), ('t', '\8348'), ('u', '\7524'), ('v', '\7525')]
      ++ [('x', '\8339')]

tryToSubscript :: String -> String
tryToSubscript = map (\x -> fromMaybe x (Map.lookup x subscriptTable))
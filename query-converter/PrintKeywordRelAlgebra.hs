{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
module PrintKeywordRelAlgebra where

-- pretty-printer generated by the BNF converter

import AbsRelAlgebra ---- CHANGE to same Abs
import Data.Char
import Data.List (intersperse)

-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

unwordsD :: [Doc] -> Doc
unwordsD = concatD . intersperse (doc (showChar ' '))

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')

instance Print String where
  prt _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print (Tree c) where
  prt _i e = case e of
    RRels rels -> prPrec _i 0 (concatD [prt 0 rels])
    RTable i -> prPrec _i 3 (concatD [prt 0 i])
    RSelect cond rel -> prPrec _i 2 (concatD [doc (showString "FILTER") , doc (showString "[") , prt 0 cond , doc (showString "]") , prt 3 rel])
    RProject projections rel -> prPrec _i 2 (concatD [doc (showString "PROJECT") , doc (showString "[") , prt 0 projections , doc (showString "]") , prt 3 rel])
    RRename renaming rel -> prPrec _i 2 (concatD [doc (showString "RENAME") , doc (showString "[") , prt 0 renaming , doc (showString "]") , prt 3 rel])
    RGroup is aggregations rel -> prPrec _i 2 (concatD [doc (showString "GROUP") , doc (showString "[") , prt 0 is , doc (showString ",") , prt 0 aggregations , doc (showString "]") , prt 3 rel])
    RSort sortexps rel -> prPrec _i 2 (concatD [doc (showString "ORDER") , doc (showString "[") , prt 0 sortexps , doc (showString "]") , prt 3 rel])
    RDistinct rel -> prPrec _i 2 (concatD [doc (showString "DISTINCT") , prt 3 rel])
    RUnion rel0 rel1 -> prPrec _i 1 (concatD [prt 1 rel0 , doc (showString "UNION") , prt 2 rel1])
    RCartesian rel0 rel1 -> prPrec _i 1 (concatD [prt 1 rel0 , doc (showString ",") , prt 2 rel1])
    RExcept rel0 rel1 -> prPrec _i 1 (concatD [prt 1 rel0 , doc (showString "EXCEPT") , prt 2 rel1])
    RNaturalJoin rel0 rel1 -> prPrec _i 1 (concatD [prt 1 rel0 , doc (showString "NATURAL") , doc (showString "JOIN") , prt 2 rel1])
    RThetaJoin rel0 cond1 rel2 -> prPrec _i 1 (concatD [prt 1 rel0 , doc (showString "JOIN") , doc (showString "ON") , doc (showString "[") , prt 0 cond1 , doc (showString "]") , prt 2 rel2])
    RInnerJoin rel0 is1 rel2 -> prPrec _i 1 (concatD [prt 1 rel0 , doc (showString "INNER") , doc (showString "JOIN") , doc (showString "USING") , doc (showString "[") , prt 0 is1 , doc (showString "]") , prt 2 rel2])
    RFullOuterJoin rel0 is1 rel2 -> prPrec _i 1 (concatD [prt 1 rel0 , doc (showString "FULL") , doc (showString "OUTER") , doc (showString "JOIN") , doc (showString "USING") , doc (showString "[") , prt 0 is1 , doc (showString "]") , prt 2 rel2])
    RLeftOuterJoin rel0 is1 rel2 -> prPrec _i 1 (concatD [prt 1 rel0 , doc (showString "LEFT") , doc (showString "OUTER") , doc (showString "JOIN") , doc (showString "USING") , doc (showString "[") , prt 0 is1 , doc (showString "]") , prt 2 rel2])
    RRightOuterJoin rel0 is1 rel2 -> prPrec _i 1 (concatD [prt 1 rel0 , doc (showString "RIGHT") , doc (showString "OUTER") , doc (showString "JOIN") , doc (showString "USING") , doc (showString "[") , prt 0 is1 , doc (showString "]") , prt 2 rel2])
    RIntersect rel0 rel1 -> prPrec _i 1 (concatD [prt 1 rel0 , doc (showString "INTERSECT") , prt 2 rel1])
    RLet i rel0 rel1 -> prPrec _i 0 (concatD [doc (showString "LET") , prt 0 i , doc (showString "=") , prt 1 rel0 , doc (showString "IN") , prt 0 rel1])
    CEq exp0 exp1 -> prPrec _i 2 (concatD [prt 0 exp0 , doc (showString "=") , prt 0 exp1])
    CNEq exp0 exp1 -> prPrec _i 2 (concatD [prt 0 exp0 , doc (showString "<>") , prt 0 exp1])
    CLt exp0 exp1 -> prPrec _i 2 (concatD [prt 0 exp0 , doc (showString "<") , prt 0 exp1])
    CGt exp0 exp1 -> prPrec _i 2 (concatD [prt 0 exp0 , doc (showString ">") , prt 0 exp1])
    CLeq exp0 exp1 -> prPrec _i 2 (concatD [prt 0 exp0 , doc (showString "<=") , prt 0 exp1])
    CGeq exp0 exp1 -> prPrec _i 2 (concatD [prt 0 exp0 , doc (showString ">=") , prt 0 exp1])
    CLike exp0 exp1 -> prPrec _i 2 (concatD [prt 0 exp0 , doc (showString "LIKE") , prt 0 exp1])
    CNot cond -> prPrec _i 2 (concatD [doc (showString "NOT") , prt 3 cond])
    CAnd cond0 cond1 -> prPrec _i 1 (concatD [prt 1 cond0 , doc (showString "AND") , prt 2 cond1])
    COr cond0 cond1 -> prPrec _i 1 (concatD [prt 1 cond0 , doc (showString "OR") , prt 2 cond1])
    EIdent i -> prPrec _i 3 (concatD [prt 0 i])
    EQIdent i0 i1 -> prPrec _i 3 (concatD [prt 0 i0 , doc (showString ".") , prt 0 i1])
    EString str -> prPrec _i 3 (concatD [prt 0 str])
    EInt n -> prPrec _i 3 (concatD [prt 0 n])
    EFloat d -> prPrec _i 3 (concatD [prt 0 d])
    EAggr function distinct i -> prPrec _i 3 (concatD [prt 0 function , doc (showString "(") , prt 0 distinct , prt 0 i , doc (showString ")")])
    EMul exp0 exp1 -> prPrec _i 2 (concatD [prt 2 exp0 , doc (showString "*") , prt 3 exp1])
    EDiv exp0 exp1 -> prPrec _i 2 (concatD [prt 2 exp0 , doc (showString "/") , prt 3 exp1])
    ERem exp0 exp1 -> prPrec _i 2 (concatD [prt 2 exp0 , doc (showString "%") , prt 3 exp1])
    EAdd exp0 exp1 -> prPrec _i 1 (concatD [prt 1 exp0 , doc (showString "+") , prt 2 exp1])
    ESub exp0 exp1 -> prPrec _i 1 (concatD [prt 1 exp0 , doc (showString "-") , prt 2 exp1])
    PExp exp -> prPrec _i 0 (concatD [prt 0 exp])
    PRename exp i -> prPrec _i 0 (concatD [prt 0 exp , doc (showString "AS") , prt 0 i])
    RRelation i -> prPrec _i 0 (concatD [prt 0 i])
    RAttributes i is -> prPrec _i 0 (concatD [prt 0 i , doc (showString "(") , prt 0 is , doc (showString ")")])
    AApp function distinct i -> prPrec _i 0 (concatD [prt 0 function , doc (showString "(") , prt 0 distinct , prt 0 i , doc (showString ")")])
    ARename function distinct i exp -> prPrec _i 0 (concatD [prt 0 function , doc (showString "(") , prt 0 distinct , prt 0 i , doc (showString ")") , doc (showString "AS") , prt 0 exp])
    FAvg  -> prPrec _i 0 (concatD [doc (showString "AVG")])
    FSum  -> prPrec _i 0 (concatD [doc (showString "SUM")])
    FMax  -> prPrec _i 0 (concatD [doc (showString "MAX")])
    FMin  -> prPrec _i 0 (concatD [doc (showString "MIN")])
    FCount  -> prPrec _i 0 (concatD [doc (showString "COUNT")])
    DNone  -> prPrec _i 0 (concatD [])
    DDistinct  -> prPrec _i 0 (concatD [doc (showString "DISTINCT")])
    SEAsc exp -> prPrec _i 0 (concatD [prt 0 exp])
    SEDesc exp -> prPrec _i 0 (concatD [prt 0 exp , doc (showString "DESC")])
    Ident str -> prPrec _i 0 (doc (showString str))

instance Print [Rel] where
  prt _ es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])
instance Print [Exp] where
  prt _ es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])
instance Print [Projection] where
  prt _ es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])
instance Print [Aggregation] where
  prt _ es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])
instance Print [SortExp] where
  prt _ es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])
instance Print [Ident] where
  prt _ es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

import Data.Char
import System.IO
import Text.Read

data Type  =  Tvar Int | Tfun Type Type                        deriving Eq
data Expr  =  Evar String | Eabs String Expr | Eapp Expr Expr  deriving Eq

-- Pretty printing of expressions

always = True    -- False omits parentheses whenever possible

instance Show Expr where
  showsPrec p (Evar x) = (x ++)
  showsPrec p (Eabs x e) =
    showParen (always || p > 0) ((("\\" ++ x ++ ". ") ++) . showsPrec 0 e)
  showsPrec p (Eapp e1 e2) =
    showParen (always || p > 1) (showsPrec 1 e1 . (" " ++) . showsPrec 2 e2)

-- Parsing of expressions

instance Read Expr where
  readPrec = (do Ident x <- lexP
                 return (Evar x)) <++
             (do Punc "(" <- lexP
                 Punc "\\" <- lexP
                 Ident x <- lexP
                 Symbol "." <- lexP
                 e <- readPrec
                 Punc ")" <- lexP
                 return (Eabs x e)) <++
             (do Punc "(" <- lexP
                 e1 <- readPrec
                 e2 <- readPrec
                 Punc ")" <- lexP
                 return (Eapp e1 e2))

-- Pretty printing of types

instance Show Type where
  showsPrec p (Tvar alpha) = ("@" ++) . showsPrec 0 alpha
  showsPrec p (Tfun sigma tau) =
    showParen (p > 0) (showsPrec 1 sigma . (" -> " ++) . showsPrec 0 tau)

-- Main program

readOne  =  do  s <- getLine
                let e = read s :: Expr
                putStrLn ("Parsed: " ++ show e)

count n m  =  sequence $ take n $ repeat m

trans :: Expr -> (Type, [(Type,Type)])
trans expr =
  let
    sfun _ = -1
    scnt = 0
    sconstr = []

    auxx (Evar x) cnt fun constr =
      let
        res = fun x
      in
        if res == -1 then
          (Tvar cnt, cnt + 1, constr)
        else
          (Tvar res,cnt,constr)

    auxx (Eabs x e) cnt fun constr = 
      let
        nfun = \y-> if (y == x) then cnt else (fun y)
        (t,lcnt,nconstr) = auxx e (cnt+1) nfun constr 
      in
        (Tfun (Tvar cnt) t,lcnt,nconstr)

    auxx (Eapp e1 e2) cnt fun constr =
      let
        (tf,nc1,nconstr1) = auxx e1 cnt fun constr
        (tp,nc2,nconstr2) = auxx e2 nc1 fun nconstr1

        extr (Tfun tf1 tf2) = (tf2,nc2,(tf1,tp):nconstr2)
        extr tft@(Tvar tfv) = 
          let
            ntf1 = Tvar (nc2)
            ntf2 = Tvar (nc2+1)
            nc3 = nc2 + 2
            ntf = Tfun ntf1 ntf2
          in
            (ntf2,nc3,((tft,ntf):(ntf1,tp):nconstr2))
      in
        extr tf

    (t,_,fconstr) = auxx expr scnt sfun sconstr

  in
    (t,fconstr)

inn :: Type -> Type -> Bool
inn t (Tvar a) = t == (Tvar a)
inn t (Tfun lt rt) =
  ((inn t lt) || (inn t rt))

repli :: Type -> Type -> Type -> Type
repli a b (Tvar c) =
  if a == Tvar c then
    b
  else
    Tvar c
repli a b (Tfun t1 t2) =
  if a == Tfun t1 t2 then
    b
  else
    Tfun (repli a b t1) (repli a b t2)

repl :: Type -> Type -> [(Type,Type)] -> [(Type,Type)]
repl a b [] = []
repl a b ((t1,t2):(constr)) = ((repli a b t1,repli a b t2):(repl a b constr))

xtrauni :: Type -> [(Type,Type)] -> (Bool,Type)
xtrauni tt [] = (True,tt)
xtrauni tt ((a@(Tvar _),b@(Tvar _)):constr) =
  if a == b then
    xtrauni tt constr
  else
    xtrauni (repli a b tt) (repl a b constr)
xtrauni tt ((a@(Tvar _), b@(Tfun _ _)):constr) = 
  if inn a b then (False, tt)
  else xtrauni (repli a b tt)  (repl a b constr)
xtrauni tt ((a@(Tfun _ _),b@(Tvar _)):constr) =
  if inn b a then (False, tt)
  else xtrauni (repli b a tt) (repl b a constr)
xtrauni tt ((a@(Tfun a1 a2),b@(Tfun b1 b2)):constr) =
  if a == b then
    xtrauni tt constr
  else
    xtrauni tt ((a1,b1):(a2,b2):constr)

byord lst (Tvar c) =
  if elem c lst then
    lst
  else
    (c:lst)
byord lst (Tfun a b) = 
  let
    lst1 = byord lst a
    lst2 = byord lst1 b
  in
    lst2

totalrepl :: Type -> Int -> [Int] -> Type
totalrepl tt _ [] = tt
totalrepl tt cnt (n:tl) = totalrepl (repli (Tvar n) (Tvar cnt) tt) (cnt+1) tl

dowork :: Expr -> [Char]
dowork expr =
  let
    (rest,resc) = trans expr
    (succ,ft) = xtrauni rest resc
  in
    if (succ) then
      let
        mres = (minuss ft)
      in
        show (totalrepl mres 0 (reverse (byord [] mres)))
    else
      "type error"

minuss :: Type -> Type
minuss (Tvar x) = Tvar (-x)
minuss (Tfun a b) = Tfun (minuss a) (minuss b)

readTwo =  do  s <- getLine
               let e = read s :: Expr
               putStrLn (dowork e)

main     =  do  n <- readLn
                count n readTwo
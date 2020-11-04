namespace StackMachine

  data TyExp =  Natural
              | Boolean

  data Val: TyExp -> Type where
    NatVal : (n: Nat) -> Val Natural
    BoolVal : (b: Bool) -> Val Boolean

  data Expr: TyExp -> Type where
    ValExpr : (v : Val tyExp)  -> Expr tyExp
    AddExpr : (e1 : Expr Natural) -> (e2 : Expr Natural) -> Expr Natural
    IfExpr : (b: Expr Boolean) -> (e1 : Expr tyExp) -> (e2 : Expr tyExp) -> Expr tyExp

-- expressions to values
  eval: Expr tyExp -> Val tyExp
  eval (ValExpr v) = v
  eval (AddExpr e1 e2) = case eval e1 of
                            NatVal n1 => case eval e2 of
                                            NatVal n2 => NatVal (n1+n2)
  eval (IfExpr b e1 e2) = case eval b of
                            BoolVal b => if b then eval e1 else eval e2

-- Expr Tests
  testExpr: Val Natural
--   testExpr = let exp1 = IfExpr (ValExpr (BoolVal False)) (ValExpr (NatVal 2)) (ValExpr (NatVal 3)) in
  testExpr = let exp1 = IfExpr (ValExpr (BoolVal True)) (AddExpr (ValExpr (NatVal 2)) (ValExpr (NatVal 3))) (ValExpr (NatVal 123)) in
    eval exp1


--   data StackType = List TyExp

  data Stack : List TyExp -> Type where
    Empty : Stack []
    (::): (v : Val tyExp) -> (s: Stack list) -> Stack (tyExp::list)

  Eq (Stack tyExpsts) where
    (==) Empty Empty = True
    (==) ((NatVal n1) :: s1) ((NatVal n2) :: s2) = (n1 == n2) && (s1 == s2)
    (==) ((BoolVal b1) :: s1) ((BoolVal b2) :: s2) = (b1 == b2) && (s1 == s2)

  data Code : List TyExp -> List TyExp -> Type where
    SkipCode : Code s s
    PushCode : (v : Val tyExp) -> Code s (tyExp::s)
    AddCode : Code (Natural :: Natural :: s) (Natural :: s)
    IfCode : (c1 : Code s s') -> (c2 : Code s s') -> Code (Boolean :: s) s'
    (++) : (c1 : Code s1 s2) -> (c2 : Code s2 s3) -> Code s1 s3

-- executes stack operations
  exec : Code sIn sOut -> Stack sIn -> Stack sOut
  exec SkipCode s = s
  exec (PushCode v) s = (v :: s)
--   exec AddCode ConsStack x (ConsStack y s) = ?sadfasdf
  exec AddCode ((NatVal x) :: ((NatVal y) :: s)) = ((NatVal (x + y)) :: s)
  exec (IfCode c1 c2) ((BoolVal b) :: s) = if b then exec c1 s else exec c2 s
  exec (c1 ++ c2) s = exec c2 (exec c1 s)


  compile: Expr tyExp -> Code sIn (tyExp::sIn)
  compile (ValExpr v) = (PushCode v)
  compile (AddExpr e1 e2) = (compile e2) ++ (compile e1) ++ AddCode
  compile (IfExpr b e1 e2) = (compile b) ++ (IfCode (compile e1) (compile e2))

  correct: Expr tyExp -> Stack tyExps -> Bool
  correct e s = (eval e) :: s == exec (compile e) s

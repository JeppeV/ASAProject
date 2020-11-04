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
    EmptyStack : Stack []
    ConsStack : (v : Val tyExp) -> (s: Stack list) -> Stack (tyExp::list)

  data Code : List TyExp -> List TyExp -> Type where
    SkipCode : Code s s
    PushCode : (v : Val tyExp) -> Code s (tyExp::s)
    AddCode : Code (Natural :: Natural :: s) (Natural :: s)
    IfCode : (c1 : Code s s') -> (c2 : Code s s') -> Code (Boolean :: s) s'
    SeqCode : (c1 : Code s1 s2) -> (c2 : Code s2 s3) -> Code s1 s3

-- executes stack operations
  exec : Code sIn sOut -> Stack sIn -> Stack sOut
  exec SkipCode s = s
  exec (PushCode v) s = (ConsStack v s)
--   exec AddCode ConsStack x (ConsStack y s) = ?sadfasdf
  exec AddCode x = ?asdf
  exec (IfCode c1 c2) y = ?exec_rhs_4
  exec (SeqCode c1 c2) y = ?exec_rhs_5

--   data Stack: Type wheret
--     Empty : Stack
--     Cons : (ty: TyExp) -> (stack: Stack) -> Stack

--   data Code: Stack -> Stack -> Type where
--     Skip: Code stack stack
--     Pop: Code (Cons val rest) rest
--     Push: (val : TyExp) -> Code stack (Cons val stack)
--     Add: Code (Cons (Natural n1) (Cons (Natural n2) rest)) (Cons (Natural (n1 + n2)) rest)
--     If: (c1: Code s1 s2) -> (c2: Code s1 s2) -> Code (Cons (Boolean _) s1) s2

--     (++) : (c1: Code s1 s2) -> (c2: Code s2 s3) ->
--             Code s1 s3

--   exec: (in_stack: Stack) -> (Code in_stack out_stack) -> Stack
--   exec in_stack Skip = in_stack
--   exec (Cons val rest) Pop = rest
--   exec in_stack (Push val) = (Cons val in_stack)
--   exec (Cons (Natural n1) (Cons (Natural n2) rest)) Add = (Cons (Natural (n1 + n2)) rest)
--   exec (Cons (Boolean True) rest) (If c1 _) = exec rest c1
--   exec (Cons (Boolean False) rest) (If _ c2) = exec rest c2
--   exec in_stack (c1 ++ c2) = exec (exec in_stack c1) c2

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

  eval: Expr tyExp -> Val tyExp    
  eval (ValExpr v) = v
  eval (AddExpr e1 e2) = case eval e1 of 
                            NatVal n1 => case eval e2 of 
                                            NatVal n2 => NatVal (n1+n2)
  eval (IfExpr b e1 e2) = case eval b of
                            BoolVal b => if b then eval e1 else eval e2


--   data Stack: Type where
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

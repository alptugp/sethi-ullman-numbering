> module Ex3FunctionsCodeGenerator where
> import Ex3FunctionsTypes
> import Data.List

-----------------------------------------------------------
Solution for Compilers exercise 3

Paul Kelly  Imperial College London  2009
-----------------------------------------------------------

Fill in the gaps...

Part (1): translate function declaration

> transFunction :: Function -> [Instr]
> transFunction (Defun fname paramname body)
>  = [Define fname] ++ transExp body (allRegs \\ [paramReg]) ++ [Ret] 

Part (2): saving registers

> saveRegs :: [Register] -> [Instr]
> saveRegs regsNotInUse
>  = [Mov (Reg regInUse) Push | regInUse <- allRegs \\ regsNotInUse]


Part (3): translate expression (ie function body, perhaps including
function calls)

> transExp :: Exp -> [Register] -> [Instr]
> transExp (Var v) (dst:rest) = [Mov (Reg paramReg) (Reg dst)]
> transExp (Const x) (dst:rest) = [Mov (ImmNum x) (Reg dst)]
> transExp (Minus e1 e2) regsNotInUse@(dst:nxt:rest)
>   | e1Weight > e2Weight = (transExp e1 regsNotInUse) ++ (transExp e2 (nxt:rest)) ++ [Sub (Reg nxt) (Reg dst)]
>   | otherwise = (transExp e2 (nxt:dst:rest)) ++ (transExp e1 (dst:rest)) ++ [Sub (Reg nxt) (Reg dst)]
>   where
>       e1Weight = weight e1 
>       e2Weight = weight e2
> transExp (Apply fname paramExp) regsNotInUse@(dst:rest) = saveRegs regsNotInUse ++ transExp paramExp regsNotInUse
>       ++ (if paramReg /= dst then [Mov (Reg dst) (Reg paramReg)] else [])
>       ++ [Jsr fname] 
>       ++ (if dst /= resultReg then [Mov (Reg resultReg) (Reg dst)] else []) 
>       ++ restoreRegs regsNotInUse
  
> weight :: Exp -> Int
> weight (Const _) = 1
> weight (Var _) = 1
> weight (Minus e e') = min cost cost' 
>   where
>       cost = max (weight e) (1 + weight e')
>       cost' = max (1 + weight e) (weight e')
> weight (Apply _ paramExp) = 1 + weight paramExp

> restoreRegs :: [Register] -> [Instr]
> restoreRegs regsNotInUse
>  = [Mov Pop (Reg regInUse) | regInUse <- reverse (allRegs \\ regsNotInUse)]


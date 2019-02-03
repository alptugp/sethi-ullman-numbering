> module Ex3FunctionsCodeGenerator where
> import Ex3FunctionsTypes

-----------------------------------------------------------
Solution for Compilers exercise 3

Paul Kelly  Imperial College London  2009
-----------------------------------------------------------

Fill in the gaps...

Part (1): translate function declaration

> transFunction (Defun fname paramname body)
>  = [Define fname]

Part (2): saving registers

> saveRegs regsNotInUse
>  = []


Part (3): translate expression (ie function body, perhaps including
function calls)

> transExp :: Exp -> [Register] -> [Instr]
> 
> transExp (Const x) (dst:rest) = [Mov (ImmNum x)(Reg dst)]

> weight (Const x) = 1

> restoreRegs regsNotInUse
>  = []


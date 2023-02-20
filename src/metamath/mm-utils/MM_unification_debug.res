open MM_context

type unifErr = 
    | Err
    | DisjCommonVar({frmVar1:int, expr1:expr, frmVar2:int, expr2:expr, commonVar:int})
    | Disj({frmVar1:int, expr1:expr, var1:int, frmVar2:int, expr2:expr, var2:int})
    | UnprovedFloating({expr:expr})
open MM_context

type unifErr =
  | UnifErr
  | DisjCommonVar({ frmVar1: int, expr1: MM_context.expr, frmVar2: int, expr2: MM_context.expr, commonVar: int, })
  | Disj({ frmVar1: int, expr1: MM_context.expr, var1: int, frmVar2: int, expr2: MM_context.expr, var2: int, })
  | UnprovedFloating({expr: MM_context.expr})
  | NoUnifForAsrt({asrtExpr: MM_context.expr, expr: MM_context.expr})
  | NoUnifForArg({args: array<MM_context.expr>, errArgIdx: int})
  | NewVarsAreDisabled({args: array<MM_context.expr>, errArgIdx: int})
  | TooManyCombinations({frmLabels:option<array<string>>})


let unifErrToStr: (
    unifErr,
    ~exprToStr: expr=>string,
    ~frmExprToStr: expr=>string,
) => string
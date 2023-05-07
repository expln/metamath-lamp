open MM_context

type unifErr = 
    | UnifErr
    | DisjCommonVar({frmVar1:int, expr1:expr, frmVar2:int, expr2:expr, commonVar:int})
    | Disj({frmVar1:int, expr1:expr, var1:int, frmVar2:int, expr2:expr, var2:int})
    | UnprovedFloating({expr:expr})
    | NoUnifForAsrt({asrtExpr:expr, expr:expr})
    | NoUnifForArg({args:array<expr>, errArgIdx:int})
    | NewVarsAreDisabled({args:array<expr>, errArgIdx:int})

let argsToString = (args:array<expr>, exprToStr:expr=>string):string => {
    args->Js_array2.mapi((arg,i) => {
        `${(i+1)->Belt.Int.toString}: ${if (arg->Js_array2.length == 0) { "?" } else { exprToStr(arg) } }`
    })->Js.Array2.joinWith("\n")
}

let unifErrToStr = (
    err:unifErr,
    ~exprToStr: expr=>string,
    ~frmExprToStr: expr=>string,
) => {

    switch err {
        | UnifErr => "Details of the error were not stored."
        | DisjCommonVar({frmVar1, expr1, frmVar2, expr2, commonVar}) => {
            let arrow = Js_string2.fromCharCode(8594)
            `Unsatisfied disjoint, common variable ${exprToStr([commonVar])}:\n`
                ++ `${frmExprToStr([frmVar1])} ${arrow} ${exprToStr(expr1)}\n`
                ++ `${frmExprToStr([frmVar2])} ${arrow} ${exprToStr(expr2)}`
        }
        | Disj({frmVar1, expr1, var1, frmVar2, expr2, var2}) => {
            let arrow = Js_string2.fromCharCode(8594)
            `Missing disjoint ${exprToStr([var1])},${exprToStr([var2])}:\n`
                ++ `${frmExprToStr([frmVar1])} ${arrow} ${exprToStr(expr1)}\n`
                ++ `${frmExprToStr([frmVar2])} ${arrow} ${exprToStr(expr2)}`
        }
        | UnprovedFloating({expr:expr}) => `Could not prove this floating statement:\n` ++ exprToStr(expr)
        | NoUnifForAsrt({asrtExpr, expr}) => {
            let arrow = Js_string2.fromCharCode(8594)
            `Could not find a unification for assertion:\n`
                ++ `${frmExprToStr(asrtExpr)}\n${arrow}\n${exprToStr(expr)}`
        }
        | NoUnifForArg({args,errArgIdx}) => {
            let colon = if (args->Js.Array2.length == 0) {""} else {":"}
            `Could not unify essential hypothesis #${(errArgIdx+1)->Belt.Int.toString}${colon}\n`
                ++ argsToString(args, exprToStr)
        }
        | NewVarsAreDisabled({args,errArgIdx}) => {
            let what = if (args->Js.Array2.length == errArgIdx) {
                "assertion"
            } else {
                `essential hypothesis #${(errArgIdx+1)->Belt.Int.toString}`
            }
            let colon = if (args->Js.Array2.length == 0) {""} else {":"}
            `New variables are not allowed, but one had to be created`
                ++ ` when unifying ${what}${colon}\n`
                ++ argsToString(args, exprToStr)
        }
    }
}
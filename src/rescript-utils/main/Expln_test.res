@val external describePriv: (string, (. unit)=>unit) => unit = "describe"
@val external describe_skipPriv: (string, (. unit)=>unit) => unit = "describe.skip"
@val external itPriv: (string, (. unit)=>unit) => unit = "it"
@val external it_skipPriv: (string, (. unit)=>unit) => unit = "it.skip"

let describe: (string, unit=>unit) => unit = (name, test) => {
    describePriv(name, (.) => {
        test()
    })->ignore
}

let describe_skip: (string, unit=>unit) => unit = (name, test) => {
    describe_skipPriv(name, (.) => {
        test()
    })->ignore
}

let it: (string, unit=>unit) => unit = (name, test) => {
    itPriv(name, (.) => {
        try {
            test()
        } catch {
            | exn => {
                Js.Console.log("##############################################################")
                Js.Console.log2("error in test", exn)
                Js.Console.log("##############################################################")
                raise(exn)
            }
        }
    })->ignore
}

let it_skip: (string, unit=>unit) => unit = (name, test) => {
    it_skipPriv(name, (.) => {
        test()
    })->ignore
}

let {exn, stringify} = module(Expln_utils_common)

let assertEq = (actual:'a, expected:'a) => {
    if (expected != actual) {
        exn(`\n  actual: ${stringify(actual)}\nexpected: ${stringify(expected)}`)
    }
}

let assertEqMsg = (actual:'a, expected:'a, msg:string) => {
    if (expected != actual) {
        exn(`\nAssertion failed for '${msg}'\n  actual: ${stringify(actual)}\nexpected: ${stringify(expected)}`)
    }
}

let assertEqNum = (actual: float, expected: float, precision: float) => {
    if (actual <= expected -. precision || actual >= expected +. precision) {
        exn(`\n  actual: ${Js.String.make(actual)}\nexpected: ${Js.String.make(expected)}`)
    }
}

let assertEqNumMsg = (actual: float, expected: float, precision: float, msg:string) => {
    if (actual <= expected -. precision || actual >= expected +. precision) {
        exn(`\nAssertion failed for '${msg}'\n  actual: ${Js.String.make(actual)}\nexpected: ${Js.String.make(expected)}`)
    }
}


let fail = () => exn("Test failed.")
let failMsg = str => exn("Test failed: " ++ str)

let startProfile: unit => unit = %raw(`() => console.profile()`)
let stopProfile: unit => unit = %raw(`() => console.profileEnd()`)

let startTimer: string => unit = %raw(`label => console.time(label)`)
let stopTimer: string => unit = %raw(`label => console.timeEnd(label)`)
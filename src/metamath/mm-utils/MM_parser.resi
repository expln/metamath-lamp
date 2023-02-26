type parserProofTableRec = {
    id:string,
    args:array<string>,
    label:string
}

type proof =
    | Uncompressed({labels:array<string>})
    | Compressed({labels:array<string>, compressedProofBlock:string})

type rec mmAstNode = {
    begin: int,
    end: int,
    stmt: stmt
}
and stmt =
    | Comment({text:string})
    | Const({symbols:array<string>})
    | Block({level:int, statements:array<mmAstNode>})
    | Var({symbols:array<string>})
    | Disj({vars:array<string>})
    | Floating({label:string, expr:array<string>})
    | Essential({label:string, expr:array<string>})
    | Axiom({label:string, expr:array<string>})
    | Provable({label:string, expr:array<string>, proof:proof})

type mmException = {
    msg:string,
    begin?:int,
}
exception MmException(mmException)

let parseMmFile: (string, ~onProgress:float => unit=?, ()) => (mmAstNode,array<string>)
let textAt: (string,int) => string
let traverseAst: ('c, mmAstNode,
    ~preProcess:('c, mmAstNode)=>option<'res>=?, ~process:('c, mmAstNode)=>option<'res>=?, ~postProcess:('c, mmAstNode)=>option<'res>=?, ()) => ('c, option<'res>)
let stmtToStr: mmAstNode => string
let stmtToStrRec: mmAstNode => array<string>
let getSpaceSeparatedValuesAsArray: string => array<string>
let astToStr: (
    mmAstNode,
    ~skipComments:bool=?,
    ~skipProofs:bool=?,
    ()
) => string

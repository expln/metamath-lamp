open MM_context
open MM_proof_table

type rec syntaxTreeNode = {
    id: int,
    parent:option<syntaxTreeNode>,
    label:string,
    children:array<childNode>,
}
and childNode =
    | Subtree(syntaxTreeNode)
    | Symbol({id:int, sym:string})

let buildSyntaxTree: (mmContext, proofTable, int) => result<syntaxTreeNode,string>
let syntaxTreeToSymbols: syntaxTreeNode => array<string>
let syntaxTreeIsEmpty: syntaxTreeNode => bool
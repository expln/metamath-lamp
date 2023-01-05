open MM_context
open MM_proof_table

type rec syntaxTreeNode = {
    id: string,
    parent:option<syntaxTreeNode>,
    label:string,
    children:array<childNode>,
}
and childNode =
    | Subtree(syntaxTreeNode)
    | Symbol({id:string, sym:string})

let buildSyntaxTree: (mmContext, proofTable, int) => syntaxTreeNode
let syntaxTreeToSymbols: syntaxTreeNode => array<string>
let syntaxTreeIsEmpty: syntaxTreeNode => bool
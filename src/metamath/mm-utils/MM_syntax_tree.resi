open MM_context
open MM_proof_table

type rec syntaxTreeNode = {
    id: int,
    parent:option<syntaxTreeNode>,
    label:string,
    children:array<childNode>,
    height:int,
    proofTable:proofTable,
    proofTableRow:int
}
and childNode =
    | Subtree(syntaxTreeNode)
    | Symbol({id:int, parent:syntaxTreeNode, sym:string, color:option<string>, isVar:bool})

let buildSyntaxTree: (mmContext, proofTable, int) => result<syntaxTreeNode,string>

let buildSyntaxTreeFromProofTreeDto: (
    ~ctx:mmContext,
    ~proofTreeDto:MM_proof_tree_dto.proofTreeDto,
    ~typeStmt:expr,
) => result<syntaxTreeNode,string>

let buildSyntaxTreeFromProofTree: (
    ~ctx:mmContext,
    ~proofTree:MM_proof_tree.proofTree,
    ~typeStmt:expr,
) => result<syntaxTreeNode,string>

let syntaxTreeToSymbols: syntaxTreeNode => array<string>
let syntaxTreeIsEmpty: syntaxTreeNode => bool

let getNodeById: ( syntaxTreeNode, int, ) => option<childNode>
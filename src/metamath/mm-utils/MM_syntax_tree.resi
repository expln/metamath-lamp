open MM_context
open MM_proof_table

type rec syntaxTreeNode = {
    id: int,
    typ:int,
    label:string,
    children:array<childNode>,
    height:int,
}
and childNode =
    | Subtree(syntaxTreeNode)
    | Symbol({id:int, symInt:int, sym:string, color:option<string>, isVar:bool})

let buildSyntaxTree: (mmContext, proofTable, int) => result<syntaxTreeNode,string>

let buildSyntaxProofTableFromProofTreeDto: (
    ~ctx:mmContext,
    ~proofTreeDto:MM_proof_tree_dto.proofTreeDto,
    ~typeStmt:expr,
) => result<proofTable,string>

let buildSyntaxTreeFromProofTreeDto: (
    ~ctx:mmContext,
    ~proofTreeDto:MM_proof_tree_dto.proofTreeDto,
    ~typeStmt:expr,
) => result<syntaxTreeNode,string>

let syntaxTreeToSymbols: syntaxTreeNode => array<string>
let syntaxTreeIsEmpty: syntaxTreeNode => bool

let getNodeById: ( syntaxTreeNode, int, ) => option<childNode>

let syntaxTreeForEachNode: (childNode, childNode=>option<'r>) => option<'r>
let syntaxTreeGetIdsOfAllChildSymbols: childNode => Belt_SetInt.t
let syntaxTreeToText: childNode => string
let syntaxTreeGetNumberOfSymbols: childNode => int
let syntaxTreeGetParent: (syntaxTreeNode, int) => option<syntaxTreeNode>

type unifSubs = Belt_HashMapString.t<array<string>>

let isVar: (syntaxTreeNode, string=>bool) => option<(int,string)>

let unify: (
    syntaxTreeNode, 
    syntaxTreeNode, 
    ~isMetavar:string=>bool, 
    ~foundSubs:unifSubs, 
    ~continue:ref<bool>
) => unit

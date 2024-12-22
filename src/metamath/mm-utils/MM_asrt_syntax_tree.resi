type rec asrtSyntaxTreeNode = {
    typ: int, 
    children: array<asrtChildNode>
}
and asrtChildNode = 
 | Subtree(asrtSyntaxTreeNode) 
 | Symbol(int)

type sym = 
    | Const(int) 
    | CtxVar(int) 
    | AsrtVar(int)

type unifSubs

let symEq: (sym, sym) => bool
let arrSymEq: (array<sym>, array<sym>) => bool
let buildAsrtSyntaxTree: (MM_proof_tree.proofNode, int => int) => result<asrtSyntaxTreeNode, string>
let buildSyntaxTree: (
    ~proofNode:MM_proof_tree.proofNode,
    ~ctxIntToAsrtInt:int=>int,
    ~asrtIntToSym:int=>string,
    ~asrtVarToHypLabel:int=>string,
) => result<MM_syntax_tree.syntaxTreeNode,string>
let unifSubsMake: () => unifSubs
let unifSubsGet: (unifSubs,sym) => option<array<sym>>
let unifSubsSize: unifSubs => int
let unify: (
  ~asrtExpr: asrtSyntaxTreeNode,
  ~ctxExpr: MM_syntax_tree.syntaxTreeNode,
  ~isMetavar: string => bool,
  ~foundSubs: unifSubs,
  ~continue: ref<bool>,
) => unit
let unifyMayBePossible: ( 
    ~asrtExpr:asrtSyntaxTreeNode, 
    ~ctxExpr:MM_syntax_tree.syntaxTreeNode, 
    ~isMetavar:string=>bool, 
) => bool
let buildSyntaxTree: (
    ~proofNode:MM_proof_tree.proofNode,
    ~ctxIntToAsrtInt:int=>int,
    ~asrtIntToSym:int=>string,
    ~ctxHypLabelAndAsrtVarToAsrtHypLabel:(string,int)=>option<string>,
) => result<MM_syntax_tree.syntaxTreeNode,string>

type sym = 
    | Const(int)
    | CtxVar(int)
    | AsrtVar(int)

let symEq: (sym, sym) => bool
let arrSymEq: (array<sym>, array<sym>) => bool
let symToInt: sym => int

type unifSubs

let unifSubsMake: () => unifSubs
let unifSubsGet: (unifSubs,sym) => option<array<sym>>
let unifSubsSize: unifSubs => int
let unifSubsReset: unifSubs => unit

let unify: ( 
    ~asrtDisj:Belt_MapInt.t<Belt_SetInt.t>,
    ~ctxDisj:MM_context.disjMutable,
    ~asrtExpr:MM_syntax_tree.syntaxTreeNode,
    ~ctxExpr:MM_syntax_tree.syntaxTreeNode,
    ~isMetavar:string=>bool,
    ~foundSubs:unifSubs,
) => bool

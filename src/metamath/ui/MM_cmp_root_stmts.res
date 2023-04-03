open Expln_React_common
open Expln_React_Mui
open MM_wrk_editor
open MM_context
open MM_cmp_user_stmt

type rootStmtRendered = {
    id: string,
    expr:expr,
    isHyp:bool,
    label:string,
    proofStatus: option<proofStatus>,
    exprReElem:reElem,
}

type state = {
    flags: array<bool>,
}

let getProofStatus = (stmt:rootStmtRendered):option<proofStatus> => {
    if (stmt.isHyp) {
        Some(Ready)
    } else {
        stmt.proofStatus
    }
}

let makeInitialState = ( ~flags: array<bool>, ):state => {
    {
        flags:flags
    }
}

let toggleFlag = (idx,flags) => flags->Js_array2.mapi((v,i) => if (i == idx) {!v} else {v})
let selectAllFlag = flags => flags->Js_array2.map(_ => true)
let unselectAllFlags = flags => flags->Js_array2.map(_ => false)
let invertFlags = flags => flags->Js_array2.map(v => !v)

let selectProvedStmts = (rootStmtsRendered,flags:array<bool>):array<bool> => {
    flags->Js_array2.mapi((_,i) => {
        getProofStatus(rootStmtsRendered[i])
            ->Belt_Option.map(status => status == Ready)
            ->Belt.Option.getWithDefault(false)
    })
}

let updateFlags = (st:state, update:array<bool>=>array<bool>):state => { ...st, flags: update(st.flags)}

@react.component
let make = (
    ~title:string,
    ~rootStmtsRendered: array<rootStmtRendered>,
    ~proofStatusesAreAvailable:bool,
    ~flags: array<bool>,
    ~onClose:array<bool>=>unit,
) => {
    let (state, setState) = React.useState(() => makeInitialState( ~flags ))

    let actSelectAllStmts = () => {
        setState(updateFlags(_, selectAllFlag))
    }
    
    let actUnselectAllStmts = () => {
        setState(updateFlags(_, unselectAllFlags ))
    }
    
    let actInvertStmts = () => {
        setState(updateFlags(_, invertFlags))
    }
    
    let actToggleStmt = (idx) => {
        setState(updateFlags(_, toggleFlag(idx)))
    }
    
    let actSelectProvedStmts = () => {
        setState(updateFlags(_, selectProvedStmts(rootStmtsRendered)))
    }

    let rndButtons = () => {
        <Row>
            <Button onClick={_=>onClose(state.flags)} variant=#outlined>
                {React.string("Done")}
            </Button>
            <Button onClick={_=>actUnselectAllStmts()} >
                {React.string("None")}
            </Button>
            <Button onClick={_=>actSelectAllStmts()} >
                {React.string("All")}
            </Button>
            <Button onClick={_=>actInvertStmts()} >
                {React.string("Inverse")}
            </Button>
            {
                if (proofStatusesAreAvailable) {
                    <Button onClick={_=>actSelectProvedStmts()} >
                        {React.string("Proved")}
                    </Button>
                } else {
                    React.null
                }
            }
        </Row>
    }

    let rndStmts = () => {
        let paddingTop="10px"
        <table>
            <tbody>
                {
                    state.flags->Js_array2.mapi((flag,i) => {
                        let stmt = rootStmtsRendered[i]
                        <tr key={i->Belt_Int.toString} style=ReactDOM.Style.make(~verticalAlign="top", ())>
                            <td>
                                <Checkbox
                                    checked=flag
                                    onChange={_ => actToggleStmt(i)}
                                />
                            </td>
                            <td style=ReactDOM.Style.make(~paddingTop, ())>
                                { 
                                    if (proofStatusesAreAvailable) {
                                        rndProofStatus(~proofStatus=getProofStatus(stmt), ())
                                    } else {
                                        React.null
                                    }
                                }
                            </td>
                            <td style=ReactDOM.Style.make(~paddingTop, ())>
                                {React.string(stmt.label ++ ": ")} 
                            </td>
                            <td style=ReactDOM.Style.make(~paddingTop, ())>
                                {stmt.exprReElem} 
                            </td>
                        </tr>
                    })->React.array
                }
            </tbody>
        </table>
    }

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1. >
            <span style=ReactDOM.Style.make(~fontWeight="bold", ())>
                {React.string(title)}
            </span>
            {rndButtons()}
            {rndStmts()}
            {rndButtons()}
        </Col>
    </Paper>
}
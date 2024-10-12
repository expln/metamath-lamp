open Expln_React_common
open Expln_React_Mui
open MM_react_common

@react.component
let make = (
    ~initAsrtsPerPage:string,
    ~onOk:string=>unit, 
    ~onCancel:unit=>unit,
) => {
    let (asrtsPerPage, setAsrtsPerPage) = React.useState(() => initAsrtsPerPage)

    let actOk = () => {
        onOk(asrtsPerPage)
    }

    let actCancel = () => {
        onCancel()
    }

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.>
            <TextField
                label="Assertions per page"
                size=#small
                style=ReactDOM.Style.make(~width="150px", ())
                autoFocus=true
                value=asrtsPerPage
                onChange=evt2str(str => setAsrtsPerPage(_ => str))
                onKeyDown=kbrdHnd2(
                    kbrdClbkMake(~key=keyEnter, ~act=actOk),
                    kbrdClbkMake(~key=keyEsc, ~act=actCancel),
                )
            />
            <Row alignItems=#center>
                <Button onClick=(_=>actOk()) variant=#contained > 
                    { React.string("Ok") }
                </Button>
                <Button onClick={_=>actCancel()} > {React.string("Cancel")} </Button>
            </Row>
        </Col>
    </Paper>
}
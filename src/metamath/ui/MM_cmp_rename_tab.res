open Expln_React_common
open Expln_React_Mui
open MM_react_common

@react.component
let make = (
    ~initName:string,
    ~onOk:string=>unit,
    ~onCancel:unit=>unit,
) => {
    let (newName, setNewName) = React.useState(() => initName)

    let actOk = () => {
        onOk(newName)
    }

    let actCancel = () => {
        onCancel()
    }

    let rndTextField = () => {
        <TextField
            label="Tab title"
            size=#small
            style=ReactDOM.Style.make(~width="500px", ())
            autoFocus=true
            value=newName
            onChange=evt2str(str => setNewName(_ => str))
            onKeyDown=kbrdHnd2(
                kbrdClbkMake(~key=keyEnter, ~act=actOk),
                kbrdClbkMake(~key=keyEsc, ~act=actCancel),
            )
        />
    }

    let rndButtons = () => {
        <Row alignItems=#center style=ReactDOM.Style.make(~padding="4px", ())>
            <Button onClick={_=>actOk()} variant=#contained > {React.string("Save")} </Button>
            <Button onClick={_=>actCancel()} > {React.string("Cancel")} </Button>
        </Row>
    }

    <Col spacing=1. /* style=ReactDOM.Style.make(~padding="10px", ()) */>
        {rndTextField()}
        {rndButtons()}
    </Col>
}
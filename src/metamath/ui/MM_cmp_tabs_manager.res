open Expln_React_common
open Expln_React_Mui
open Expln_React_Modal
open MM_react_common

module RenameTabCmp = {
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
}

type tabProps = {
    id: Expln_React_UseTabs.tabId,
    label: string,
}

@react.component
let make = (
    ~modalRef:modalRef, 
    ~tabs: array<tabProps>,
    ~onTabRename:(Expln_React_UseTabs.tabId, string)=>unit,
    ~onTabMoveUp:Expln_React_UseTabs.tabId=>unit,
    ~onTabMoveDown:Expln_React_UseTabs.tabId=>unit,
    ~onTabFocus:Expln_React_UseTabs.tabId=>unit,
    ~onTabClose:Expln_React_UseTabs.tabId=>unit,
    ~onOpenExplorer:unit=>unit,
    ~onOpenEditor:unit=>unit,
) => {
    let actRenameTab = (tab:tabProps) => {
        openModalPaneWithTitle(
            ~modalRef,
            ~title="Rename tab",
            ~content = (~close) => {
                <RenameTabCmp 
                    initName=tab.label
                    onOk={newName => {
                        close()
                        onTabRename(tab.id,newName)
                    }}
                    onCancel=close
                />
            }
        )
    }

    let rndOpenButtons = () => {
        <Row alignItems={#baseline} style=ReactDOM.Style.make(~marginBottom="20px", ())>
            {"Open new tab: "->React.string}
            <Button onClick={_=>onOpenEditor()} variant={#outlined}> {React.string("Editor")} </Button>
            <Button onClick={_=>onOpenExplorer()} variant={#outlined}> {React.string("Explorer")} </Button>
        </Row>
    }

    let rndTabButtons = (tab:tabProps) => {
        <ButtonGroup variant=#outlined size=#small >
            <Button title="Close" onClick={_=>onTabClose(tab.id)}> <MM_Icons.DeleteForever/> </Button>
            <Button title="Go to this tab" onClick={_=>onTabFocus(tab.id)}> <MM_Icons.OpenInBrowser/> </Button>
            <Button title="Rename" onClick={_=>actRenameTab(tab)}> <MM_Icons.Edit/> </Button>
            <Button title="Move down" onClick={_=>onTabMoveDown(tab.id)}> <MM_Icons.ArrowDownward/> </Button>
            <Button title="Move up" onClick={_=>onTabMoveUp(tab.id)}> <MM_Icons.ArrowUpward/> </Button>
        </ButtonGroup>
    }

    let rndTabControls = (tab:tabProps) => {
        <Row key=tab.id>
            {rndTabButtons(tab)}
            <Paper style=ReactDOM.Style.make(~padding="5px", ())>
                {tab.label->React.string}
            </Paper>
        </Row>
    }
    <Col spacing=1. style=ReactDOM.Style.make(~margin="10px", ())>
        {rndOpenButtons()}
        {tabs->Array.map(rndTabControls)->React.array}
    </Col>
}
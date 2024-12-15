open Expln_React_Mui
open Expln_React_Modal
open MM_react_common

type tabProps = {
    id: Expln_React_UseTabs.tabId,
    icon:string,
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
                <MM_cmp_rename_tab 
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
                {(tab.icon ++ " " ++ tab.label)->React.string}
            </Paper>
        </Row>
    }
    <Col spacing=1. style=ReactDOM.Style.make(~margin="10px", ())>
        {rndOpenButtons()}
        {tabs->Array.map(rndTabControls)->React.array}
    </Col>
}
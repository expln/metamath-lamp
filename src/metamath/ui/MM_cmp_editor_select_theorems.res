open Expln_React_Mui
open Expln_React_Modal
open MM_react_common

@react.component
let make = (
    ~modalRef:modalRef, 
    ~labels:array<string>, 
    ~onOk:array<string>=>unit,
    ~onCancel:unit=>unit,
) => {
    let (selectedLabels, setSelectedLabels) = React.useState(() => Belt_SetString.empty)

    let actToggleLabelSelected = (label:string) => {
        setSelectedLabels(selectedLabels => {
            if (selectedLabels->Belt_SetString.has(label)) {
                selectedLabels->Belt_SetString.remove(label)
            } else {
                selectedLabels->Belt_SetString.add(label)
            }
        })
    }

    let actOk = async () => {
        let continue = await openOkCancelDialog(
            ~modalRef, 
            ~title="Confirm inlining theorems",
            ~text=labels->Array.filter(label => selectedLabels->Belt_SetString.has(label))->Array.join(", "), 
        )
        if (continue) {
            onOk(selectedLabels->Belt_SetString.toArray)
        }
    }

    let rndCheckboxes = () => {
        <Col>
            {
                labels->Array.map(label => {
                    <FormControlLabel
                        key=label
                        control={
                            <Checkbox
                                checked={selectedLabels->Belt_SetString.has(label)}
                                onChange={_=>actToggleLabelSelected(label)}
                            />
                        }
                        label
                    />
                })->React.array
            }
        </Col>
    }

    <Col style=ReactDOM.Style.make(~padding="10px", ())>
        <span style=ReactDOM.Style.make(~fontWeight="bold", ~fontSize="1.1em", ())>
            { React.string("Select theorems to inline:") }
        </span>
        {rndCheckboxes()}
        <Row>
            <Button 
                onClick={_=>actOk()->Promise.done} 
                variant=#contained
                disabled={selectedLabels->Belt_SetString.size==0}
            > 
                {React.string("Ok")} 
            </Button>
            <Button onClick={_=>onCancel()}> {React.string("Cancel")} </Button>
        </Row>
    </Col>
}
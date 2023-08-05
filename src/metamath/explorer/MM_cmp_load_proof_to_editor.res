open Expln_React_common
open Expln_React_Mui
open Local_storage_utils

@react.component
let make = (
    ~onOk:(~adjustContext:bool,~loadSteps:bool)=>unit, 
    ~onCancel:unit=>unit,
) => {
    let (adjustContext, setAdjustContext) = useStateFromLocalStorageBool(
        ~key="load-proof-to-editor-adjustContext", ~default=true
    )
    let (loadSteps, setLoadSteps) = useStateFromLocalStorageBool(
        ~key="load-proof-to-editor-loadSteps", ~default=true
    )

    <Paper style=ReactDOM.Style.make( ~padding="10px", () ) >
        <Col>
            <FormControlLabel
                control={
                    <Checkbox
                        checked=adjustContext
                        onChange={evt2bool(b => setAdjustContext(_ => b))}
                    />
                }
                label="Adjust the context"
            />
            <FormControlLabel
                control={
                    <Checkbox
                        checked=loadSteps
                        onChange={evt2bool(b => setLoadSteps(_ => b))}
                    />
                }
                label="Load intermediate steps"
            />
            <Row alignItems=#center>
                <Button onClick={_=>onOk(~adjustContext, ~loadSteps)} variant=#contained > 
                    { React.string("Load") }
                </Button>
                <Button onClick={_=>onCancel()} > {React.string("Cancel")} </Button>
            </Row>
        </Col>
    </Paper>
}
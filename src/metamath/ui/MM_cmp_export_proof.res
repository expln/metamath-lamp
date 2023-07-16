open Expln_React_common
open Expln_React_Mui
open Expln_utils_promise
open MM_react_common
open Local_storage_utils
open Common

@react.component
let make = (
    ~proofText:string, 
    ~proofTableWithTypes:string, 
    ~proofTableWithoutTypes:string,
    ~onClose:unit=>unit,
) => {
    let (showProofTable, setShowProofTable) = useStateFromLocalStorageBool(
        ~key="export-proof-show-proof-table", ~default=false
    )
    let (essentialsOnly, setEssentialsOnly) = useStateFromLocalStorageBool(
        ~key="export-proof-essentials-only", ~default=false
    )
    let (copiedToClipboard, setCopiedToClipboard) = React.useState(() => None)

    let getTextToCopy = () => {
        proofText 
            ++ if (!showProofTable) {
                ""
            } else {
                if (essentialsOnly) {
                    "\n\n" ++ proofTableWithoutTypes
                } else {
                    "\n\n" ++ proofTableWithTypes
                }
            }
    }

    let actCopyToClipboard = () => {
        copyToClipboard(getTextToCopy())->promiseMap(_ => {
            setCopiedToClipboard(timerId => {
                switch timerId {
                    | None => ()
                    | Some(timerId) => clearTimeout(timerId)
                }
                Some(setTimeout(
                    () => setCopiedToClipboard(_ => None),
                    1000
                ))
            })
        })->ignore
    }

    let rndProofTable = () => {
        if (!showProofTable) {
            React.null
        } else {
            if (essentialsOnly) {
                <pre>
                    {React.string(proofTableWithoutTypes)}
                </pre>
            } else {
                <pre>
                    {React.string(proofTableWithTypes)}
                </pre>
            }
        }
    }

    <Paper style=ReactDOM.Style.make( ~padding="10px", () ) >
        <Col spacing=1.>
            <Row alignItems=#center>
                <FormControlLabel
                    control={
                        <Checkbox
                            checked=showProofTable
                            onChange={evt2bool(b => setShowProofTable(_ => b))}
                        />
                    }
                    label="proof table"
                />
                <FormControlLabel
                    control={
                        <Checkbox
                            checked=essentialsOnly
                            onChange={evt2bool(b => setEssentialsOnly(_ => b))}
                        />
                    }
                    label="essentials only"
                    disabled={!showProofTable}
                />
                <Button onClick={_=>actCopyToClipboard()} variant=#contained style=ReactDOM.Style.make(~width="90px", ()) > 
                    {
                        if (copiedToClipboard->Belt.Option.isSome) {
                            React.string("Copied")
                        } else {
                            React.string("Copy")
                        }
                    } 
                </Button>
                <Button onClick={_=>onClose()} > {React.string("Close")} </Button>
            </Row>
            <pre style=ReactDOM.Style.make(~overflowWrap="break-word", ~whiteSpace="pre-wrap", ())>
                {React.string(proofText)}
            </pre>
            {rndProofTable()}
        </Col>
    </Paper>
}
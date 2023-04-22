open Expln_React_common
open Expln_React_Mui
open MM_react_common
open Local_storage_utils

let showProofTableLocStorKey = "export-proof-show-proof-table"
let essentialsOnlyLocStorKey = "export-proof-essentials-only"

@react.component
let make = (
    ~proofText:string, 
    ~proofTableWithTypes:string, 
    ~proofTableWithoutTypes:string,
    ~onClose:unit=>unit
) => {
    let (showProofTable, setShowProofTablePriv) = React.useState(() => {
        locStorReadBool(showProofTableLocStorKey)->Belt_Option.getWithDefault(false)
    })
    let setShowProofTable = (showProofTable:bool):unit => {
        locStorWriteBool(showProofTableLocStorKey, showProofTable)
        setShowProofTablePriv(_ => showProofTable)
    }

    let (essentialsOnly, setEssentialsOnlyPriv) = React.useState(() => {
        locStorReadBool(essentialsOnlyLocStorKey)->Belt_Option.getWithDefault(false)
    })
    let setEssentialsOnly = (essentialsOnly:bool):unit => {
        locStorWriteBool(essentialsOnlyLocStorKey, essentialsOnly)
        setEssentialsOnlyPriv(_ => essentialsOnly)
    }

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
            <Row>
                <FormControlLabel
                    control={
                        <Checkbox
                            checked=showProofTable
                            onChange={evt2bool(setShowProofTable)}
                        />
                    }
                    label="proof table"
                />
                <FormControlLabel
                    control={
                        <Checkbox
                            checked=essentialsOnly
                            onChange={evt2bool(setEssentialsOnly)}
                        />
                    }
                    label="essentials only"
                    disabled={!showProofTable}
                />
                <Button onClick={_=>copyToClipboard(getTextToCopy())} variant=#contained > {React.string("Copy")} </Button>
                <Button onClick={_=>onClose()} > {React.string("Close")} </Button>
            </Row>
            <pre style=ReactDOM.Style.make(~overflowWrap="break-word", ~whiteSpace="pre-wrap", ())>
                {React.string(proofText)}
            </pre>
            {rndProofTable()}
        </Col>
    </Paper>
}
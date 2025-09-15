open Expln_React_common
open Expln_React_Mui

@react.component
let make = (
    ~patternVersion:int,
    ~highlightMatchedSymbols:bool,
    ~onOk:(~patternVersion:int, ~highlightMatchedSymbols:bool)=>unit, 
    ~onCancel:unit=>unit,
) => {
    let (patternVersion, setPatternVersion) = React.useState(() => patternVersion)
    let (highlightMatchedSymbols, setHighlightMatchedSymbols) = React.useState(() => highlightMatchedSymbols)

    let actOk = () => {
        onOk(~patternVersion, ~highlightMatchedSymbols)
    }

    let actCancel = () => {
        onCancel()
    }

    <Col spacing=1.>
        <Row alignItems=#center>
            {React.string("Pattern version")}
            <RadioGroup
                row=true
                value={patternVersion->Belt_Int.toString}
                onChange=evt2str(str => setPatternVersion(_=>Int.fromString(str)->Option.getOr(1)))
            >
                <FormControlLabel value="1" control={ <Radio/> } label="1" />
                <FormControlLabel value="2" control={ <Radio/> } label="2" />
            </RadioGroup>
        </Row>
        <FormControlLabel
            control={
                <Checkbox
                    checked={patternVersion!=1 && highlightMatchedSymbols}
                    onChange=evt2bool(b => setHighlightMatchedSymbols(_=>b))
                    disabled={patternVersion==1}
                />
            }
            label="Highlight matched symbols"
        />
        <Row alignItems=#center>
            <Button onClick=(_=>actOk()) variant=#contained > 
                { React.string("Ok") }
            </Button>
            <Button onClick={_=>actCancel()} > {React.string("Cancel")} </Button>
        </Row>
    </Col>
}
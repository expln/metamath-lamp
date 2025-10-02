open Expln_React_common
open Expln_React_Mui

@react.component
let make = (
    ~patternVersion:int,
    ~highlightMatchedSymbols:bool,
    ~highlightColor:string,
    ~onOk:(~patternVersion:int, ~highlightMatchedSymbols:bool, ~highlightColor:string)=>unit, 
    ~onCancel:unit=>unit,
) => {
    let (patternVersion, setPatternVersion) = React.useState(() => patternVersion)
    let (highlightMatchedSymbols, setHighlightMatchedSymbols) = React.useState(() => highlightMatchedSymbols)
    let (highlightColor, setHighlightColor) = React.useState(() => highlightColor)

    let actOk = () => {
        onOk(~patternVersion, ~highlightMatchedSymbols, ~highlightColor)
    }

    let actCancel = () => {
        onCancel()
    }

    let highlightMatchedSymbolsFinal = patternVersion!=1 && highlightMatchedSymbols

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
        <Row>
            <FormControlLabel
                control={
                    <Checkbox
                        checked=highlightMatchedSymbolsFinal
                        onChange=evt2bool(b => setHighlightMatchedSymbols(_=>b))
                        disabled={patternVersion==1}
                    />
                }
                label="Highlight matched symbols"
            />
            {
                if (highlightMatchedSymbolsFinal) {
                    MM_react_common.rndColorSelect(
                        ~availableColors=MM_cmp_settings.allColors, 
                        ~selectedColor=highlightColor, 
                        ~onNewColorSelected=newHighlightColor=>setHighlightColor(_=>newHighlightColor),
                        ~label="Color"
                    )
                } else {
                    React.null
                }
            }
        </Row>
        <Row alignItems=#center>
            <Button onClick=(_=>actOk()) variant=#contained > 
                { React.string("Ok") }
            </Button>
            <Button onClick={_=>actCancel()} > {React.string("Cancel")} </Button>
        </Row>
    </Col>
}
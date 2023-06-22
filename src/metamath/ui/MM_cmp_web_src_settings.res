open Expln_React_common
open Expln_React_Mui

type webSrcSettingsState = {
    id: string,
    alias: string,
    url: string,
    trusted: bool,
    err: option<string>,
}

@react.component
let make = (
    ~webSrcSettings:array<webSrcSettingsState>, 
    ~onAdd: unit => unit, 
    ~onAliasChange: (string,string) => unit, 
    ~onUrlChange: (string,string) => unit, 
    ~onTrustedChange: (string,bool) => unit,
    ~onDelete: string => unit,
    ~defaultIds:array<string>,
) => {
    let rndWebSrcSetting = (src:webSrcSettingsState) => {
        let isDefault = defaultIds->Js_array2.includes(src.id)
        <Col key=src.id>
            <Row>
                <TextField label="Alias (optional)" size=#small style=ReactDOM.Style.make(~width="150px", ()) 
                    value=src.alias onChange=evt2str(onAliasChange(src.id,_)) 
                    disabled=isDefault
                />
                <TextField label="URL" size=#small style=ReactDOM.Style.make(~width="400px", ()) 
                    value=src.url onChange=evt2str(onUrlChange(src.id,_)) 
                    disabled=isDefault
                />
                <FormControlLabel
                    control={
                        <Checkbox
                            checked=src.trusted
                            onChange=evt2bool(onTrustedChange(src.id,_))
                        />
                    }
                    label="Download without warning"
                    style=ReactDOM.Style.make(
                        ~border="solid 1px lightgrey", 
                        ~borderRadius="7px", 
                        ~paddingRight="10px",
                        ~marginTop="-2px",
                        ~marginLeft="2px",
                        ()
                    )
                />
                <IconButton key="delete-button" onClick={_ => onDelete(src.id)} disabled=isDefault>
                    <MM_Icons.Delete/>
                </IconButton>
            </Row>
            {
                switch src.err {
                    | None => React.null
                    | Some(msg) => <pre style=ReactDOM.Style.make(~color="red", ())>{React.string(msg)}</pre>
                }
            }
        </Col>
    }

    <Col style=ReactDOM.Style.make(~marginTop="5px", ())>
        { webSrcSettings->Js_array2.map(rndWebSrcSetting)->React.array }
        <Row alignItems={#baseline} >
            <IconButton key="add-button" onClick={_ => onAdd()}>
                <MM_Icons.Add/>
            </IconButton>
        </Row>
    </Col>
}
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
) => {
    let rndWebSrcSetting = (src:webSrcSettingsState) => {
        <Col key=src.id>
            <Row>
                <TextField label="Alias" size=#small style=ReactDOM.Style.make(~width="250px", ()) 
                    value=src.alias onChange=evt2str(onAliasChange(src.id,_)) />
                <TextField label="URL" size=#small style=ReactDOM.Style.make(~width="400px", ()) 
                    value=src.url onChange=evt2str(onUrlChange(src.id,_)) />
                <FormControlLabel
                    control={
                        <Checkbox
                            checked=src.trusted
                            onChange=evt2bool(onTrustedChange(src.id,_))
                        />
                    }
                    label="Do not ask before downloading"
                    style=ReactDOM.Style.make(
                        ~border="solid 1px lightgrey", 
                        ~borderRadius="7px", 
                        ~paddingRight="10px",
                        ~marginTop="-2px",
                        ~marginLeft="2px",
                        ()
                    )
                />
                <IconButton key="delete-button" onClick={_ => onDelete(src.id)}>
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
        <IconButton key="add-button" onClick={_ => onAdd()}>
            <MM_Icons.Add/>
        </IconButton>
    </Col>
}
open Expln_React_Mui

let rndProgress = (~text:string, ~pct:float, ~onTerminate:option<unit=>unit>=?, ()) => {
    <Paper style=ReactDOM.Style.make(~padding=onTerminate->Belt.Option.map(_=>"5px")->Belt.Option.getWithDefault("10px"), ())>
        <Row alignItems=#center spacing=1.>
            <span style=ReactDOM.Style.make(~paddingLeft="10px", ())>
                {`${text}: ${(pct *. 100.)->Js.Math.round->Belt.Float.toInt->Belt_Int.toString}%`->React.string}
            </span>
            {
                switch onTerminate {
                    | None => React.null
                    | Some(onTerminate) => {
                        <IconButton onClick={_ => onTerminate()}>
                            <MM_Icons.CancelOutlined/>
                        </IconButton>
                    }
                }
            }
        </Row>
    </Paper>
}
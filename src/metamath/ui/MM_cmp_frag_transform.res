open MM_syntax_tree
open Expln_React_Modal
open MM_react_common
open Expln_React_common
open Expln_React_Mui

type selection = {
    "getText": () => string,
}

let syntaxTreeToSelection = (tree:childNode):selection => {
    {
        "getText": () => tree->syntaxTreeToText,
    }
}

type fragmentTransform = {
    canApply: selection => bool,
    displayName: selection => string,
}

let stringToFragTranformsUnsafe: string => {..} = %raw(`body => new Function("", body)()`)

let stringToFragTranforms = (str:string):result<array<fragmentTransform>,string> => {
    let transforms = stringToFragTranformsUnsafe(str)
    switch transforms["length"]->Js.Nullable.toOption {
        | None => Error(`transforms["length"] is None`)
        | Some(len) => {
            Js.Console.log2(`transforms["length"]`, len)
            Ok([])
        }
    }
}

@react.component
let make = (
    ~modalRef: modalRef,
    ~onCancel:unit=>unit,
    // ~selectedSubtree:childNode,
) => {
    switch stringToFragTranforms(`
        const tr1 = {
            name: "action1",
        }
        const tr2 = {
            name: "action2",
        }
        return [tr1, tr2]
    `) {
        | Error(msg) => {
            openInfoDialog( ~modalRef, ~text=msg, (), )
        }
        | Ok(transforms) => {
            Js.Console.log2(`transforms->Js_array2.length`, transforms->Js_array2.length)
        }
    }

    let rndButtons = () => {
        <Row>
            <Button onClick={_=>onCancel()} variant=#outlined>
                {React.string("Cancel")}
            </Button>
        </Row>
    }

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.>
            { rndButtons() }
        </Col>
    </Paper>
}
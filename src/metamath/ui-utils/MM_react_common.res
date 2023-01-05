open Expln_React_common

let rndProgress: (~text:string, ~pct:float) => reElem = (~text:string, ~pct:float) => {
    <span style=ReactDOM.Style.make(~padding="10px", ())>
        {`${text}: ${(pct *. 100.)->Js.Math.round->Belt.Float.toInt->Belt_Int.toString}%`->React.string}
    </span>
}
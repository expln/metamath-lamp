open MM_api

let apiSetMarkFirstProvableStepAsGoal = (
    ~params:apiInput,
    ~setMarkFirstProvableStepAsGoal:bool=>promise<result<unit,string>>
):promise<result<unit,string>> => {
    switch params->apiInputToJson->JSON.Decode.bool {
        | None => Promise.resolve(Error("The parameter of setMarkFirstProvableStepAsGoal() must be a boolean."))
        | Some(bool) => setMarkFirstProvableStepAsGoal(bool)
    }
}

let apiGetMarkFirstProvableStepAsGoal = (
    ~markFirstProvableStepAsGoal:bool
):promise<result<JSON.t,string>> => {
    Promise.resolve(Ok(JSON.Encode.bool(markFirstProvableStepAsGoal)))
}

let updateSettingsApi = (
    ~setMarkFirstProvableStepAsGoal:bool=>promise<result<unit,string>>,
    ~markFirstProvableStepAsGoal:bool,
) => {
    setSettingsApi({
        "setMarkFirstProvableStepAsGoal": makeApiFunc(
            "settings.setMarkFirstProvableStepAsGoal", 
            params => apiSetMarkFirstProvableStepAsGoal( ~params, ~setMarkFirstProvableStepAsGoal, )
        ),
        "getMarkFirstProvableStepAsGoal": makeApiFunc(
            "settings.getMarkFirstProvableStepAsGoal", 
            _ => apiGetMarkFirstProvableStepAsGoal( ~markFirstProvableStepAsGoal, )
        ),
    })
}
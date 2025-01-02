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

let updateSettingsApi = (
    ~setMarkFirstProvableStepAsGoal:bool=>promise<result<unit,string>>
) => {
    setSettingsApi({
        "setMarkFirstProvableStepAsGoal": makeApiFunc(
            "settings.setMarkFirstProvableStepAsGoal", 
            params => apiSetMarkFirstProvableStepAsGoal( ~params, ~setMarkFirstProvableStepAsGoal, )
        ),
    })
}
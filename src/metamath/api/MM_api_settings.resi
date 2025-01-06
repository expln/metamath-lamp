let updateSettingsApi: (
  ~setMarkFirstProvableStepAsGoal: bool => promise<result<unit, string>>,
  ~markFirstProvableStepAsGoal:bool,
) => unit

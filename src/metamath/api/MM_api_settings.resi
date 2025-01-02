let updateSettingsApi: (
  ~setMarkFirstProvableStepAsGoal: bool => promise<result<unit, string>>,
) => unit

type asrtsToSkipResult = {
    regex: string, 
    asrtsToSkip: array<string>
}

@react.component
let make: (
  ~initText: string,
  ~initRegex: string,
  ~onSave: asrtsToSkipResult => unit,
  ~onCancel: () => unit,
) => React.element

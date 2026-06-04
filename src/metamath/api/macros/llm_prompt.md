### The task
Your task is to help me to develop a Metamath proof using metamath-lamp (mm-lamp) tool.
Mm-lamp has built-in API functions which you can call to add/update/delete statements in the editor 
and invoke different features of mm-lamp like bottom-up prover, etc.
Each API function accepts at most one JSON object as a parameter.
Each such JSON object has a predefined structure depending on the function.
During our conversation you will need to ask me to execute one of the available API functions.
To ask me to execute an API function, put a JSON object at the end of your reply:
```json
{
  "functionName": string,
  "parameters": jsonObject
}
```
`parameters` is an optional attribute.
You should not include it if an API function doesn't accept any parameters.
Make sure that JSON object is at the very end of your reply.
I will execute the function you asked and reply to you with the output of the function.
The output of the function and all the previous our conversation should help you to come up with the next action 
to do to get the proof complete.

### Available API functions

#### getState

The `getState` function doesn't accept any parameters.
It returns the current state of the editor.

#### addSteps

The `addSteps` function accepts new steps to add to the editor.
It returns the state of the editor after the steps have been added.
The format of the parameter it accepts is as follows:

```json
{
  "beforeLabel": string, 
  "afterLabel":  string, 
  "variables": [[variableType:string, variableName:string]], 
  "steps": [{
        "label": string,
        "type": string,
        "justification": string,
        "statement": string,
  }]
}
```
* `beforeLabel` is a label of an existing statement before which the new steps should be added.
`afterLabel` is a label of an existing statement after which the new steps should be added.
Both `beforeLabel` and `afterLabel` are optional.
If neither `beforeLabel` nor `afterLabel` is specified then the new steps will be added at the very bottom 
of the editor, but before the goal step if it exists.
If both `beforeLabel` and `afterLabel` are specified then only `beforeLabel` will be used.
* `variables` is an array of new variables to add to the editor.
Each element of this array is a two-element sub-array of strings.
The first string in such a sub-array is the type of the variable to add.
The second string in a sub-array is the name of the variable.
It makes sense to add only variables which are used in statements.
Unused variables will be removed automatically.
See the [Optimizations to consider](#optimizations-to-consider) section to understand when adding new variables
may make sense.
Example value of `variables`: `[["setvar","set_of_all_sets"], ["class","number_of_apples"]]`
* `steps` is an array of steps to add.
  * `label` is the label of a step. It is optional. If it is omitted then a new unique label will be generated.
  * `type` is the type of step. 
  It can be one of: `"h"` - hypothesis, `"p"` - regular provable step, `"g"` - the goal step.
  If `type` is omitted then `"p"` is used by default.
  * `justification` is the justification of a step. It is optional. 
  If it is omitted then the step will not have a justification.
  * `statement` is the statement itself (the content of the step). It is a required attribute of a step object.

#### updateSteps

`updateSteps` modifies existing steps.
It returns the state of the editor after the steps have been modified.

### Optimizations to consider
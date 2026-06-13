## The task
Your task is to help me to develop a Metamath proof using metamath-lamp (mm-lamp) tool.
Mm-lamp has built-in API functions which you can call to add/update/delete steps in the editor 
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
Always put your request to execute an API function at the very end of your reply.
I will execute the function you asked and reply to you with the output of the function.
The output of the function and all the previous conversation should help you to come up with the next action 
to do to get the proof complete.

## Available API functions

### getState

The `getState` function doesn't accept any parameters.
It returns the current state of the editor.
The returned editor state consists of:
* definition of local variables
* disjoint variable groups
* list of steps

Each step consists of:
* one line with the statement of the step and some its attributes (status, labels, etc.)
* optional few lines containing error messages related to the step

The format of the line with the step statement is as follows:  

`type status label statement`

`type` can be:
* `H` - hypothesis
* `P` or `p` - regular provable step. The uppercase for bookmarked steps, the lowercase for unbookmarked steps.
* `G` - the goal step

`status` can be:
* `v` - step is proved
* `?` - step is not proved
* `~` - step has a valid justification but some its dependency steps are not proved yet
* `x` - step has an invalid justification
* `.` - step is a hypothesis or there is an error in the editor that prevented mm-lamp from determining the status

`label` must not contain whitespaces.

Not all steps will be returned. Only steps listed below will be returned:
* bookmarked steps
* hypothesis steps and the goal step
* steps with status `?`, `~`, or `x`
* steps with errors

All other steps will be returned.

### addSteps

The `addSteps` function accepts new steps to add to the editor.
It returns the state of the editor after the steps have been added (the output of the `getState` function).
The format of the parameter object it accepts is as follows:

```json
{
  "beforeLabel": string, 
  "afterLabel":  string, 
  "variables": [[variableType:string, variableName:string]], 
  "steps": [{
    "label": string,
    "type": string,
    "justification": string,
    "statement": string
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
You don't need to add existing variables. 
You always can use existing (predefined) variables when you need them in the proof.
It makes sense to add only variables which are used in statements.
Unused variables will be removed automatically.
See the [Optimizations to consider](#optimizations-to-consider) section to understand when adding new variables
may be needed.
Example value of `variables`: `[["setvar","set_of_all_sets"], ["class","number_of_apples"]]`
* `steps` is an array of steps to add.
  * `label` is the label of a step. It is optional. If it is omitted then a new unique label will be generated.
  * `type` is the type of step. 
  It can be one of: `"H"` - hypothesis, `"P"` - regular provable step, `"G"` - the goal step.
  If `type` is omitted then `"P"` is used by default.
  * `justification` is the justification of a step. It is optional. 
  If it is omitted then the step will not have a justification.
  * `statement` is the statement itself (the content of the step). It is a required attribute of a step object.

All steps added by the `addSteps` function are bookmarked by default.

The order of steps does matter.
A step can be derived from preceding steps only.

### updateSteps

`updateSteps` modifies existing steps.
It returns the state of the editor after the steps have been modified (the output of the `getState` function).
The format of the parameter object it accepts is as follows:
```json
{
  "steps": [{
    "label": string,
    "type": string,
    "justification": string,
    "statement": string,
    "isBookmarked": boolean
  }]
}
```
`steps` is an array of steps to update. 
Meaning of all attributes of a step object is the same as for the `addSteps` function.
`label` is a required attribute. It must be a label of an existing statement.
All other attributes are optional.
If any of the optional attributes is missing then it will not change for the step.
So, only attributes which need to be changed should be provided.

### deleteSteps

`deleteSteps` deletes existing steps.
It returns the state of the editor after the steps have been deleted (the output of the `getState` function).
The format of the parameter object it accepts is as follows:
```json
{
  "labels": [string]
}
```
`labels` is an array of labels of steps to delete.

### prove

`prove` starts a bottom-up prover for the specified steps.
If a proof is found then this function returns the state of the editor with the found proof applied 
(saved in the editor).
If no proof found then this function doesn't return anything.
In such cases I will write you that no proof found. And you will need to think what to do next.
The format of the parameter object the `prove` function accepts is as follows:
```json
{
  "stepToProve": string,
  "stepsToDeriveFrom": [string]
}
```
* `stepToProve` is a label of an existing step which needs to be proved. This is a required attribute.
* `stepsToDeriveFrom` is an optional array of existing labels which may be used to prove the `stepToProve`.
Steps referenced in `stepsToDeriveFrom` must precede the `stepToProve` in the editor.
See the [Optimizations to consider](#optimizations-to-consider) section 
for the specifics on how to use this attribute.

### findAssertions

`findAssertions` returns assertions which match a specified pattern.
The format of the parameter object the `findAssertions` function accepts is as follows:
```json
{
  "pattern": string,
  "pageNum": int
}
```
* `pattern` is the pattern to search by. This should be [the version 2 pattern](https://github.com/expln/metamath-lamp-docs/blob/master/mm_lamp_versions/dev/explorer/search_by_pattern_v2.md).
* `pageNum` the number of the page to return. This is an optional parameter.
Its default value is 1. 
The output of the `findAssertions` function includes the maximum value of `pageNum`
for the specified pattern.


## Optimizations to consider

### Skip explicit justifications

In most cases mm-lamp can find justifications for steps itself if all the required steps to use in the justification
are present in the editor (and precede the step to prove).
This means you don't have to provide justifications when adding new steps.
Skipping explicit justifications will decrease number of errors when an incorrect assertion label is used
(caused by your hallucination or if the assertion was renamed recently).
So, try not to provide explicit justifications and let mm-lamp to figure them out.

### Use bottom-up prover

Additionally to the skipping explicit justifications technique, you can skip some intermediate steps 
and use the bottom-up prover to let mm-lamp to find some simple/obvious intermediate steps. 

The bottom-up prover attempts to prove a step by working backwards from its statement. 
Understanding how it works internally will help you use it wisely and avoid wasting calls on steps it cannot prove.

#### How the prover works

At the **top level**, the prover tries to find a justification for the step being proved by combining:

* assertions from the loaded database, and
* steps explicitly listed in stepsToDeriveFrom (only preceding steps in the editor are eligible).

If `stepsToDeriveFrom` is omitted, the prover relies solely on database assertions at the top level 
— which only works when the step follows directly from axioms or theorems with no editor steps needed 
(e.g. `|- ( 7 + 2 ) e. CC`).

At **deeper levels**, the prover does not reuse editor steps or `stepsToDeriveFrom` entries directly. 
Instead, it repeatedly applies database assertions in reverse: 
it takes a statement that still needs to be proved and asks "which assertion could have produced this?"
generating new sub-goals.
It then checks whether each sub-goal is already proved 
— either by matching a database assertion directly, or by matching a proved step in the editor.
If a sub-goal is not immediately proved, the prover recurses deeper, up to a fixed depth limit 
(not controllable via the API).
Two constraints prevent infinite expansion: 
generated sub-goals must be strictly shorter in symbol count than the statement they came from,
and no new variables may be introduced. 
The one exception to the strict-shorter rule is statements ending with `e. _ )` or `e. _`
(where `_` is any single symbol):
for these, sub-goals of equal length are also allowed. 
This relaxation is what enables transitions like `|- A e. RR` → `|- A e. CC`, 
where both statements have the same length.

#### When the prover succeeds

The prover works well for steps that are:

* close to trivially true given the assertions already in scope (e.g. `|- A e. CC` from `|- A e. RR`), or
* simple rearrangements or reductions (e.g. reassociating `+`), or
* provable by a short chain of database assertions with no complex editor-step dependencies 
beyond what is in `stepsToDeriveFrom`.

#### When the prover fails — and what to do

The most common reason for failure is that the step is **too complex**:
the proof tree it would need to build exceeds the fixed depth,
or the required intermediate results are not reachable via reverse application from the current statement.
In such cases, do not retry the prover on the same step unchanged.
Instead, **add explicit intermediate steps** that break the goal into simpler pieces,
then prove each piece in sequence.
The prover is most effective as a finisher for the last small gap, not as a solver for large leaps.

Providing too many labels in `stepsToDeriveFrom` slows the prover significantly;
up to 5 labels is a practical ceiling.
If the prover fails even with well-chosen `stepsToDeriveFrom`, 
that is a strong signal to decompose the step further rather than expand the label list.

### Keep minimal number of visible steps

Very often Metamath proofs become too lengthy.
Sending a lot of proved steps to you each time is not desired because it will make the conversation difficult
to read. Also, you probably don't need to always see all the steps because you can remember the idea of the
proof. Probably you will need to see only some recent proved and unproved steps.
Another problem with long proofs is that they will consume my token quota.
So, you need to keep number of visible steps at a minimum level.
Whenever a step becomes proved, and you anticipate you will not need to explicitly reference it in the
further proof, then hide such step.
You can hide steps by setting `"isBookmarked": false` for them in the `updateSteps` function.

### Meaningful variable names

Sometimes you can make a proof more readable by using meaningful variable names.
For example, instead of using the predefined in set.mm variable `x`,
you can use a meaningful name like `set_of_all_sets`.
Whenever you think I or you will benefit from using meaningful variable names,
you can introduce them via the `variables` input parameter of the `addSteps` function.

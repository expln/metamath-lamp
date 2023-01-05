let procName: string

type request = 
  | FindParens({ctx: MM_context.mmContext})

type response = 
  | FindParensProgress({pct: float}) 
  | FindParensDone({parens: string})

let beginFindParens: (~ctx: MM_context.mmContext, ~onProgress: float => unit, ~onDone: string => unit, ) => unit

let processOnWorkerSide: (~req: request, ~sendToClient: response => unit) => unit

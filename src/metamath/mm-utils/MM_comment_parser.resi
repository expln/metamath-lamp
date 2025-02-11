type commentPart = 
    | Text(string) 
    | MathMode(string) 
    | LabelMode(string)

let splitCommentIntoParts: string => array<commentPart>

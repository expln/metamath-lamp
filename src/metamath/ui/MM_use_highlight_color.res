let useHighlightColor = ():(string, (string=>string)=>unit) => {
    Local_storage_utils.useStateFromLocalStorage(
        ~key="pe-index-highlight-color", 
        ~fromString=strOpt => {
            let defaultRes = MM_cmp_settings.defaultHighlightColor
            switch strOpt {
                | None => defaultRes
                | Some(color) => MM_cmp_settings.allColors->Array.find(c=>c==color)->Option.getOr(defaultRes)
            }
        },
        ~toString=str=>str
    )
}
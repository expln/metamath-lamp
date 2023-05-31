"use strict";

import {Dialog} from "@mui/material";
import * as React from "react";

const make = ({opn, children, maxWidth, fullWidth}) => {
    return React.createElement(
        Dialog,
        {
            open:opn,
            maxWidth,
            fullWidth
        },
        children
    )
}

export default make
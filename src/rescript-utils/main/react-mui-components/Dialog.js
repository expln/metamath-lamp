"use strict";

import {Dialog} from "@mui/material";
import * as React from "react";

const make = ({opn, children, maxWidth, fullWidth, fullScreen, slots}) => {
    return React.createElement(
        Dialog,
        {
            open:opn,
            maxWidth,
            fullWidth,
            fullScreen,
            slots
        },
        children
    )
}

export default make
"use strict";

import {Menu} from "@mui/material";
import * as React from "react";

const make = ({opn, children, anchorEl, onClose}) => {
    return React.createElement(
        Menu,
        {
            open:opn,
            anchorEl,
            onClose
        },
        children
    )
}

export default make
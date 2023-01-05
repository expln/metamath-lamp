"use strict";

import {Dialog} from "@mui/material";

const make = ({opn, children, maxWidth, fullWidth}) => {
    return <Dialog open={opn} maxWidth={maxWidth} fullWidth={fullWidth}>{children}</Dialog>
}

export default make
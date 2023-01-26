"use strict";

import Select from '@mui/material/Select';

const make = ({ 
    labelId,
    label,
    value,
    onChange,
    onClose,
    children,
}) => {
    return (<Select
        labelId
        label
        value
        onChange
        onClose
    >
        {children}
    </Select>)
}

export default make
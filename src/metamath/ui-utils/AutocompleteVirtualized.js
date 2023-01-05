'use strict';

import * as React from 'react'
import TextField from '@mui/material/TextField'
import Autocomplete, { autocompleteClasses } from '@mui/material/Autocomplete'
import useMediaQuery from '@mui/material/useMediaQuery'
import Popper from '@mui/material/Popper'
import { useTheme, styled } from '@mui/material/styles'
import { VariableSizeList } from 'react-window'
import Typography from '@mui/material/Typography'

const LISTBOX_PADDING = 8 // px

function renderRow({ data, index, style }) {
    const dataSet = data[index]
    const inlineStyle = {
        ...style,
        top: style.top + LISTBOX_PADDING,
    }

    return (
        <Typography component="li" {...dataSet[0]} noWrap style={inlineStyle}>
            {dataSet[1]}
        </Typography>
    )
}

const OuterElementContext = React.createContext({})

const OuterElementType = React.forwardRef((props, ref) => {
    const outerProps = React.useContext(OuterElementContext)
    return <div ref={ref} {...props} {...outerProps} />
})

function useResetCache(data) {
    const ref = React.useRef(null)
    React.useEffect(() => {
        if (ref.current != null) {
            ref.current.resetAfterIndex(0, true)
        }
    }, [data])
    return ref
}

// Adapter for react-window
const ListboxComponent = React.forwardRef((props, ref) => {
    const { children, ...other } = props

    const theme = useTheme()
    const smUp = useMediaQuery(theme.breakpoints.up('sm'), {
        noSsr: true,
    })

    const itemCount = children.length
    const itemSize = smUp ? 36 : 48

    const getHeight = () => {
        if (itemCount > 8) {
            return 8 * itemSize
        } else {
            return itemSize * children.length
        }
    }

    const gridRef = useResetCache(itemCount)

    return (
        <div ref={ref}>
            <OuterElementContext.Provider value={other}>
                <VariableSizeList
                    itemData={children}
                    height={getHeight() + 2 * LISTBOX_PADDING}
                    width="100%"
                    ref={gridRef}
                    outerElementType={OuterElementType}
                    innerElementType="ul"
                    itemSize={index => itemSize}
                    overscanCount={5}
                    itemCount={itemCount}
                >
                    {renderRow}
                </VariableSizeList>
            </OuterElementContext.Provider>
        </div>
    )
})

const StyledPopper = styled(Popper)({
    [`& .${autocompleteClasses.listbox}`]: {
        boxSizing: 'border-box',
        '& ul': {
            padding: 0,
            margin: 0,
        },
    },
})

export default function make({value, options, onChange, size}) {
    return (
        <Autocomplete
            sx={{ width: 300 }}
            size={size}
            disableListWrap
            PopperComponent={StyledPopper}
            ListboxComponent={ListboxComponent}
            value={value === undefined ? null : value}
            options={options}
            isOptionEqualToValue={(option,value) => option === value}
            renderInput={(params) => <TextField {...params} label="Label" />}
            renderOption={(props, option) => [props, option]}
            onChange={(e, value) => onChange(value === null ? undefined : value)}
        />
    )
}

"use strict";

import { styled } from '@mui/material/styles';
import AccordionSummary from '@mui/material/AccordionSummary';

const AccordionSummaryStyled = styled(AccordionSummary)(({ theme }) => ({
    backgroundColor: 'rgba(0, 0, 0, .03)',
    flexDirection: 'row-reverse',
}))

export default AccordionSummaryStyled
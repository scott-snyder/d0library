      SUBROUTINE CDC_L2TRK
     &         (PARAM_SET_NUMBER,HARDWARE,RESULT_FLAG,EXTRA_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dummy first pass at TOOL
C-
C-   Inputs  : PARAM_SET_NUMBER : Number in series of parameters to use.
C-             HARDWARE : Mask with bit set for Level-1 trigger which
C-                        started this filter.
C-   Outputs : RESULT_FLAG : Flag set to TRUE when TOOL wants event passed
C-             EXTRA_FLAG :  Not used
C-   Controls: None
C-
C-   Created  13-APR-91   by the L2STATE program
C-   Updated  14-APR-1991   Srini Rajagopalan  Include Filter call 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PARAM_SET_NUMBER,HARDWARE
      LOGICAL RESULT_FLAG,EXTRA_FLAG
C----------------------------------------------------------------------
      RESULT_FLAG=.TRUE.
C
      CALL COSMIC_L2CDC(RESULT_FLAG)
C
  999 RETURN
      END

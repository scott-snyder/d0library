      SUBROUTINE EZGET_SIZE (PARAM,NVAL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the array size in full words (32 bits)
C-                         for identifier PARAM from a pre-selected
C-                         RCP bank.  
C-
C-   Inputs  : PARAM   [C*]    Name of parameter (CHARACTER string)
C-
C-   Outputs : NVAL            Value(s) pertaining to name ``PARAM''.
C-
C-             IER      [I]     Error code
C-
C-                               0 --> OK
C-                              -1 --> Variable not found
C-                              -2 --> Bank not found
C-
C-   Controls: None
C-   Calls:    EZGETA
C-   Created 2-MAR-1992   Harrison B. Prosper, Chip Stewart 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) PARAM
      INTEGER       NVAL
      INTEGER       IER
C----------------------------------------------------------------------
      CALL EZGETA (PARAM,0,0,0,NVAL,IER)
  999 RETURN
      END

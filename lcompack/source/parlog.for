      CHARACTER*40 FUNCTION PARLOG(LIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return a left justified string from logical variable
C-
C-   Inputs  : LIN: Logical to convert to string
C-   Outputs : None
C-   Controls: None
C-
C-   Original April 1987 Jan S. Hoftun
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE 
      LOGICAL LIN
      CHARACTER*40 TEMP
C----------------------------------------------------------------------
      IF(LIN) THEN 
         WRITE(TEMP,FMT='(''True'')') 
      ELSE
         WRITE(TEMP,FMT='(''False'')') 
      ENDIF
      PARLOG=TEMP
      RETURN
      END

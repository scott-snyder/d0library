      CHARACTER*40 FUNCTION PAREAL(XIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return a left justified string from real number
C-
C-   Inputs  : XIN: Real number to convert to string
C-   Outputs : None
C-   Controls: None
C-
C-   Original April 1987 Jan S. Hoftun
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL XIN
      INTEGER K
      CHARACTER*40 TEMP
C----------------------------------------------------------------------
      WRITE(TEMP,FMT='(F25.4)') XIN
      DO K=1,20
        IF(TEMP(1:1).EQ.' ') TEMP=TEMP(2:)
      ENDDO
      PAREAL=TEMP
      RETURN
      END

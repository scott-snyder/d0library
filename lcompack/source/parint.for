      CHARACTER*40 FUNCTION PARINT(IN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return a left justified string from integer
C-
C-   Inputs  : IN: Integer to convert to string
C-   Outputs : None
C-   Controls: None
C-
C-   Original April 1987 Jan S. Hoftun
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE 
      INTEGER IN,K
      CHARACTER*40 TEMP
C----------------------------------------------------------------------
      WRITE(TEMP,FMT='(I12)') IN
      DO K=1,12
         IF(TEMP(1:1).EQ.' ') TEMP=TEMP(2:)
      ENDDO
      PARINT=TEMP
      RETURN
      END

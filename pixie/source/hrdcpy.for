      SUBROUTINE HRDCPY( DRVNAM, ICTAB, IFTAB )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets the color and fill tables for printing a
C-              hardcopy in the LN03.
C-
C-   Inputs  : DRVNAM - name of the driver in use.
C-
C-   Outputs : ICTAB - color table
C-             IFTAB - fill table
C-
C-   Created  24-APR-1989   LUPE ROSAS
C-   Updated   5-OCT-1989   Lupe Rosas  Foreground color
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*3 DRVNAM
      INTEGER ICTAB(*), IFTAB(*), FILL(17), I
      DATA FILL/ 0, 47, 26, 25, 44, 27, 43, 45, 28, 11, 23, 12, 46, 
     &          41, 24, 42, 47/
C----------------------------------------------------------------------
CC      IF ( (DRVNAM .EQ. 'LN3') .OR. (DRVNAM.EQ.'GPV') ) THEN
        DO 10 I=1, 17
          IFTAB(I) = FILL(I)
   10   CONTINUE
C      ENDIF
        ICTAB(1)  = 7
        ICTAB(2)  = 8
        ICTAB(17) = 8
  999 RETURN
      END

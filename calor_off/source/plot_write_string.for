      SUBROUTINE PLOT_WRITE_STRING
     &  (INTENSITY1,INTENSITY2,COLOR1,COLOR2,X,Y,STRING)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-   Define initial and final intensities and colors, move to position
C-   (X,Y) in the world coordinate frame and write out given string.
C-   The string is centred at (X,Y).
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   8-SEP-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER       INTENSITY1
      INTEGER       INTENSITY2
      INTEGER       COLOR1
      INTEGER       COLOR2
      REAL          X,Y
      CHARACTER*(*) STRING
C----------------------------------------------------------------------
      CALL JJUST  (2,2)              ! Justify centre,centre
      CALL JINTEN (INTENSITY1)
      CALL JCOLOR (COLOR1)
      CALL JMOVE  (X,Y)
      CALL J3STRG (STRING(1:LEN(STRING)))
      CALL JCOLOR (COLOR2)
      CALL JINTEN (INTENSITY2)
  999 RETURN
      END

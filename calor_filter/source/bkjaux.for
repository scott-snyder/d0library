      SUBROUTINE BKJAUX(NCHA,LJAUX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book JAUX, Jets Auxilary bank. 
C-
C-   Inputs  : NCHA = # of 'Jet candidates' we want to store
C-   Outputs : LJAUX= pointer to JAUX
C-   Controls:
C-
C-   Created  14-MAY-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:JAUX.PARAMS'   ! params for JAUX
      INCLUDE 'D0$LINKS:IZJAUX.LINK'
      INTEGER NCHA,LJAUX,LSUP1
      INTEGER IOH, GZFRES
      SAVE IOH
      INTEGER NSIZ                      ! Total number of data words
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      NSIZ = NCHA*NREP_JAUX + 4

      IF ( FIRST ) THEN
        CALL MZFORM('JAUX','4I/3I6F4I',IOH)
        FIRST = .FALSE.
      END IF
C--- Get supporting link
      LSUP1 = GZFRES()
C---Book it
      IF (LSUP1 .LE. 0) RETURN
      CALL MZBOOK(IXMAIN,LJAUX,LSUP1,-IZJAUX,'JAUX',0,0,NSIZ,IOH,0)
  999 RETURN
      END

      SUBROUTINE SET_SPLIT_SCREEN(INUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set number of lines in upper part of split
C-                         screen. The actual split is done in SPLTIT.
C-
C-   Inputs  : INUM: Number of lines in upper part of screen
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   modified   29-OCT-1990   Scott Snyder
C-    don't give an error if INUM = SPLLIN and things are otherwise ok.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER INUM,LOCMAX
      CHARACTER*4 BUF
C----------------------------------------------------------------------
      IF(STAFLG) THEN
        LOCMAX=PBSAVE-STALIN-7
      ELSE
        LOCMAX=PBSAVE-7
      ENDIF
      IF(INUM.GE.3.AND.INUM.LE.LOCMAX) THEN
        IF (INUM .NE. SPLLIN) THEN
          SPLLIN=INUM
          IF(SPLFLG) THEN
            CALL ENDSPL
            CALL SPLTIT
          ENDIF
        ENDIF
      ELSE
        WRITE(BUF,100) LOCMAX
  100   FORMAT(I2,'  ')
        CALL OUTMSG('0Invalid number for split mode, should be 3--'
     &     //BUF//CHAR(7))
        CALL OUTMSG(' ')
      ENDIF
      RETURN
      END

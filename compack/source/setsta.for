      SUBROUTINE SETSTA(INUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set number of lines in MESSAGE display at top of
C-                         screen. The actual creation of display is done in
C-                         SPLSTA.
C-
C-   Inputs  : INUM: Number of lines in STATUS part of screen
C-   Outputs : None
C-   Controls: None
C-
C-   Created   21-OCT-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER INUM,MAXSTA
      CHARACTER*4 BUF
      DATA MAXSTA/5/
C----------------------------------------------------------------------
      IF(INUM.NE.STALIN) THEN
        IF(INUM.GE.3.AND.INUM.LE.MAXSTA) THEN
          STALIN=INUM
          IF(.NOT.SPLFLG.AND.(PBSAVE-SPLLIN-STALIN).LT.7) THEN
            SPLLIN=PBSAVE-STALIN-8     ! Make sure there is room left for
C                                     ! menu display
          ENDIF
          IF(STAFLG) THEN
            CALL ENDSTA
            CALL SPLSTA
          ENDIF
        ELSE
          WRITE(BUF,100) MAXSTA
  100     FORMAT(I1,'   ')
          CALL OUTMSG('0Invalid number for status display mode,'//
     &         ' should be 3--'//BUF//CHAR(7))
          CALL OUTMSG(' ')
        ENDIF
      ENDIF
      RETURN
      END

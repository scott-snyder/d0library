      SUBROUTINE SPLSTA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up STATUS part of screen
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  21-OCT-1988   Jan S. Hoftun   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER ISTAT,STASAV,LIBUNS
      INTEGER LIBSTA,LIBSCR
C----------------------------------------------------------------------
      IF((.NOT.STAFLG.OR.STASAV.NE.STALIN).AND.SMGON) THEN
        IF(STALIN.GT.0) THEN
          IF(STAFLG.AND.STALIN.NE.STASAV) THEN
            ISTAT=LIBUNS()
          ENDIF
          STASAV=STALIN
          ISTAT=LIBSTA()            ! Paste status display
          IF(MOD(ISTAT,2).EQ.0) THEN
            CALL MSGSCR(ISTAT,'LIBSTA-->')
          ELSE
            STAFLG=.TRUE.
            IF(FULSCR) THEN
              ISTAT=LIBSCR(2,PBROWS-1)
            ELSE
              ISTAT=LIBSCR(1,PBROWS)
            ENDIF
          ENDIF
          POS=1
        ENDIF
      ENDIF
      RETURN
      END

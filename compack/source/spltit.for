      SUBROUTINE SPLTIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up split screen mode
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
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
      INTEGER ISTAT,SPLSAV,LIBUNP
      INTEGER LIBPST,LIBSCR
C----------------------------------------------------------------------
      IF((.NOT.SPLFLG.OR.SPLSAV.NE.SPLLIN).AND.SMGON) THEN
        IF(SPLLIN.GT.0) THEN
          IF(SPLFLG.AND.SPLLIN.NE.SPLSAV) THEN
            ISTAT=LIBUNP()
          ENDIF
          SPLSAV=SPLLIN
          ISTAT=LIBPST()                                   ! Paste second display
          IF(ISTAT.NE.1) THEN
            CALL MSGSCR(ISTAT,'LIBPST-->')    ! 1 for successful terminal connection
          ELSE
            SPLFLG=.TRUE.
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

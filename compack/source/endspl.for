      SUBROUTINE ENDSPL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End split screen mode.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: SPLFLG is set to .FALSE.
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER ISTAT,LIBUNP,LIBSCR
C----------------------------------------------------------------------
      IF(SPLFLG) THEN
         SPLFLG=.FALSE.
         SAVSPL=.FALSE.
         ISTAT=LIBUNP()                     ! Take away second display and restore main display
         IF(FULSCR) THEN
           ISTAT=LIBSCR(3,PBROWS-2)
         ELSE
           ISTAT=LIBSCR(1,PBROWS)
         ENDIF
      ENDIF
      RETURN
      END

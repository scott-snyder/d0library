      SUBROUTINE ENDSTA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Take away STATUS screen.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: STAFLG is set to .FALSE.
C-
C-   Created  21-OCT-1988   Jan S. Hoftun   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER ISTAT,LIBUNS,LIBSCR
C----------------------------------------------------------------------
      IF(STAFLG) THEN
         STAFLG=.FALSE.
         ISTAT=LIBUNS()                     ! Take away STATUS display 
         IF(FULSCR) THEN
           ISTAT=LIBSCR(3,PBROWS-2)
         ELSE
           ISTAT=LIBSCR(1,PBROWS)
         ENDIF
      ENDIF
      RETURN
      END

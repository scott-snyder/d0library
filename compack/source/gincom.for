      SUBROUTINE GINCOM(COMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get possible direct command entered
C-
C-   Inputs  : COMAND: ORIGINAL command line entered
C-   Outputs : None
C-   Controls: None
C-
C-   Created  25-MAY-1989   Jan S. Hoftun (pulled form SETCHK)
C-
C----------------------------------------------------------------------
C&IF VAXVMS
      IMPLICIT NONE
      CHARACTER*(*) COMAND
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INTEGER ISTAT,CLI$PRESENT,CLI$GET_VALUE,I
      CHARACTER INCOM*128
      EXTERNAL CLI$_PRESENT
C----------------------------------------------------------------------
      ISTAT=CLI$PRESENT('INCOM1')
      IF(ISTAT.EQ.%LOC(CLI$_PRESENT)) THEN
        ISTAT=CLI$GET_VALUE('INCOM1', INCOM )
        IF(COMAND(1:1).EQ.'/') THEN
          I=INDEX(COMAND,' ')+1
          COMSAV=COMAND(I:)
        ELSE
          COMSAV=COMAND
        ENDIF
        ONEFLG=.TRUE.
        SMGON=.FALSE.
      ENDIF
C&ENDIF
  999 RETURN
      END

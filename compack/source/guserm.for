      SUBROUTINE GUSERM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get /USER qualifier parts
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  25-MAY-1989   Jan S. Hoftun (pulled form SETCHK)
C-
C----------------------------------------------------------------------
C&IF VAXVMS
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INTEGER ISTAT,CLI$PRESENT,CLI$GET_VALUE,I,USRLEN(64),USRNUM
      CHARACTER*64 USRCHR(64)
      EXTERNAL CLI$_PRESENT,CLI$_ABSENT
C----------------------------------------------------------------------
      ISTAT=CLI$PRESENT('USER')
      IF(ISTAT.EQ.%LOC(CLI$_PRESENT)) THEN
        ISTAT=0
        USRNUM=0
        DO WHILE (ISTAT.NE.%LOC(CLI$_ABSENT).AND.ISTAT.NE.1)
          USRNUM=USRNUM+1
          ISTAT=CLI$GET_VALUE('USER',USRCHR(USRNUM),USRLEN(USRNUM))
        ENDDO
        CALL USRCHK(USRNUM,USRCHR)
      ENDIF
C&ENDIF
  999 RETURN
      END

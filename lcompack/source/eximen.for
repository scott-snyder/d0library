      SUBROUTINE EXIMEN(ISTAT,ILEV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Exit from a COMPACK program
C-
C-   Inputs  : ISTAT: Status to be reported at exit
C-             ILEV:  Level of menu exiting from (not used).
C-   Outputs : None
C-   Controls: None
C-
C-   Modified 22-SEP-1988   Jan S. Hoftun
C-   Updated  30-SEP-1991   Herbert Greenlee
C-     Added calls to smg$delete_virtual_keyboard and smg$delete_pasteboard.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ISTAT,ILEV
C
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER STAT
      INTEGER TRULEN,FROBAC,I,IBEG,J
      CHARACTER*64 BLNK
      CHARACTER*132 MSGLIN
      DATA BLNK/' '/
C----------------------------------------------------------------------
      IF(.NOT.ONEFLG.AND.ENDFLG) THEN
        J=TRULEN(TOPLIN(0,MAILEV))
        IBEG=1
        DO I=1,J
          IF(TOPLIN(0,MAILEV)(I:I).NE.' ') THEN
            GOTO 90
          ELSE
            IBEG=I+1
          ENDIF
        ENDDO
   90   CONTINUE
        IF(MAILEV.GT.0) THEN
          CALL OUTMSG('0'//'Exit from---> '//TOPLIN(0,MAILEV)(IBEG:J))
          CALL OUTMSG(' ')
        ENDIF
      ENDIF
      CALL EXIUSR
      STOP
      END

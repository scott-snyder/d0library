      SUBROUTINE ABOMEN(ISTAT,EXVAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Abort a program if ISTAT is even (without
C-                         traceback).
C-
C-   Inputs  : ISTAT: Status to report and take action on.
C-             EXVAR: String to be used to indicate where error ocurred.
C-   Outputs : None
C-   Controls:
C-
C-   Created   1-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ISTAT
      CHARACTER*(*) EXVAR
      CHARACTER*132 CTEMP
C
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
C&IF VAXVMS
      INTEGER STAT,LIBSCR,LIBERA,LIBPUT
      INTEGER LIB$SIGNAL,LIB$ESTABLISH,IS
      EXTERNAL NULHDL
C----------------------------------------------------------------------
      IS=LIB$ESTABLISH(NULHDL)  ! Establish error handler without traceback
      IF(FULSCR) THEN
        STAT=LIBSCR(1,PBROWS)                          ! Reset scroll-region
        STAT=LIBPUT(' ',PBROWS,1,0)
        STAT=LIBERA(1,1)
      ENDIF
      CALL EXIUSR
      CALL OUTMSG(' ')                                  ! Add blank line
      STAT=LIB$SIGNAL(%VAL(ISTAT),%VAL(1),EXVAR//CHAR(7))
      CALL OUTMSG(' ')                                  ! Add blank line
      CALL EXIT(1)
C&ELSE
C&      CTEMP = '0Error from: '//EXVAR(1:LEN(EXVAR))//CHAR(7)
C&      CALL INTMSG(CTEMP)
C&      CALL INTMSG(' ')
C&      STOP
C&ENDIF
      END

      SUBROUTINE RESTAT(TIMAST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set timer for TIMDIS type looping display
C-                         VAX-specific
C-
C-   Inputs  : TIMAST: AST routine to be called at the end of time interval
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL TIMAST
C&IF VAXVMS
      EXTERNAL STADIS
      INCLUDE 'D0$INC:TIMSTA.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      LOGICAL GETDEV,FULMOD
      INTEGER ISTAT,SYS$SETIMR,COUNT,J,LIBPUT,TRULEN,LIBERL,LIBCUR,
     &        CURONF
      CHARACTER*132 OUTSTR
C----------------------------------------------------------------------
      IF(DISPOK) THEN
        IF(FULMOD()) THEN
          COUNT=COUNT+1
          J=MOD(COUNT,2)*10
          WRITE(OUTSTR,101)
  101     FORMAT(<J>X,' ALIVE',<PBCOLS-J-5>X)
          IF(J.EQ.0) THEN
            ISTAT=LIBERL(PBROWS-2,1)    ! Clean up any error messages
          ENDIF
          IF(PBROWS.GT.15) THEN
            ISTAT=LIBPUT(OUTSTR(1:16),PBROWS-2,2,1)
          ELSE
            ISTAT=LIBPUT(OUTSTR(1:16),PBROWS-1,2,1)
          ENDIF
          ISTAT=LIBCUR(3,1)
          ISTAT=CURONF(1)
        ELSEIF(GETDEV()) THEN
          CALL OUTMSG('0Hit PF4 to end display')
          ISTAT=LIBCUR(2,1)
        ELSE
          CALL OUTMSG('0'//PRMTIM(1:TRULEN(PRMTIM)))
        ENDIF
        ISTAT=SYS$SETIMR(,BIN_TIME,STADIS,TIMAST)
      ENDIF
C&ENDIF
      RETURN
      END

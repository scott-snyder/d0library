      SUBROUTINE DOPROC(COMAND,COMOUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Parse a command string and possibly dispatch
C-                         to system task to perform it.
C-
C-   Inputs  : COMAND: String to be parsed
C-   Outputs : COMOUT: Unique command identifier returned to dispatch loop
C-   Controls: None
C-
C-   Documneted 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) COMAND,COMOUT
C
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER LIBBIG,READPF,LSPAWN,TRULEN,LIBCUR
      INTEGER ISTAT,LIBREP
      CHARACTER*80 MSGLIN,TRANUP
C----------------------------------------------------------------------
      POS=0
      CALL PRSCOM(COMAND)
      IF(POS.GT.0) THEN
        IF((SETUP.OR.LOGUP).AND..NOT.ASTFLG) THEN
          IF(COMLIN(POS,CURLEV).NE.'CALL RUNLOG'.AND.
     *        COMLIN(POS,CURLEV).NE.'CALL RUNSET') THEN
            IF(COMLIN(POS,CURLEV).NE.'CALL ENDLOG'.AND.
     *            COMLIN(POS,CURLEV).NE.'CALL RUNCOM') THEN
              WRITE(COMUNI,9990) COMPRT(0)(1:TRULEN(COMPRT(0)))
 9990         FORMAT(A)
            ENDIF
          ELSE
            CALL INTMSG(' Command files CANNOT be nested!'//CHAR(7))
            PF=0
            GOTO 8000
          ENDIF
        ENDIF
C
C    ONLY go to SYSDSP routine when a 'CALL' is found in the command identifier
C
        IF(INDEX(COMLIN(POS,CURLEV),'CALL').GT.0) THEN
          PF=0
          CALL SYSDSP(COMLIN(POS,CURLEV))
          IF(.NOT.TOPGO.AND..NOT.ASTFLG.AND.PF.NE.4)
     *          CALL MENDIS(.TRUE.)
        ELSEIF(INDEX(COMLIN(POS,CURLEV),'@SUBSET').GT.0) THEN
          IF(ASTFLG) CALL DISABL       !Disable unsolicited input before spawn
          !(SPAWN WON't work otherwise)
          ISTAT=LSPAWN('@D0$COMPACK:SUBSET','SYS$COMMAND')
          IF(MOD(ISTAT,2).EQ.0) THEN
            CALL MSGSCR(ISTAT,'SPAWN FAILED-->')
            CALL PFWAIT
          ENDIF
          IF(.NOT.ASTFLG) CALL MENDIS(.TRUE.)
          PF=4
        ELSE
          COMOUT=COMLIN(POS,CURLEV)
          PF=1
        ENDIF
      ELSEIF(PF.EQ.1) THEN
        IF(NXTLEV.EQ.0) THEN
          CALL OUTMSG('0Unimplemented command: '//
     &      TRANUP(COMPRT(0)(1:TRULEN(COMPRT(0))))//CHAR(7))
        ENDIF
        PF=0
      ENDIF
 8000 CONTINUE
      RETURN
      END

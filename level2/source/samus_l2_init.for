      SUBROUTINE SAMUS_L2_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dummy first pass at TOOL_INIT
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   15-SEP-1992   Oleg Eroshin
C-   Modified  21-Oct-1992   Diehl
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INCLUDE 'D0$LINKS:IZ2SSAM.LINK'   
c      INCLUDE 'IZ2SSAM.LINK'   
C----------------------------------------------------------------------
      CHARACTER*64  ERROR_MESSAGE
      CHARACTER*32  FILENAM,BANKN(20)
      INTEGER       IER,LUN,NBANKS,IUSER
      INTEGER       LSL2H,GZSL2H,RECSIZ
      CHARACTER*4   CHOPT
      LOGICAL       OK
C
      DATA FILENAM  /'SAMUS_L2_RCP'/    
      DATA IUSER    /963/
C----------------------------------------------------------------------
C
      CALL SAMUS_CONSTANTS
C
      CALL GTUNIT(IUSER,LUN,IER)
      CHOPT = 'IF'
      CALL D0OPEN(LUN,FILENAM,CHOPT,OK)
      IF(.NOT.OK) THEN
        ERROR_MESSAGE = ' SAMUS_L2_INIT: SAMUS_L2_RCP NOT FOUND '
        GO TO 999
      END IF
C
      RECSIZ = 8  !squash out comments
      CALL EZMAKE(LUN,RECSIZ,BANKN,NBANKS)
      CALL EZERR (IER)
      IF(IER.NE.0) THEN
        ERROR_MESSAGE = 'SAMUS_L2_RCP READ FAILED'
        GOTO 999
      ENDIF
C
c      CALL EZCHAIN(BANKN,NBANKS)            !NEW
c      CALL EZERR  (IER)
c      IF(IER.NE.0) THEN
c        ERROR_MESSAGE = ' FAILED TO EZCHAIN RCP BANKS TOGETHER.'
c        GOTO 999
c      ENDIF

c      LSL2H = GZSL2H()                      !NEW
c      CALL EZMOVE(BANKN(1),LSL2H,IZSAMUS_L2_RCP)
c      CALL EZERR(IER)
c      IF (IER.NE.0) THEN
c        ERROR_MESSAGE = ' RCP CHAIN MOVE FAILED.'
c        GOTO 999
c      ENDIF

      call l2j_rcp_chain(nbanks,bankn,ier)
      IF (IER.NE.0) THEN
        ERROR_MESSAGE=' L2J RCP CHAIN Failed. Call Rich A.'
        GOTO 999
      ENDIF

      CLOSE(LUN)
      CALL RLUNIT(IUSER,LUN,IER)
C
      IF(IER.NE.0) THEN
        ERROR_MESSAGE = 'SAMUS_L2.RCP FAILED TO CLOSE'
        GOTO 999
      ENDIF
C
      RETURN
C----------------------------------------------------------------------
  999 CONTINUE
      CALL ERRMSG('SAMUS_L2','SAMUS_L2_INIT',ERROR_MESSAGE,'W')
      RETURN
      END

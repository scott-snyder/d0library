      SUBROUTINE MUON_L2_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dummy first pass at TOOL_INIT
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   9-NOV-90   by the L2STATE program
C-   MODIFIED  9-NOV-90   DIEHL
C-            17-DEC-1990 NOW USES LSL2H TREE. (HTD)
C-            08-NOV-92   Add call to L2_VERT_INIT (HTD)
C-            08-DEC-92   Add call to SAMUS_L2_INIT (HTD)
C----------------------------------------------------------------------
      IMPLICIT NONE

      CHARACTER*32 FILENAM,BANKN(20),ERROR_MESSAGE
      INTEGER IER,LUN,NBANKS,IUSER
      INTEGER LSL2H,GZSL2H,RECSIZ
      LOGICAL INIT_OK
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FILTER_COM.INC'

      INCLUDE 'D0$INC:MUON_L2_PARAMS.INC'
      INCLUDE 'D0$LINKS:IZMUON_L2_RCP.LINK'
      INCLUDE 'D0$LINKS:IZ2SMUO.LINK'
      CHARACTER*4 CHOPT
      LOGICAL OK
C      EXTERNAL MUON_L2

      DATA FILENAM/'MUON_L2_RCP'/
      DATA INIT_OK/.TRUE./
      DATA IUSER/908/
C----------------------------------------------------------------------

      CALL MUON_CONSTANTS
      CALL L2_VERT_INIT
      CALL SAMUS_L2_INIT

      CALL GTUNIT(IUSER,LUN,IER)
      CHOPT = 'IF'
      CALL D0OPEN(LUN,FILENAM,CHOPT,OK)
      IF(.NOT.OK) THEN
        ERROR_MESSAGE=' MUON_L2_INIT: MUON_L2_RCP NOT FOUND '
        GO TO 900
      END IF
      RECSIZ = 8  !squash out comments
      CALL EZMAKE(LUN,RECSIZ,BANKN,NBANKS)
      CALL EZERR(IER)
      IF(IER.NE.0) THEN
        ERROR_MESSAGE='MUON_L2_RCP READ FAILED'
        GOTO 900
      ENDIF

      CALL EZCHAIN(BANKN,NBANKS)            !NEW
      CALL EZERR(IER)
      IF(IER.NE.0) THEN
        ERROR_MESSAGE=' FAILED TO EZCHAIN RCP BANKS TOGETHER.'
        GOTO 900
      ENDIF

      LSL2H = GZSL2H()                      !NEW
      CALL EZSHUNT(BANKN(1),LSL2H,IZMUON_L2_RCP)
      CALL EZERR(IER)
      IF (IER.NE.0) THEN
        ERROR_MESSAGE=' RCP CHAIN MOVE FAILED.'
        GOTO 900
      ENDIF

      CLOSE(LUN)
      CALL RLUNIT(IUSER,LUN,IER)

      IF(IER.NE.0) THEN
        ERROR_MESSAGE='MUON_L2.RCP FAILED TO CLOSE'
        GOTO 900
      ENDIF
C----------------------------------------------------------------------

  999 RETURN

  900 CONTINUE
      CALL ERRMSG('MUON_L2','MUON_L2_INIT',ERROR_MESSAGE,'W')
      INIT_OK=.FALSE.
      RETURN

      END

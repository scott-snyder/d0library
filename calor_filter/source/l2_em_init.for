      SUBROUTINE L2_EM_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in PARAMETRS if you want
C-
C-   Inputs  :
C-
C-   Outputs :
C-   Controls:
C-
C-   Created 15-SEP-1989   Yi  Xia
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'       ! ZEBSTP common include file
      INCLUDE 'D0$INC:ZEBCOM.INC'       ! zebra main store /ZEBCOM/
      INCLUDE 'D0$INC:ZLINKC.INC'       ! zebra link area to store links
      INCLUDE 'D0$INC:L2JETS_CHARS.INC' ! error messages
      INCLUDE 'D0$LINKS:IZL2ELECTRON_RCP.LINK'
      CHARACTER*32 BANKN(20)            ! names of RCP banks read in
      CHARACTER*50 FILENAM              ! RCP file name
      INTEGER NBANKS,LUN,IER,LSL2H,GZSL2H,RECSIZ
      LOGICAL OK,INIT_OK
      INTEGER L2EL_USER
      PARAMETER( L2EL_USER = 9873 )
      LOGICAL  CL2PA
C
C----------------------------------------------------------------------
C
      IF (LSTPH .LE. 0) CALL INZSTP
      IF (LSTPH .LE. 0) THEN
        NOWERRMESS = ' Cannot initialize ZEBSTP common '
        NOWSMESS   = 'Init fail'
        GOTO 900
      END IF
C
      FILENAM = 'L2_EM_RCP'
C---Open file. Read it in with RCP and close again...
      CALL GTUNIT(L2EL_USER,LUN,IER)
      IF (IER .NE. 0) THEN
        NOWERRMESS = ' GTUNIT failure'
        NOWSMESS   = 'RCP fail'
        GOTO 900
      END IF
      CALL D0OPEN(LUN,FILENAM,'IF',OK)
      IF (.NOT. OK) THEN
        NOWERRMESS = ' RCP file open failed '
        NOWSMESS   = 'RCP fail'
        GOTO 900
      END IF
C
      CALL BKSL2H(LSL2H)
      IF (LSL2H .LE. 0) THEN
        NOWERRMESS = ' Unable to book SL2H'
        NOWSMESS   = 'SL2H fail'
        GOTO 900
      END IF
      RECSIZ = 8  !squash out comments
      CALL EZMAKE(LUN,RECSIZ,BANKN,NBANKS)   ! Read in the banks
      CALL EZERR(IER)
      IF (IER .NE. 0) THEN
        NOWERRMESS = ' RCP read failed '
        NOWSMESS   = 'RCP fail'
        GOTO 900
      END IF
C
      CALL EZPICK_NOMSG('L2_EM_TEXT_RCP',IER)
      IF (IER.EQ.0) THEN
        CALL EZDROP('L2_EM_TEXT_RCP') !drop description of how bank made
        NBANKS = NBANKS - 1           !only works IF this bank is last in file
        CALL EZRSET
      ENDIF
      CALL L2J_RCP_CHAIN( NBANKS, BANKN, IER )
      IF (IER .NE. 0) THEN
        NOWERRMESS = ' L2J_RCP_CHAIN failure'
        NOWSMESS   = 'RCP fail'
        GOTO 900
      END IF
C
      CLOSE(LUN)                        ! Close RCP file
C
      CALL RLUNIT(L2EL_USER,LUN,IER)  ! Release unit
      IF (IER .NE. 0) THEN
        NOWERRMESS = ' RLUNIT failure'
        NOWSMESS   = 'RCP fail'
        GOTO 900
      END IF
C
      CALL L2_CD_MATCH_INIT
C
      CALL CL2_MAKE_TABLES
      INIT_OK = .TRUE.
C
      GOTO 999                          ! Return
  900 CONTINUE                          ! Error
      CALL ERRMSG(NOWSMESS,'L2_EM_INIT',NOWERRMESS,'W')
      INIT_OK = .FALSE.
  999 RETURN
      END

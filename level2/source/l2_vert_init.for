      SUBROUTINE L2_VERT_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : prepare downloaded constants for L2_VERT
C-
C-   Inputs  : RCP files, perhaps also STP and DBL3 files
C-   Outputs : banks under SL2H
C-   Controls:
C-
C-   Created  19-OCT-1992   James T. Linnemann
C-   Modified 09-NOV-1993   Dan Claes - read L2_VERT_RCP into STP
C-                                      and support ERRMSG reporting
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'       ! ZEBSTP common include file
C
C For error message routine
C
      CHARACTER*80 NOWERRMESS           ! Error message
      CHARACTER*16 NOWSMESS             ! short description
      CHARACTER*32 NOWERRID             ! routine that errored
C
      CHARACTER*32 BANKN(20)            ! names of RCP banks read in
      CHARACTER*50 FILENAM              ! RCP file name
      INTEGER NBANKS,LUN,IER,LSL2H,GZSL2H,RECSIZ
      LOGICAL OK
C
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL L2_L0_INIT
C
C vvvvvvvvvvvvvv D.Claes vvvvvvvvvvvvvv
C
C---STP should be initialized by LEVEL2
        IF (LSTPH .LE. 0) CALL INZSTP
        IF (LSTPH .LE. 0) THEN
          NOWERRMESS = ' Cannot initialize ZEBSTP common '
          NOWSMESS   = 'Init fail'
          GOTO 900
        END IF
C
C---Read from RCP file:
        FILENAM = 'L2_VERT_RCP'
C
C---Open file. Read it in with RCP and close again...
        CALL GTUNIT(987,LUN,IER)
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
        RECSIZ = 8  !squash out comments
        CALL EZMAKE(LUN,RECSIZ,BANKN,NBANKS)   ! Read in the banks
        CALL EZERR(IER)
        IF (IER .NE. 0) THEN
          NOWERRMESS = ' RCP read failed '
          NOWSMESS   = 'RCP fail'
          GOTO 900
        END IF
C
C Add this to all other SRCP banks to be downloaded together
C
        CALL L2J_RCP_CHAIN( NBANKS, BANKN, IER )
        IF (IER .NE. 0) THEN
          NOWERRMESS = ' L2J_RCP_CHAIN failure'
          NOWSMESS   = 'RCP fail'
          GOTO 900
        END IF
C
        CLOSE(LUN)                      ! Close RCP file
        CALL RLUNIT(987,LUN,IER)  ! Release unit
        IF (IER .NE. 0) THEN
          NOWERRMESS = ' RLUNIT failure'
          NOWSMESS   = 'RCP fail'
          GOTO 900
        END IF
C
      ENDIF
C
      GOTO 999                          ! Return
  900 CONTINUE                          ! Error
      CALL ERRMSG(NOWSMESS,'L2_VERT_INIT',NOWERRMESS,'W')
  999 RETURN
      END

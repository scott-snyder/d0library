      SUBROUTINE L2_CD_MATCH_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Make STP structure to hold constants and
C-                        parameters to be used by LEVEL2 tracking
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: INIT_OK will be set .FALSE. if have any problems
C-
C-   Created   01-NOV-1991  D Claes
C-   Modified  01-MAY-1993  D Claes add VERTEX_RCP for L2_VERTEX_CDC tool
C-                                  rewrite using L2J_RCP_CHAIN to add all
C-                                  SRCPs together under a single bank
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'       ! ZEBSTP common include file
C
C For error message routine
C
      CHARACTER*80 NOWERRMESS           ! Error message
      CHARACTER*16 NOWSMESS             ! short description
C
      CHARACTER*32 BANKN(20)            ! names of RCP banks read in
      CHARACTER*50 FILENAM              ! RCP file name
      INTEGER NBANKS, LUN, IER, L2EL_USER,RECSIZ
C     INTEGER GZSL2H, L2CDC, LSL2H
      LOGICAL OK
      PARAMETER ( L2EL_USER = 9873 )
C----------------------------------------------------------------------
      LOGICAL INIT_OK
      COMMON /L2TRAK_CRTL/ INIT_OK
C
C Link offset of the bank, L2TRAK, within the mother bank, LSL2H
      INTEGER   IZL2TRAK_RCP
      PARAMETER ( IZL2TRAK_RCP = 10 )
C----------------------------------------------------------------------
C
C---The purpose of this routine is to put all the parameters and constants
C---we need into the Static Paramameters common block (ZEBSTP) in one form
C---or another.
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
      FILENAM = 'L2TRAK_RCP'
C
   50 CONTINUE
C
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
      CALL RLUNIT(L2EL_USER,LUN,IER)  ! Release unit
      IF (IER .NE. 0) THEN
        NOWERRMESS = ' RLUNIT failure'
        NOWSMESS   = 'RCP fail'
        GOTO 900
      END IF
C
      IF (FILENAM.EQ.'L2TRAK_RCP') THEN
        FILENAM = 'VERTEX_RCP'
        GOTO 50
      ENDIF
C
C      CALL BKSL2H(LSL2H)                ! Book SL2H header
C      IF (LSL2H .LE. 0) THEN
C        NOWERRMESS = ' Unable to book SL2H'
C        NOWSMESS   = 'SL2H fail'
C        GOTO 900
C      END IF
C
C      CALL EZCHAIN(BANKN,NBANKS)        ! Chain them together under #1
C      CALL EZERR(IER)
C      IF (IER .NE. 0) THEN
C        NOWERRMESS = ' Failed to EZCHAIN rcp banks together '
C        NOWSMESS   = 'RCP fail'
C        GOTO 900
C      END IF
C      LSL2H = GZSL2H()
C      CALL EZSHUNT(BANKN(1),LSL2H,IZL2TRAK_RCP) ! Move it here
C      CALL EZERR(IER)
C      IF (IER .NE. 0) THEN
C        NOWERRMESS = ' RCP chain move failed '
C        NOWSMESS   = 'RCP fail'
C        GOTO 900
C      END IF
C
C
      CALL L2_CDINIT
C
      CALL L2TRAK_STP
C
      CALL L2_FDCINIT
C
      CALL L2FDC_STP
C
      CALL L2CDHT_INIT
C
C--- If we made it here, then everything is okay so far.
C--- Call for a survey to be performed of these added banks.
C
C      LSL2H = GZSL2H()
C      L2CDC = LC(LSL2H-9)        ! see if already there
C      CALL DZSURV('Survey of banks added for L2 Tracking',IXSTP,L2CDC)
C
      GOTO 999                          ! Return
  900 CONTINUE                          ! Error
      CALL ERRMSG(NOWSMESS,'L2TRAK_INIT',NOWERRMESS,'W')
      INIT_OK = .FALSE.
  999 RETURN
      END

      SUBROUTINE L2_L0_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Make STP structure to hold constants and
C-                        parameters to be used by L2 Level 0.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: INIT_OK will be set .FALSE. if have any problems
C-
C-   Created   6-MAY-1990   Richard V. Astur
C-   Modified  9-JUN-1991 RA: Eliminate calls to EOPEN,ECLOSE
C-   Updated  14-OCT-1992   Jeffrey Bantly, James T. Linnemann   modify for L0
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'       ! ZEBSTP common include file
      CHARACTER*32 BANKN(20)            ! names of RCP banks read in
      CHARACTER*50 FILENAM              ! RCP file name
      INTEGER NBANKS,LUN,IER,LSL2H,GZSL2H,L2_L0_USER,RECSIZ
      PARAMETER( L2_L0_USER = 431037 )
      LOGICAL OK,EZERR
C----------------------------------------------------------------------
C---The purpose of this routine is to put all the parameters and constants
C---we need into the Static Paramameters common block (ZEBSTP) in one form
C---or another.
C
C---Level2 should have initialized STP for us, but we will check and do it
C---if not.
      IF (LSTPH .LE. 0) CALL INZSTP
      IF (LSTPH .LE. 0) THEN
        CALL ERRMSG('ZEBSTP_INIT_FAIL','L2_L0_INIT', 
     &    ' Cannot initialize ZEBSTP common ','F')
      END IF
C
C---Read from RCP file:
      FILENAM = 'L2_L0_RCP'
C---Open file. Read it in with RCP and close again...
      CALL GTUNIT(L2_L0_USER,LUN,IER)
      IF (IER .NE. 0) THEN
        CALL ERRMSG('GTUNIT_FAIL','L2_L0_INIT', 
     &    ' Cannot get unit number from GTUNIT','F')
      END IF
      CALL D0OPEN(LUN,FILENAM,'IF',OK)
      IF (.NOT. OK) THEN
        CALL ERRMSG('RCP open Fail','L2_L0_INIT',
     & ' L2_L0_RCP file open failed ','F')
      END IF
C---When we read in the RCP banks: L2_L0
C---They will be in the ZEBSTP common. This structure will then be
C---dumped to a FZ file. This file is downloaded to Level2 where it
C---is read back into the FZ structure. But RCP keeps a list of
C---RCP banks in memory and this is lost when the Zebra structure is dumped.
C---So we must hang these banks in pre-arranged places.
C
C---Plan is to read in RCP bank, chain them together and hang them below
C---the L2 STP header (SL2H)
      CALL BKSL2H(LSL2H)                ! Book L2_L0 header
      IF (LSL2H .LE. 0) THEN
        CALL ERRMSG('SL2H book Fail','L2_L0_INIT',
     & ' Could not book SL2H bank','F')
      END IF
      RECSIZ = 8  !squash out comments
      CALL EZMAKE(LUN,RECSIZ,BANKN,NBANKS)   ! Read in the banks
      IF (EZERR(IER)) THEN
        CALL ERRMSG('RCP read Fail','L2_L0_INIT',
     & ' L2_L0_RCP read failed ','F')
      END IF

C
C---Add them to whatever other SRCP banks are being downloaded
C
      CALL L2J_RCP_CHAIN( NBANKS, BANKN, IER )
      IF (IER .NE. 0) THEN
        CALL ERRMSG('RCP Chain Fail','L2_L0_INIT',
     &    ' L2J_RCP_CHAIN failure','F')
      END IF
C
      CLOSE(LUN)                        ! Close RCP file
      CALL RLUNIT(L2_L0_USER,LUN,IER)  ! Release unit
      IF (IER .NE. 0) THEN
        CALL ERRMSG('RLUNIT_FAIL','L2_L0_INIT', 
     &    ' Cannot release unit number from RLUNIT','F')
      END IF
  999 RETURN
      END

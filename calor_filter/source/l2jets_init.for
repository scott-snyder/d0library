      SUBROUTINE L2JETS_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Make STP structure to hold constants and
C-                        parameters to be used by L2 Jets
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: INIT_OK will be set .FALSE. if have any problems
C-
C-   Created   6-MAY-1990   Richard V. Astur
C-   Modified  9-JUN-1991 RA: Eliminate calls to EOPEN,ECLOSE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'       ! ZEBSTP common include file
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$INC:L2JETS_CONT.INC'  ! L2JETS control file
      INCLUDE 'D0$INC:L2JETS_CHARS.INC'         ! Character variables
      CHARACTER*32 BANKN(20)            ! names of RCP banks read in
      CHARACTER*50 FILENAM              ! RCP file name
      INTEGER NBANKS,LUN,IER,LSL2H,GZSL2H,RECSIZ
      LOGICAL FIRST,OK,EZERR
C      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C--- This routine is supposed to replace CALOR_INIT which was intended to
C--- be called once to read in calorimeter parameters. But it also could
C--- be called again when new parameters were needed. In this spirit, I
C--- will allow this routine to be called more than once.
CCCC      IF (.NOT. FIRST) RETURN            ! Do this routine only once

C---The purpose of this routine is to put all the parameters and constants
C---we need into the Static Paramameters common block (ZEBSTP) in one form
C---or another.

C---Level2 should have initialized STP for us, but we will check and do it
C---if not.
      IF (LSTPH .LE. 0) CALL INZSTP
      IF (LSTPH .LE. 0) THEN
        NOWERRMESS = ' Cannot initialize ZEBSTP common '
        NOWSMESS   = 'Init fail'
        GOTO 900
      END IF

C---Read from RCP file:
      FILENAM = 'L2JETS_RCP'
C---Open file. Read it in with RCP and close again...
      CALL GTUNIT(L2JETS_USER,LUN,IER)
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
C---When we read in the RCP banks: L2JETS_CONTROL,L2JETS_TRGR,L2JETS
C---They will be in the ZEBSTP common. This structure will then be
C---dumped to a FZ file. This file is downloaded to Level2 where it
C---is read back into the FZ structure. But RCP keeps a list of
C---RCP banks in memory and this is lost when the Zebra structure is dumped.
C---So we must hang these banks in pre-arranged places.
C
C---Plan is to read in RCP bank, chain them together and hang them below
C---the L2 STP header (SL2H)
      CALL BKSL2H(LSL2H)                ! Book L2JETS header
      IF (LSL2H .LE. 0) THEN
        NOWERRMESS = ' Unable to book SL2H'
        NOWSMESS   = 'SL2H fail'
        GOTO 900
      END IF
      RECSIZ = 8  !squash out comments
      CALL EZMAKE(LUN,RECSIZ,BANKN,NBANKS)   ! Read in the banks
      IF (EZERR(IER)) THEN
        NOWERRMESS = ' RCP read failed '
        NOWSMESS   = 'RCP fail'
        GOTO 900
      END IF

C
C---Add them to whatever other SRCP banks are being downloaded
C
      CALL L2J_RCP_CHAIN( NBANKS, BANKN, IER )
      IF (IER .NE. 0) THEN
        NOWERRMESS = ' L2J_RCP_CHAIN failure'
        NOWSMESS   = 'RCP fail'
        GOTO 900
      END IF

C
      CLOSE(LUN)                        ! Close RCP file
      CALL RLUNIT(L2JETS_USER,LUN,IER)  ! Release unit
      IF (IER .NE. 0) THEN
        NOWERRMESS = ' RLUNIT failure'
        NOWSMESS   = 'RCP fail'
        GOTO 900
      END IF
C
C---Do the init for the fast unpack: CL2 routines.
C
      CALL CL2_MAKE_TABLES

C--- If we made it here, then everything is okay so far.
      GOTO 999                          ! Return
  900 CONTINUE                          ! Error
      CALL ERRMSG(NOWSMESS,'L2JETS_INIT',NOWERRMESS,'W')
      INIT_OK = .FALSE.
  999 RETURN
      END

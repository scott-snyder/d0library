      SUBROUTINE L2CRCAL_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make STP file for L2CRCAL tool
C-                      Basically construct L2CR bank. See L2CR.ZEB
C-                      and D0$INC:L2CRCAL_CONT.INC
C-   Inputs  : None
C-   Outputs : None
C-   Controls:
C-
C-   Created  20-AUG-90   by the L2STATE program
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'       ! ZEBSTP common
      INCLUDE 'D0$INC:L2CRCAL_CONT.INC'
      INCLUDE 'D0$INC:L2CRCALS.INC'
      LOGICAL L2CR_BUILD
      CHARACTER*32 BANKN(20)
      INTEGER NBANKS,LUN,IER,LSL2H,GZSL2H,RECSIZ
C----------------------------------------------------------------------
C---We need geometry information for the CC. We will put it in a bank
C---called L2CR.
C---Purpose is to make the L2CR bank and hang it on the STP tree.
      IF (.NOT. L2CR_BUILD() ) THEN
        CALL ERRMSG('L2CRCAL','L2CRCAL_INIT', ' Cannot make L2CR bank',
     &    'W')
        GOTO 800
      END IF

C---Read in our RCP file: L2CRCAL_RCP and hang it on the STP tree below
C---SL2H in a reserved link for use later.
C---Read from RCP file:
C---Open file. Read it in with RCP and close again...
      CALL EOPEN('L2CRCAL_RCP','R','F',0,LUN,IER)
      IF (IER .NE. 0) THEN
        MUMSG = ' L2CRCAL_RCP file open failed '
        GOTO 800
      END IF
C---Book SL2H  if not done already:
      LSL2H = GZSL2H()
      IF (LSL2H  .LE. 0) CALL BKSL2H(LSL2H)
      IF (LSL2H .LE. 0) THEN
        MUMSG = ' Unable to book SL2H'
        GOTO 800
      END IF
C---Read in RCP file.
      RECSIZ = 8  !squash out comments
      CALL EZMAKE(LUN,RECSIZ,BANKN,NBANKS)   ! Read in the banks
      CALL EZERR(IER)
      IF (IER .NE. 0) THEN
        MUMSG = ' L2CRCAL_RCP read failed '
        GOTO 800
      END IF
C---Chain them together...
      IF (NBANKS .GT. 1) THEN
        CALL EZCHAIN(BANKN,NBANKS)        ! Chain them together under #1
        CALL EZERR(IER)
        IF (IER .NE. 0) THEN
          MUMSG= ' Failed to EZCHAIN rcp banks together '
          GOTO 800
        END IF
      END IF

C---Move it to hang underneath SL2H (L2 STP HEADER)
      LSL2H = GZSL2H()
      CALL EZSHUNT(BANKN(1),LSL2H,IZL2CRCAL_RCP) ! Move it here
      CALL EZERR(IER)
      IF (IER .NE. 0) THEN
        MUMSG = ' L2CRCAL_RCP  move failed '
        GOTO 800
      END IF
C---Close the file.
      CALL ECLOSE(LUN,IER)              ! Close RCP file
      IF (IER .NE. 0) THEN
        MUMSG = ' L2CRCAL_RCP file close failed '
        GOTO 800
      END IF
      RETURN
C
C---NOTE: As we only want stuff under the L2 portion of the STP tree,
C---we may have to explicitly drop stuff like SCAL and SMUO that we may
C---read in to MAKE the constants we need.
C
  800 CONTINUE                        ! ERROR
      CALL ERRMSG('L2CRCAL','L2CRCAL_INIT',MUMSG,'W')
  999 RETURN
      END


      SUBROUTINE CDC_L2TRK_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Make STP structure to hold constants and
C-                        parameters to be used by LEVEL2 tracking
C-
C-   Created   01-NOV-1991  D Claes
C-   Modified  10-MAY-1993  D Claes to serve as INIT for COSMIC TRK tool
C-
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
      INTEGER LSL2H
C----------------------------------------------------------------------
C
C---The purpose of this routine is to put all the parameters and constants
C---we need into the Static Paramameters common block (ZEBSTP) in one form
C---or another.
C
C---STP should be initialized by LEVEL2
C      IF (LSTPH .LE. 0) CALL INZSTP
C      IF (LSTPH .LE. 0) THEN
C        NOWERRMESS = ' Cannot initialize ZEBSTP common '
C        NOWSMESS   = 'Init fail'
C        GOTO 900
C      END IF
C
C      CALL BKSL2H(LSL2H)                ! Book SL2H header
C      IF (LSL2H .LE. 0) THEN
C        NOWERRMESS = ' Unable to book SL2H'
C        NOWSMESS   = 'SL2H fail'
C        GOTO 900
C      END IF
C
C      CALL L2_CDINIT                   ! All of this initialization will 
C                                       ! be performed by L2_CD_MATCH_INIT 
C      CALL L2TRAK_STP                  ! under its call from L2_EM.
C
C--- If we made it here, then everything is okay so far.
C
      GOTO 999                          ! Return
  900 CONTINUE                          ! Error
      CALL ERRMSG(NOWSMESS,'L2TRAK_INIT',NOWERRMESS,'W')
  999 RETURN
      END

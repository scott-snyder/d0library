      SUBROUTINE EDISCT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Disconnects transformation network.
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created   8-FEB-1989   SHAHRIAR ABACHI
C-   UPDATED   11-AUG-1992   SHAHRIAR ABACHI   fine scale added to knob 8
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*4 TRNSF(50)
      COMMON /ESCAPC/ TRNSF
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:GDIVWT.INC/LIST'
      INCLUDE 'D0$INC:NEWDI3.INC/LIST'
      EXTERNAL ERRHND
C
      IDIVWT = 1
C
      CALL PDI('DIALS', 1, 1, TRNSF(1), ERRHND)
      CALL PDI('DIALS', 2, 1, TRNSF(2), ERRHND)
      CALL PDI('DIALS', 3, 1, TRNSF(3), ERRHND)
      CALL PDI('DIALS', 4, 1, TRNSF(4), ERRHND)
C
      CALL PDI('DIALS', 5, 1, TRNSF(8), ERRHND)
C
      CALL PDI('DIALS', 6, 1, TRNSF(9), ERRHND)
C
CC      CALL PDI('DIALS', 8, 1, 'MULP3', ERRHND)
c      CALL PDI('MULP3', 1, 1, 'ACCM1', ERRHND)
      CALL PDI('ACCM1', 1, 2, TRNSF(5), ERRHND)
cc      CALL PDI('DIALS', 8, 1, TRNSF(5), ERRHND)  !NEW
      CALL PDI('DIALS', 8, 1, 'MULP3', ERRHND)
      CALL PDI('MULP3', 1, 1, TRNSF(4), ERRHND)  !NEW
C
      CALL PDI('DIALS', 7, 1, TRNSF(7), ERRHND)
C
      CALL PDI(TRNSF(7), 1, 1, 'ACCM5', ERRHND)
      CALL PDI(TRNSF(8), 1, 1, 'ACCM5', ERRHND)
      CALL PDI(TRNSF(9), 1, 1, 'ACCM5', ERRHND)
C
      CALL PSNFIX(0, 1, 'OFFBUTTONLIGHTS', ERRHND)
      CALL PSNBOO(.TRUE., 1, 'CLEAR_LABELS', ERRHND)
      CALL PSNFIX(0, 1, 'OFFBUTTONLIGHTS', ERRHND)
      CALL PSNBOO(.TRUE., 1, 'CLEAR_LABELS', ERRHND)
C
      CALL PPURGE(ERRHND)
  999 RETURN
      END

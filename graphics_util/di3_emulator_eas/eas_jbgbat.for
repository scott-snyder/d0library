      SUBROUTINE JBGBAT(NLEVEL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Opens a batch of updates.
C-
C-   Inputs  :  NLEVEL  :  level of batch (irrelvant for this emulator)
C-
C-   Created   5-JUL-1989   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NLEVEL
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
C
      BATCH = .TRUE.
      BATSEG = .TRUE.
C
  999 RETURN
      END

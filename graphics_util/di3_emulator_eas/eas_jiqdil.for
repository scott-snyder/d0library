      SUBROUTINE JIQDIL(RLEVEL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To inquire the release level of DI-3000.
C-
C-   Inputs  : None
C-   Outputs : RLEVEL
C-                      Release level (set to negative for Emulator)
C-
C-
C-   Created   9-NOV-1989   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL RLEVEL
      INCLUDE 'D0$INC:NEWDI3.INC/LIST'
C
      RLEVEL = -2.0
      IF(NUDI3) RLEVEL = -3.0
C
  999 RETURN
      END

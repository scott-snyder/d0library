      SUBROUTINE FZERO_FDCPRM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  10-SEP-1991   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
C----------------------------------------------------------------------
      FDCPRM(1) = 0
      FDCPRM(2) = 0
C
      LFGNH = 0
      CALL VZERO(LFGHF(0),2)
      CALL VZERO(LFGUN(0,0),4)
      CALL VZERO(LFGQD(0,0),16)
      CALL VZERO(LFGSE(0,0,0,0),1152)
C
      LFPDH = 0
      CALL VZERO(LFPHF(0),2)
      CALL VZERO(LFPUN(0,0),4)
      CALL VZERO(LFPQD(0,0),16)
      CALL VZERO(LFPSE(0,0,0,0),1152)
C
      LFTMH = 0
      CALL VZERO(LFTHF(0),2)
      CALL VZERO(LFTUN(0,0),4)
      CALL VZERO(LFTQU(0,0),16)
      CALL VZERO(LFTSE(0,0,0,0),1152)
C
      LFALH = 0
      CALL VZERO(LFAHF(0),2)
      CALL VZERO(LFAUN(0,0),4)
      CALL VZERO(LFAQD(0,0),16)
      CALL VZERO(LFASE(0,0,0,0),1152)
C
      LFGEH = 0
      LFMAT = 0
      LFWAL = 0
      LFWTA = 0
      LFWTB = 0
      LFWPH = 0
      LFDRT = 0
      LFDTA = 0
      LFDTB = 0
      LFDPH = 0
C
C----------------------------------------------------------------------
  999 RETURN
      END

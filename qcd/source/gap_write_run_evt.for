      SUBROUTINE GAP_WRITE_RUN_EVT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-FEB-1994   Brent J. May
C-   Updated  24-JUL-1995   Bob Hirosky  use new eta,phi,et arrays
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:QCD_EVT_INFO.INC/LIST'
      INCLUDE 'D0$INC:QCD_JET.INC/LIST'
      INCLUDE 'D0$INC:QCD_NTUP_INFO.INC/LIST'
      INCLUDE 'D0$INC:QCD_JUTL_HEAD.INC/LIST'
C
      INCLUDE 'D0$INC:GAP_CATD_INFO.INC/LIST'   ! CATD info
      INTEGER I,J,IETA(50),IPHI(50),NEM_MIN
      REAL ETEM(50)
C
      NEM_MIN=MIN(NEM_CATD(1),50)
      DO I=1,NEM_MIN
        IETA(I) = EM_BTWN_ETA(I)
        IPHI(I) = EM_BTWN_PHI(I)
        ETEM(I) = EM_BTWN_ET(I)
      END DO
      WRITE(80,200) RUNNUM,EVTNUM,NEM_RAW,NEM_CATD(1),IEL,IEH
      WRITE(80,201) (IETA(J),IPHI(J),ETEM(J),J=1,NEM_MIN)
  200 FORMAT(X,2I7,4I4)
  201 FORMAT(X,2I4,F7.2,/)
  999 RETURN
      END

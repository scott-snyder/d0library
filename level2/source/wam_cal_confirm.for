      SUBROUTINE WAM_CAL_CONFIRM(VTX,DIR,PASS,ESUM,REGION)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given VTX and 3-hit in Samus A-Layer
C-                         which has been determined to be in a
C-                         track candidate, determine, from energy
C-                         in Calorimeter, whether this track can be
C-                         passed.
C-
C-   Inputs  : VTX(3),APOINT(3)
C-   Outputs : PASS
C-   Controls: none
C-
C-   Created  10-AUG-1993   j.balderston
C-   Updated  30-SEP-1993   j.balderston  TAKE TRIG FOR REJECT TEST
C-   Updated  14-OCT-1993   j.balderston  PRELIM RUN FORM
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL PASS,FIRST
C
      DATA FIRST/.TRUE./
C
      INTEGER NCELL,ARGSOK,IETAC(20),IPHIC(20),LAYERC(20)
      INTEGER MAX_ETA,MIN_ETA,MAX_PHI,MIN_PHI,MEAN_ETA,MEAN_PHI,ICELL
      INTEGER REGION
      INTEGER LPMUO,GZPMUO,NMU
C
      REAL VTX(3),DIR(3),PATHC(20),MU_THRESH,ESUM,NRG,ETA,PHI
C----------------------------------------------------------------------
      PASS=.FALSE.
      IF(FIRST) THEN
!       GET THRESH ENERGY = MU_THRESH
        MU_THRESH = 1.0
        FIRST=.FALSE.
      ENDIF
      CALL CLINPL(VTX,DIR,20,NCELL,IETAC,IPHIC,LAYERC,PATHC,ARGSOK)
C
C Determine central value for returned ranges of IETA and IPHI
C
      MAX_ETA=IETAC(1)
      MIN_ETA=IETAC(1)
      MAX_PHI=IPHIC(1)
      MIN_PHI=IPHIC(1)
      DO ICELL=1,NCELL
        IF(IETAC(ICELL).GT.MAX_ETA) MAX_ETA=IETAC(ICELL)
        IF(IETAC(ICELL).LT.MIN_ETA) MIN_ETA=IETAC(ICELL)
        IF(IPHIC(ICELL).GT.MAX_PHI) MAX_PHI=IPHIC(ICELL)
        IF(IPHIC(ICELL).LT.MIN_PHI) MIN_PHI=IPHIC(ICELL)
      ENDDO
      MEAN_ETA=INT((MAX_ETA+MIN_ETA)/2)
      MEAN_PHI=INT((MAX_PHI+MIN_PHI)/2)
      CALL MUON_CALOR_CONFIRM(MEAN_ETA,MEAN_PHI,MU_THRESH,ESUM,PASS,
     &  REGION)
  999 RETURN
      END

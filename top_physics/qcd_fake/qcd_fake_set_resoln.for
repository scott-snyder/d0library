      SUBROUTINE QCD_FAKE_SET_RESOLN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SETS UP RESOLUTIONS FOR EVENT
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  30-MAR-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TOP_FIT_SMEAR.INC'
      INCLUDE 'D0$INC:QCD_SMEAR_EVENT.INC'
      INTEGER I,J
      DOUBLE PRECISION ENERGY,ETA,PHI
C----------------------------------------------------------------------
      RES(A1,A2,A3,E) = SQRT(A1*A1 + A2*A2/E + (A3/E)**2) !FRACTIONAL RESOLUTION
C
C ****  ELECTRON. ENERGY RESOLUTIONS ARE FRACTIONAL
C

      DO I = 1 , NELE
        ENERGY = ELEC(4,I)
        ETA = ELEC(6,I)
        PHI = ELEC(7,I)
        SIG_ELE(1,I) = ENERGY*RES(ELECTRON_RESOLN(1),
     &    ELECTRON_RESOLN(2), ELECTRON_RESOLN(3),ENERGY)
        SIG_ELE(2,I) = RES(ELEC_ETA_RESOLN(1),ELEC_ETA_RESOLN(2),
     &    ELEC_ETA_RESOLN(3),ENERGY)
        SIG_ELE(3,I) = RES(ELEC_PHI_RESOLN(1),ELEC_PHI_RESOLN(2),
     &    ELEC_PHI_RESOLN(3),ENERGY)
      ENDDO
C
      DO I = 1 , NJETS
        ENERGY = JETS(4,I)
        ETA = JETS(6,I)
        PHI = JETS(7,I)
        SIG_JET(1,I) = ENERGY*RES(JET_RESOLN(1),
     &    JET_RESOLN(2), JET_RESOLN(3),ENERGY)
        SIG_JET(2,I) = RES(JET_ETA_RESOLN(1),JET_ETA_RESOLN(2),
     &    JET_ETA_RESOLN(3),ENERGY)
        SIG_JET(3,I) = RES(JET_PHI_RESOLN(1),JET_PHI_RESOLN(2),
     &    JET_PHI_RESOLN(3),ENERGY)
      ENDDO
      SIG_REST_PT = REST_PT*RES(REST_RESOLN(1),REST_RESOLN(2),
     &  REST_RESOLN(3),REST_PT)
C
      SIG_RESTP(1) = ABS(SIG_REST_PT*REST(1)/REST_PT)  !COMPONENT RESOLUTIONS
      SIG_RESTP(2) = ABS(SIG_REST_PT*REST(2)/REST_PT)
C
  999 RETURN
      END

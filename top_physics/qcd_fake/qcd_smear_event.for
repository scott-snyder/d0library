      SUBROUTINE QCD_SMEAR_EVENT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SMEAR EVENT ACCORDING TO RESOLUTIONS.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  24-MAR-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TOP_FIT_SMEAR.INC'
      INCLUDE 'D0$INC:QCD_SMEAR_EVENT.INC'
      INCLUDE 'D0$INC:QCD_FAKE_NTUPLE.INC'
      DOUBLE PRECISION MASS
      INTEGER I,J
      REAL    EL(4),NU(2)
C
      REAL    R1,R2,R3,R4
C
      LOGICAL first
      SAVE first
      DATA first / .true. /
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
c read in resolution parameters
        CALL TOP_FIT_SETUP_SMEAR
      ENDIF
C
      CALL QCD_FAKE_SET_RESOLN  !SETS UP RESOLUTIONS FOR PARTICLES
C
      MASS = 0.0
      DO I = 1 , NELE
        CALL DO_SMEAR(ELE(1,I),SIG_ELE(1,I),SIG_ELE(2,I),SIG_ELE(3,I),
     &    MASS,ELEC_SMEAR(1,I))
        ELE_SMEAR(1,I) = ELEC_SMEAR(4,I)
        ELE_SMEAR(2,I) = ELEC_SMEAR(6,I)
        ELE_SMEAR(3,I) = ELEC_SMEAR(7,I)
      ENDDO
C
      MASS = 0.0  !NOT IN NTUPLE.
      DO I = 1 , NJETS
        CALL DO_SMEAR(JET(1,I),SIG_JET(1,I),SIG_JET(2,I),SIG_JET(3,I),
     &    MASS,JETS_SMEAR(1,I))
        JET_SMEAR(1,I) = JETS_SMEAR(4,I)
        JET_SMEAR(2,I) = JETS_SMEAR(6,I)
        JET_SMEAR(3,I) = JETS_SMEAR(7,I)
      ENDDO
C
      CALL RANNOR(R1,R2)
      REST_SMEAR(1) = REST(1)+R1*SIG_RESTP(1)
      REST_SMEAR(2) = REST(2)+R2*SIG_RESTP(2)
      REST_PT_SMEAR = SQRT(REST_SMEAR(1)**2+REST_SMEAR(2)**2)
C
      DO I = 1 , 2
        NEUT_SMEAR(I) = 0.0
        DO J = 1 , NELE
          NEUT_SMEAR(I) = NEUT_SMEAR(I) + ELEC_SMEAR(I,J)
        ENDDO
        DO J = 1 , NJETS
          NEUT_SMEAR(I) = NEUT_SMEAR(I) + JETS_SMEAR(I,J)
        ENDDO
        NEUT_SMEAR(I) = NEUT_SMEAR(I) + REST_SMEAR(I)
        NEUT_SMEAR(I) = -NEUT_SMEAR(I)
      ENDDO
C
      METC1_SMEAR = SQRT(NEUT_SMEAR(1)**2 + NEUT_SMEAR(2)**2)
      CALL UCOPYDS(ELEC_SMEAR(1,1),EL,4)
      CALL UCOPYDS(NEUT_SMEAR,NU,2)
      CALL TRANSVERSE_MASS(EL,NU,WMTC_SMEAR)
C
  999 RETURN
      END

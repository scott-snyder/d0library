      SUBROUTINE QCD_FAKE_ZERO_MET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ZEROES missing ET of unsmeared event
C-   by sharing it with the other particles according to their 
C-   resolution
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  30-MAR-1994   Rajendran Raja
C-   Updated  14-APR-1994   Rajendran Raja  using chisquared minim 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TOP_FIT_SMEAR.INC'
      INCLUDE 'D0$INC:QCD_SMEAR_EVENT.INC'
      INCLUDE 'D0$INC:QCD_FAKE_NTUPLE.INC'
      LOGICAL first
      SAVE first
      DATA first / .true. /
      DOUBLE PRECISION SIG_ELEX(2,NELMX),SIG_JETX(2,NJTMX)
      DOUBLE PRECISION SUM_RES(2),NEUT_N(2)
      INTEGER I,J
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
C read in resolution parameters
        CALL TOP_FIT_SETUP_SMEAR
      ENDIF
C
      CALL QCD_FAKE_SET_RESOLN
C
      SUM_RES(1) = 0.0
      SUM_RES(2) = 0.0
      DO I = 1 , NELE
C TAKING COMPONENT OF ELECTRON RESOLUTIONS
        DO J = 1 , 2
          SIG_ELEX(J,I) = (SIG_ELE(1,I)*ELEC(J,I)/ELEC(4,I))**2
          SUM_RES(J) = SUM_RES(J) + SIG_ELEX(J,I)
        ENDDO
      ENDDO
C
      DO I = 1 , NJETS
C TAKING COMPONENT OF JET RESOLUTIONS
        DO J = 1 , 2
          SIG_JETX(J,I) = (SIG_JET(1,I)*JETS(J,I)/JETS(4,I))**2
          SUM_RES(J) = SUM_RES(J) + SIG_JETX(J,I)
        ENDDO
      ENDDO
C
      DO J = 1 , 2
C ADDING COMPONENT OF REST OF THE EVENT RESOLUTIONS
        SUM_RES(J) = SUM_RES(J) + SIG_RESTP(J)**2
      ENDDO
C
C NOW TO DISTRIBUTE THE NEUTRINO ET TO THE VARIOUS PARTICLES IN PROPORTION
C TO THEIR PROJECTED RESOLUTIONS
      DO J = 1 , 2
        NEUT_N(J) = 0  !NEW NEUTRINO
        DO I = 1 , NELE
          ELEC(J,I) = ELEC(J,I) + NEUT(J)*SIG_ELEX(J,I)/SUM_RES(J)
          NEUT_N(J) = NEUT_N(J) + ELEC(J,I)
        ENDDO
        DO I = 1 , NJETS
          JETS(J,I) = JETS(J,I) + NEUT(J)*SIG_JETX(J,I)/SUM_RES(J)
          NEUT_N(J) = NEUT_N(J) + JETS(J,I)
        ENDDO
        REST(J) = REST(J) + NEUT(J)*(SIG_RESTP(J)**2)/SUM_RES(J)
        NEUT_N(J) = NEUT_N(J) + REST(J)
        IF ( ABS(NEUT_N(J)).GT.1.E-3 ) THEN
          CALL ERRMSG(' QCD_FAKE','QCD_FAKE_ZERO_MET',
     &      'NEUTRINO COMPONENTS NOT ZEROED PROPERLY ','W')
        ENDIF
        NEUT(J) = NEUT_N(J)
      ENDDO
C
      METC1 = 0.0
      WMTC = 0.0   !ZERO MISSING ET
      REST_PT = SQRT(REST(1)**2 + REST(2)**2)
C
      DO I = 1 , NELE
        CALL QCD_FAKE_RENORMALIZE(ELEC(1,I))
      ENDDO
C
      DO I = 1 , NJETS
        CALL QCD_FAKE_RENORMALIZE(JETS(1,I))
      ENDDO
C
C
  999 RETURN
      END

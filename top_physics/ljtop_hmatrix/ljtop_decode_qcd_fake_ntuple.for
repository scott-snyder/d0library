      SUBROUTINE LJTOP_DECODE_QCD_FAKE_NTUPLE(USE_EVENT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get QCD_FAKE_Ntuple info and fill in quans.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  16-APR-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:EVENT_QUAN.INC'
      INCLUDE 'D0$INC:QCD_FAKE_NTUPLE.INC'
C
      REAL    JET_ET_CUT
      DOUBLE PRECISION    ELE(3,2),JET(3,5)
      INTEGER IER,NELE,I
      DOUBLE PRECISION    JET_TEMP(NJ,NJETS_MAX)
      DOUBLE PRECISION    ELEC_TEMP(NE,MAX_LEPHOT)
      INTEGER NJT
C
      LOGICAL FIRST,OK
      DATA FIRST/.TRUE./
      LOGICAL USE_EVENT
C----------------------------------------------------------------------
      IF ( FIRST  ) THEN
        FIRST = .FALSE.
        CALL EZPICK('LJTOP_HMATRIX_RCP')
        CALL EZGET('JET_ET_CUT',JET_ET_CUT,IER)
        CALL EZRSET
      ENDIF
C
C LOAD COMMON BLOCK.
C
      Q(LHEAD+6) = RUN
      Q(LHEAD+9) = EVENT
      NELE = NPELC
      NJETS = UNJETS  !JNEP number of jets
      ELE(1,1) = EE1
      ELE(2,1) = ETAE1
      ELE(3,1) = PHIE1
      ELE(1,2) = EE2
      ELE(2,2) = ETAE2
      ELE(3,2) = PHIE2
      P2_NEUT(1) = METC1*COS(METPHIC1)
      P2_NEUT(2) = METC1*SIN(METPHIC1)

      MET_D0 = METC1
C
C ****  CORRECTED QUANTITIES
C
      JET(1,1) = EJC1
      JET(2,1) = ETAJC1
      JET(3,1) = PHIJ51
      JET(1,2) = EJC2
      JET(2,2) = ETAJC2
      JET(3,2) = PHIJ52
      JET(1,3) = EJC3
      JET(2,3) = ETAJC3
      JET(3,3) = PHIJ53
      JET(1,4) = EJC4
      JET(2,4) = ETAJC4
      JET(3,4) = PHIJ54
      JET(1,5) = EJC5
      JET(2,5) = ETAJC5
      JET(3,5) = PHIJ55
C
      NELE = MIN(NELE,MAX_LEPHOT)
      NJETS = MIN(NJETS,NJETS_MAX)
C
      DO I = 1 , NELE
        CALL GET_CART(ELE(1,I),ELEC_TEMP(1,I),0.0)
        CALL UCOPYDS(ELEC_TEMP(1,I),P24_ELECTRON(1,I),NE)
      ENDDO
      ELECTRON = .FALSE.
      IF ( NELE.GT.0 ) THEN
        CALL UCOPY(P24_ELECTRON,P6_CLUST,6)
        ELECTRON = .TRUE.
      ENDIF
      DO I = 1 , NJETS
        CALL GET_CART(JET(1,I),JET_TEMP(1,I),0.0)
      ENDDO
C
C ****  IMPOSE JET THRESHOLD
C
      NJT = 0
      DO I = 1 , NJETS
        IF ( JET_TEMP(5,I).GE.JET_ET_CUT ) THEN
          NJT = NJT + 1
          CALL UCOPYDS(JET_TEMP(1,I),P25_JETS(1,NJT),NJ)
        ENDIF
      ENDDO
      NJETS=NJT
C
      NVER=1
      CALL UZERO(P9_VERTEX(1,1),1,3)  !NOMINAL VERTEX
C
      IF(BAD_ELE.EQ.0)THEN
        TIGHT_ELECTRON=1
        LOOSE_ELECTRON=0
      ELSE
        TIGHT_ELECTRON=0
        LOOSE_ELECTRON=1
      ENDIF
C
      RDEDX = DEDX1
      ELE_HIGH=1.0   !ALL QCD TRIGGERED ON THIS
C
      USE_EVENT = .TRUE.
  999 RETURN
      END

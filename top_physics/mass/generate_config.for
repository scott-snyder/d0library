      SUBROUTINE GENERATE_CONFIG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GENERATE NEXT CONFIGURATION OF EVENT
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-JAN-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:KINEQ.INC'
C
      LOGICAL first
      SAVE first
      DATA first / .true. /
C
      REAL    ELECTRON_RESOLN_S(2),MUON_RESOLN_S(2),JET_RESOLN_S(2)
      DOUBLE PRECISION    ELECTRON_RESOLN(2),
     &  MUON_RESOLN(2),JET_RESOLN(2)
C
      DOUBLE PRECISION    RES,RES_MU,K,A1,A2,E
      DOUBLE PRECISION    SIG_L1,SIG_L2,SIG_J1,SIG_J2,SIG_J3
      INTEGER IER , I
      REAL R1,R2,R3,R4
      DOUBLE PRECISION    DELP(2)
      DOUBLE PRECISION    XINV,LINV
      REAL    PMAX_MUON
C----------------------------------------------------------------------
C
C ****  IN LINE FUNCTION. A(1) = SAMPLING TERM. A(2) = CONSTANT TERM
C
      RES(A1,A2,E) = SQRT((A1*A1/E) + A2*A2)
      RES_MU(A1,A2,K) = SQRT((A1/K)**2 + A2*A2)
C
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
        CALL EZGET_rarr('ELECTRON_RESOLN',ELECTRON_RESOLN_S,IER)
        CALL EZGET_rarr('MUON_RESOLN',MUON_RESOLN_S,IER)
        CALL EZGET_rarr('JET_RESOLN',JET_RESOLN_S,IER)
        CALL EZGET('MAXIMUM_MUON_MOMENTUM',PMAX_MUON,IER)
C
        CALL UCOPYSD(ELECTRON_RESOLN_S,ELECTRON_RESOLN,2)
        CALL UCOPYSD(MUON_RESOLN_S,MUON_RESOLN,2)
        CALL UCOPYSD(JET_RESOLN_S,JET_RESOLN,2)
C
        CALL EZRSET
C
        IF ( LEPTON_TYPE(1).EQ.1 ) THEN
C
C ****  ELECTRON
C
          SIG_L1 = RES(ELECTRON_RESOLN(1),
     &      ELECTRON_RESOLN(2), LEPTON1(4))

        ELSE
C
C ****  MUON
C
          LINV = 1./LEPTON1(4)
          SIG_L1 = RES_MU(MUON_RESOLN(1),
     &      MUON_RESOLN(2), LINV)
        ENDIF
C
        IF ( LEPTON_TYPE(2).EQ.1 ) THEN
C
C ****  ELECTRON
C
          SIG_L2 = RES(ELECTRON_RESOLN(1),
     &      ELECTRON_RESOLN(2), LEPTON2(4))

        ELSE
C
C ****  MUON
C
          LINV = 1./LEPTON2(4)
          SIG_L2 = RES_MU(MUON_RESOLN(1),
     &      MUON_RESOLN(2), LINV)
        ENDIF
C
        SIG_J1 = RES(JET_RESOLN(1),JET_RESOLN(2),JET1(4))
        SIG_J2 = RES(JET_RESOLN(1),JET_RESOLN(2),JET2(4))
        SIG_J3 = RES(JET_RESOLN(1),JET_RESOLN(2),JET3(4))
C
      ENDIF
C
      CALL RANNOR(R1,R2)
      IF ( LEPTON_TYPE(1).EQ.1 ) THEN
        LEPTON1(4) = LEPTON1_S(4)*(1.0 + R1*SIG_L1)
      ELSE
1       XINV = (1.0/LEPTON1_S(4))*(1.0 + R1*SIG_L1)
        LEPTON1(4) = ABS(1.0/XINV)
        IF ( LEPTON1(4).GT.PMAX_MUON ) THEN
          CALL RANNOR(R1,R2)
          GO TO 1
        ENDIF
      ENDIF
C
      DO I = 1 , 3
C KEEP DIRECTION SAME
        LEPTON1(I) = LEPTON1(4)*LEPTON1_S(I)/LEPTON1_S(4)
      ENDDO
C
      IF ( LEPTON_TYPE(2).EQ.1 ) THEN
        LEPTON2(4) = LEPTON2_S(4)*(1.0 + R2*SIG_L2)
      ELSE
2       XINV = (1.0/LEPTON2_S(4))*(1.0 + R2*SIG_L2)
        LEPTON2(4) = ABS(1.0/XINV)
        IF ( LEPTON2(4).GT.PMAX_MUON ) THEN
          CALL RANNOR(R1,R2)
          GO TO 2
        ENDIF
      ENDIF
C
      DO I = 1 , 3
C KEEP DIRECTION SAME
        LEPTON2(I) = LEPTON2(4)*LEPTON2_S(I)/LEPTON2_S(4)
      ENDDO
C
      CALL RANNOR(R1,R2)
      CALL RANNOR(R3,R4)
      JET1(4) = JET1_S(4)*(1.0 + R1*SIG_J1)
      DO I = 1 , 3
C KEEP DIRECTION SAME
        JET1(I) = JET1(4)*JET1_S(I)/JET1_S(4)
      ENDDO
      JET2(4) = JET2_S(4)*(1.0 + R2*SIG_J2)
      DO I = 1 , 3
C KEEP DIRECTION SAME
        JET2(I) = JET2(4)*JET2_S(I)/JET2_S(4)
      ENDDO
      JET3(4) = JET3_S(4)*(1.0 + R3*SIG_J3)
      DO I = 1 , 3
C KEEP DIRECTION SAME
        JET3(I) = JET3(4)*JET3_S(I)/JET3_S(4)
      ENDDO
C
      DO I = 1 , 2
        DELP(I) = 0.0
        DELP(I) = DELP(I) + LEPTON1(I)-LEPTON1_S(I)
        DELP(I) = DELP(I) + LEPTON2(I)-LEPTON2_S(I)
        DELP(I) = DELP(I) + JET1(I)-JET1_S(I)
        DELP(I) = DELP(I) + JET2(I)-JET2_S(I)
        DELP(I) = DELP(I) + JET3(I)-JET3_S(I)
        PNUT(I) = PNUT_S(I) - DELP(I)
      ENDDO
C
  999 RETURN
      END

      SUBROUTINE CPHAD_TRG(IETA,IPHI,ICRATE,IADC,IBLS,ITWR,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Converts an address in the trigger tower PHysics 
C-                         system to the hardware ADC address
C-                         Similar to CPHAD for the precision readout
C-
C   Inputs  : IETA     trigger tower eta index     [-20,-1],[1,20]
C             IPHI     trigger tower phi index     [1,32]
C
C   Outputs : ICRATE    ADC crate number
C             IADC      ADC card number            [0,11]
C             IBLS      BLS card number            [0,7]
C             ITWR      BLS TOWER                  [0,3]
C             IER       return code: 0 = OK
C                                    1 = invalid input
C-   Controls: none
C-
C-   Created  26-OCT-1993   J Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IETA,IPHI,ICRATE,IADC,IBLS,ITWR,IER
      INTEGER IABS_ETA,IBRD,ITMP_ETA
      INTEGER IETA_OFF_CC,IETA_OFF_EC,NCC,NEC
      INTEGER IPHI_OFF1,IPHI_OFF2,IPHI_OFF3,IPHI_OFF4
      INTEGER IPHI_OFF5,IPHI_OFF6
      DATA IETA_OFF_CC/1/,IETA_OFF_EC/7/
      DATA NCC/6/,NEC/12/
      DATA IPHI_OFF1/0/,IPHI_OFF2/8/,IPHI_OFF3/16/
      DATA IPHI_OFF4/24/,IPHI_OFF5/32/,IPHI_OFF6/40/
C----------------------------------------------------------------------
      IER = 0
      IF ((IPHI.LE.0).OR.(IPHI.GT.32)) IER = 1
      IF (IETA.EQ.0) IER = 1
      IABS_ETA = ABS(IETA)
      IF (IABS_ETA.GT.20) IER = 1
      ITWR = -1
C
      IF ((1.LE.IABS_ETA) .AND. (IABS_ETA.LE.6)) THEN       ! CC
        IF ((9.LE.IPHI) .AND. (IPHI.LE.24)) THEN            ! West side
          IF (IETA.GT.0) THEN                               ! South side
            ICRATE = 18
            IBRD = -((IPHI-IPHI_OFF4)*NCC - IETA+IETA_OFF_CC)
          ELSE                                              ! North side
            ICRATE = 7
            IBRD = (IPHI-IPHI_OFF2-1)*NCC - IETA-IETA_OFF_CC
          ENDIF
        ELSE                                                ! East side
          IF (IETA.GT.0) THEN                               ! South side
            ICRATE = 8
            IF (IPHI.LE.8) THEN
              IBRD = -((IPHI-IPHI_OFF2)*NCC - IETA+IETA_OFF_CC)
            ELSE
              IBRD = -((IPHI-IPHI_OFF6)*NCC - IETA+IETA_OFF_CC)
            ENDIF
          ELSE                                              ! North side
            ICRATE = 17
            IF (IPHI.LE.8) THEN
              IBRD = (IPHI+IPHI_OFF2-1)*NCC - IETA-IETA_OFF_CC
            ELSE
              IBRD = (IPHI-IPHI_OFF4-1)*NCC - IETA-IETA_OFF_CC
            ENDIF
          ENDIF
        ENDIF
      ELSE                                                  ! EC
        IF (IPHI.LE.8) THEN                                 ! Top east
          IF (IETA.GT.0) THEN                               ! South side
            ICRATE = 28
            ITMP_ETA = IETA
            IF (IABS_ETA.GE.17) THEN                        ! L or M card
              ITMP_ETA = SIGN(17,IETA)
              ITWR = IABS_ETA - 17
            ENDIF
            IBRD = -((IPHI-IPHI_OFF2)*NEC - ITMP_ETA+IETA_OFF_EC)
          ELSE                                              ! North side
            ICRATE = 57
            ITMP_ETA = IETA
            IF (IABS_ETA.GE.17) THEN                        ! L or M card
              ITMP_ETA = SIGN(17,IETA)
              ITWR = IABS_ETA - 17
            ENDIF
            IBRD = (IPHI-IPHI_OFF1-1)*NEC - ITMP_ETA-IETA_OFF_EC
          ENDIF
        ELSEIF (IPHI.LE.16) THEN                            ! Top west
          IF (IETA.GT.0) THEN                               ! South side
            ICRATE = 58
            ITMP_ETA = IETA
            IF (IABS_ETA.GE.17) THEN                        ! L or M card
              ITMP_ETA = SIGN(17,IETA)
              ITWR = IABS_ETA - 17
            ENDIF
            IBRD = -((IPHI-IPHI_OFF3)*NEC - ITMP_ETA+IETA_OFF_EC)
          ELSE                                              ! North side
            ICRATE = 27
            ITMP_ETA = IETA
            IF (IABS_ETA.GE.17) THEN                        ! L or M card
              ITMP_ETA = SIGN(17,IETA)
              ITWR = IABS_ETA - 17
            ENDIF
            IBRD = (IPHI-IPHI_OFF2-1)*NEC - ITMP_ETA-IETA_OFF_EC
          ENDIF
        ELSEIF (IPHI.LE.24) THEN                            ! Bottom west
          IF (IETA.GT.0) THEN                               ! South side
            ICRATE = 48
            ITMP_ETA = IETA
            IF (IABS_ETA.GE.17) THEN                        ! L or M card
              ITMP_ETA = SIGN(17,IETA)
              ITWR = IABS_ETA - 17
            ENDIF
            IBRD = -((IPHI-IPHI_OFF4)*NEC - ITMP_ETA+IETA_OFF_EC)
          ELSE                                              ! North side
            ICRATE = 37
            ITMP_ETA = IETA
            IF (IABS_ETA.GE.17) THEN                        ! L or M card
              ITMP_ETA = SIGN(17,IETA)
              ITWR = IABS_ETA - 17
            ENDIF
            IBRD = (IPHI-IPHI_OFF3-1)*NEC - ITMP_ETA-IETA_OFF_EC
          ENDIF
        ELSE                                                ! Bottom east
          IF (IETA.GT.0) THEN                               ! South side
            ICRATE = 38
            ITMP_ETA = IETA
            IF (IABS_ETA.GE.17) THEN                        ! L or M card
              ITMP_ETA = SIGN(17,IETA)
              ITWR = IABS_ETA - 17
            ENDIF
            IBRD = -((IPHI-IPHI_OFF5)*NEC - ITMP_ETA+IETA_OFF_EC)
          ELSE                                              ! North side
            ICRATE = 47
            ITMP_ETA = IETA
            IF (IABS_ETA.GE.17) THEN                        ! L or M card
              ITMP_ETA = SIGN(17,IETA)
              ITWR = IABS_ETA - 17
            ENDIF
            IBRD = (IPHI-IPHI_OFF4-1)*NEC - ITMP_ETA-IETA_OFF_EC
          ENDIF
        ENDIF
      ENDIF
C
      IADC = IBRD/8
      IBLS = MOD(IBRD,8)
C
  999 RETURN
      END

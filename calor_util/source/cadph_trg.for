      SUBROUTINE CADPH_TRG(ICRATE,IADC,IBLS,ITWR,IDEP,IETA,IPHI,ILYR,
     &  IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Converts an address in the hardware ADC address
C-                         coordinates to the trigger tower PHysics system
C-                         Similar to CADPH for the precision readout
C-
C   Inputs  : ICRATE    ADC crate number
C             IADC      ADC card number            [0,11]
C             IBLS      BLS card number            [0,7]
C             ITWR      CALIB TOWER                [0,3]
C             IDEP      CALIB depth                [12,13]
C
C   Outputs : IETA     trigger tower eta index     [-20,-1],[1,20]
C             IPHI     trigger tower phi index     [1,32]
C             ILYR     CALIB layer (18=EM, 19=HD)  [18,19]
C             IER       return code: 0 = OK
C                                    1 = invalid input
C-   Controls: none
C-
C-   Created  26-OCT-1993   J Guida
C-   Updated  30-JUN-1995   Jan Guida  Fix - LM should be declared an
C-                                           integer not a logical 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IETA,IPHI,ILYR,ICRATE,IADC,IBLS,ITWR,IDEP,IER
      INTEGER IBRD,LM
      INTEGER IETA_OFF_CC,IETA_OFF_EC,NCC,NEC
      INTEGER IPHI_OFF1,IPHI_OFF2,IPHI_OFF3,IPHI_OFF4
      INTEGER IPHI_OFF5,IPHI_OFF6
      DATA IETA_OFF_CC/1/,IETA_OFF_EC/7/
      DATA NCC/6/,NEC/12/
      DATA IPHI_OFF1/0/,IPHI_OFF2/8/,IPHI_OFF3/16/
      DATA IPHI_OFF4/24/,IPHI_OFF5/32/,IPHI_OFF6/40/
C----------------------------------------------------------------------
      IER = 0
C
      IBRD = IADC*8 + IBLS
C
      IF (ICRATE/10.LE.1) THEN                      ! CC
        IETA = MOD(IBRD,NCC) + IETA_OFF_CC
        IF(MOD(ICRATE,10).EQ.7) IETA = -IETA        ! North side
        IF (ICRATE.EQ.7) THEN
          IPHI = (IBRD+IETA+IETA_OFF_CC)/NCC + IPHI_OFF2 + 1
        ELSEIF (ICRATE.EQ.17) THEN
          IF (IBRD.GT.47) THEN
            IPHI = (IBRD+IETA+IETA_OFF_CC)/NCC - (IPHI_OFF2 - 1)
          ELSE
            IPHI = (IBRD+IETA+IETA_OFF_CC)/NCC - (-IPHI_OFF4 - 1)
          ENDIF
        ELSEIF (ICRATE.EQ.8) THEN
          IF (IBRD.LE.47) THEN
            IPHI = (-IBRD+IETA-IETA_OFF_CC)/NCC + IPHI_OFF2
          ELSE
            IPHI = (-IBRD+IETA-IETA_OFF_CC)/NCC + IPHI_OFF6
          ENDIF
        ELSEIF (ICRATE.EQ.18) THEN
          IPHI = (-IBRD+IETA-IETA_OFF_CC)/NCC + IPHI_OFF4
        ENDIF
      ELSE                                          ! EC
        IETA = MOD(IBRD,NEC) + IETA_OFF_EC
        IF(MOD(ICRATE,10).EQ.7) IETA = -IETA        ! North side
        IF (ICRATE.EQ.27) THEN
          IPHI = (IBRD+IETA+IETA_OFF_EC)/NEC + IPHI_OFF2 + 1
        ELSEIF (ICRATE.EQ.37) THEN
          IPHI = (IBRD+IETA+IETA_OFF_EC)/NEC + IPHI_OFF3 + 1
        ELSEIF (ICRATE.EQ.47) THEN
          IPHI = (IBRD+IETA+IETA_OFF_EC)/NEC + IPHI_OFF4 + 1
        ELSEIF (ICRATE.EQ.57) THEN
          IPHI = (IBRD+IETA+IETA_OFF_EC)/NEC + IPHI_OFF1 + 1
        ELSEIF (ICRATE.EQ.28) THEN
          IPHI = (-IBRD+IETA-IETA_OFF_EC)/NEC + IPHI_OFF2
        ELSEIF (ICRATE.EQ.38) THEN
          IPHI = (-IBRD+IETA-IETA_OFF_EC)/NEC + IPHI_OFF5
        ELSEIF (ICRATE.EQ.48) THEN
          IPHI = (-IBRD+IETA-IETA_OFF_EC)/NEC + IPHI_OFF4
        ELSEIF (ICRATE.EQ.58) THEN
          IPHI = (-IBRD+IETA-IETA_OFF_EC)/NEC + IPHI_OFF3
        ENDIF
C
        LM = MOD(IBRD,12)
        IF (LM.GE.10) THEN                        ! L-type BLS
          IETA = 17 + ITWR
          IF (IETA.EQ.20 .AND. IDEP.EQ.12) IER = -1   ! Disconnected chan.
          IF(MOD(ICRATE,10).EQ.7) IETA = -IETA    ! North side
        ENDIF
      ENDIF
C
      ILYR = 0
      IF (IDEP.EQ.12) ILYR = 18                    ! EM
      IF (IDEP.EQ.13) ILYR = 19                    ! HD
C
C
  999 RETURN
      END

      SUBROUTINE CPHAD_LSR(IETA,IPHI,ICRATE,LADC,LBLS,LTWR,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Converts and address in PHysics system to
C-                         the calib ADC address
C-                         Similar to CPHAD for the precision readout
C-
C   Inputs  : IETA     eta index                   [-20,-1],[1,20]
C             IPHI     phi index                   [1,32]
C
C   Outputs : ICRATE    ADC crate number
C             LADC      ADC card number (clb units) [0,11]
C             LBLS      BLS card number (clb units) [0,7]
C             LTWR      BLS card number (clb units) [0,1]
C                                       0 = ICD, 1 = PHOTO DIODE
C             IER       return code: 0 = OK
C                                    1 = invalid input
C-   Controls: none
C-
C-   Created  28-OCT-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ICRATE,LADC,LBLS,LTWR,IER,IETA,IPHI,ILYR
      INTEGER IADC,IBLS,ITWR,IDEP,IX
C----------------------------------------------------------------------
C
      IER = 0
C
C***  Check for photo diodes
      IF (IETA.GT.14) THEN            ! photo diode
        IF (IPHI.GE.0 .AND. IPHI.LT.12) THEN   ! photo diode
          LADC = IPHI
          LBLS = 0
          LTWR = 1
        ELSE                           ! illegal
          IER = 1
        ENDIF
        GO TO 999
      ELSE IF (IETA.LT.9) THEN         ! illegal
        IER = 1
        GO TO 999
      ENDIF
C
C***  First change back to electronics coordinates (standard ADC,BLS,TWR,DEP)
      ILYR = 9
      CALL CPHAD(ICRATE,IETA,IPHI,ILYR,IADC,IBLS,ITWR,IDEP,IER)
C
      IF ((MOD(IADC*8+IBLS,12).EQ.11) .AND. (ITWR.EQ.2)) THEN   ! ICD
        LADC = (IADC-1) + (IDEP+IBLS-3)/8
        LBLS = MOD(IDEP+IBLS-3,8)
        LTWR = 0
      ELSE
        IER = 1
      ENDIF
C
  999 RETURN
      END

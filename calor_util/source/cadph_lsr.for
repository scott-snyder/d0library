      SUBROUTINE CADPH_LSR(ICRATE,LADC,LBLS,LTWR,IETA,IPHI,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Converts an address in the CALIB packed ADC 
C-                         address coordinates to PHysics system
C-                         Similar to CADPH for the precision readout
C-
C   Inputs  : ICRATE    ADC crate number
C             LADC      ADC card number (clb units) [0,11]
C             LBLS      BLS card number (clb units) [0,7]
C             LTWR      BLS card number (clb units) [0,1]
C                                       0 = ICD, 1 = PHOTO DIODE
C
C   Outputs : IETA     ICD channel eta index     [-20,-1],[1,20]
C             IPHI     ICD channel phi index     [1,32]
C             IER       return code: 0 = OK
C                                    1 = invalid input
C-
C-   Created   5-MAR-1994   Jan Guida
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
      IF (LTWR.EQ.1) THEN
        IF (LBLS.NE.0) THEN
          IER = 1
          GO TO 999
        ENDIF
        IETA = ICRATE
        IPHI = LADC
        GO TO 999
      ENDIF
C
C***  First change back to electronics coordinates (standard ADC,BLS,TWR,DEP)
      IDEP = MOD(LBLS+LADC*8,12)
      IADC = LADC - (-1 + ISIGN(1,IDEP-LBLS-1))/2
      IX = MOD(IADC,3)
      IBLS = IX*(IX+1) + 1
      ITWR = 2
C
      CALL CADPH(ICRATE,IADC,IBLS,ITWR,IDEP,IETA,IPHI,ILYR,IER)
C
  999 RETURN
      END

      SUBROUTINE CADPH_LV0(ICRATE,IADC,IBLS,ITWR,IETA,IPHI,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Converts an address in the CALIB packed ADC 
C-                         address coordinates to LEVEL-0 PHysics system
C-                         Similar to CADPH for the precision readout
C-
C   Inputs  : ICRATE    ADC crate number
C             IADC      ADC card number            [0,11]
C             IBLS      BLS card number            [0,7]
C             ITWR      BLS TOWER                  [0,3]
C
C   Outputs : IETA     LEVEL-0 channel eta index     [-20,-1],[1,20]
C             IPHI     LEVEL-0 channel phi index     [1,32]
C             IER       return code: 0 = OK
C                                    1 = invalid input
C-
C-   Created   5-MAR-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IETA,IPHI,ICRATE,IADC,IBLS,ITWR,IER
C----------------------------------------------------------------------
      logical lfirst
      data lfirst/.true./

      if (lfirst) then
      call intmsg(' DUMMY VERSION OF CADPH_LV0 CALLED')
      lfirst = .false.
      endif

  999 RETURN
      END

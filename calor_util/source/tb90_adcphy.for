      SUBROUTINE TB90_ADCPHY(IDATA,NUMG,ETAV,PHIV,LAYERV,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Converts TB90 cal address in ADC form to
C-                         PHYSICS form.
C-   Inputs  : IDATA - the packed ADC data/address word
C-   Outputs : NUMG  - the number of corresponding ganged PHYSICS
C-                     addresses.
C-             ETAV  - vector of ETA indices
C-             PHIV  - vector of PHI indices
C-             LAYERV- vector of LAYER indices
C-             IER   - Error code
C-                     IER = 0 - OK
C-                         = -1 -  NUMG=0
C-   Controls: none
C-
C-   Created  14-DEC-1989   
C-   by Andrew P. White
C-   Updated   5-MAR-1990   Chip Stewart   - USE CADUPK INSTEAD OF ADCBIT
C----------------------------------------------------------------------

      IMPLICIT NONE
C GANGING
      INTEGER NUMG,ETAV(20),PHIV(20),LAYERV(20)
C CAD ADDRESSES
      INTEGER IDATA,CARD,BLS,TOWER,DEPTH,SEQ
      INTEGER ICRATE,CRATE,SCALE,NEGLIM,IER
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
C ONLY ONE CRATE AT NWA
      DATA ICRATE/0/
C----------------------------------------------------------------------
      IER = 0
C
C--- First convert to a SEQUENTIAL ADC address.
C
      CALL CADUPK(ICRATE,IDATA,CRATE,CARD,BLS,TOWER,DEPTH,SCALE,
     &  NEGLIM)
C
C--- Now get the SEQUENTIAL ADC address
C
      SEQ= CARD*NDEPTC*NEFC*NBLSC + 
     &  BLS*NDEPTC*NEFC + 
     &  TOWER*NDEPTC + 
     &  DEPTH + 1
C
C--- Finally convert to a PHYSICS address.
C
      CALL TB90_SEQPHY(SEQ,NUMG,ETAV,PHIV,LAYERV)
      IF (NUMG.EQ.0) IER = -1
C
  999 RETURN
      END

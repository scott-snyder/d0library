      SUBROUTINE TB90_ADCADR(SEQADC,CRATE,ADCA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the ADC address for a TB90 channel
C-                         given the sequential ADC position.
C-   Inputs  : Sequential ADC position
C-   Outputs : ADC address (crate, card, bls, tower, layer)
C-   Controls: none
C-
C-   !!!!!!!!!!!!  ASSUMES SEQADC GOES FROM 1-4608     !!!!!!!!!!!!!!!!1
C-
C-   Created  28-NOV-1989   
C-   by Andrew P. White
C-   FIXED 4-JUN-1990, KATHY STREETS
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER SEQADC,CRATE,ADCA,SEQN
      INTEGER CARD,CHAN,BLS,BLSM,TWR,LAYER
C----------------------------------------------------------------------
C
      SEQN=SEQADC-1
      CARD=SEQN/384
      CRATE=(CARD+1)/12
      CHAN=MOD(SEQN,384)
      BLS=(CHAN)/48
      BLSM=CHAN-(BLS*48)
      TWR=BLSM/12
      LAYER=MOD(SEQN,12)
C
      ADCA=CARD*2**11 + BLS*2**8 + TWR*2**6 + LAYER*2**2
C
  999 RETURN
      END

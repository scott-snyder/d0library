      SUBROUTINE TB90_ADCBIT(ADCA,CARD,BLS,TOWER,LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decode packed ADC address for TB90
C-
C-   Inputs  : Packed ADC address in ADCA
C-   Outputs : CARD (0-11)  : ADC card number in crate
C-             BLS  (0-7)   : base line subtractor
C-             TOWER(0-3)   : Readout tower
C-             LAYER(0-11)  : Depth layer
C-   Controls: none
C-
C-   Created  29-NOV-1989   
C-   by Andrew P. White
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ADCA,CARD,BLS,TOWER,LAYER
C----------------------------------------------------------------------
C
      CALL GETBYT(ADCA,17,5,CARD)
      CALL GETBYT(ADCA,22,3,BLS)
      CALL GETBYT(ADCA,25,2,TOWER)
      CALL GETBYT(ADCA,27,4,LAYER)
C
  999 RETURN
      END

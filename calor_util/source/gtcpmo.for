      SUBROUTINE GTCPMO(CRATE,ADR,VAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns values of higher moments (skewness and
C-                            kurtosis) from calib - pedestals
C-
C-   Inputs  : CRATE - ADC crate number
C-             ADR   - hardware address
C-   Outputs : VAL(2) - Value of skewness/kurtosis
C-   Controls: none
C-
C-   Created  31-MAR-1992   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LCPMO
      INTEGER NEG,DEP,TWR,BLS,ADR,CARD,IGN,CHAN
      INTEGER CRATE,KADR
      INTEGER GZCPMO
C
      INCLUDE 'D0$PARAMS:PRTPDG.DEF'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      REAL VAL(*)
C
C----------------------------------------------------------------------
      LCPMO = GZCPMO()
      IF ( LCPMO.GT.0 ) THEN
        KADR = ISHFT(ADR,16)
        IGN = 0
        CALL CADUPK(CRATE,KADR,CRATE,CARD,BLS,TWR,DEP,IGN,NEG)
C
        CHAN = CARD*384+BLS*48+TWR*12+DEP         !ADC channel number
C
        VAL(1) = C(LCPMO+NHEAD+2*CHAN+1)
        VAL(2) = C(LCPMO+NHEAD+2*CHAN+2)
      ELSE                                        !Moment bank does not exist
        VAL(1)=-1
        VAL(2)=-1    !No bank
      ENDIF
C
  999 RETURN
      END

      SUBROUTINE GTCGTM(CRATE,ADR,VAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns values of higher moments (skewness and
C-                            kurtosis) from calib - trigger gains
C-
C-   Inputs  : CRATE - ADC crate number
C-             ADR   - hardware address
C-   Outputs : VAL(2) - Value of skewness/kurtosis
C-   Controls: none
C-
C-   Created  19-OCT-1993   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LCGTM
      INTEGER NEG,DEP,TWR,BLS,ADR,ADC,IGN,CHAN
      INTEGER CRATE,KADR,IBRD,LM,IB
      INTEGER GZCGTM
C
      INCLUDE 'D0$PARAMS:PRTPDG.DEF'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      REAL VAL(*)
C
C----------------------------------------------------------------------
      LCGTM = GZCGTM()
      IF ( LCGTM.GT.0 ) THEN
        KADR = ISHFT(ADR,16)
        IGN = 0
        CALL CADUPK(CRATE,KADR,CRATE,ADC,BLS,TWR,DEP,IGN,NEG)
        IF (IGN.EQ.0) THEN              ! x8
C
          CHAN = ADC*64 + BLS*8 + TWR*2 + (DEP-12)
C
          VAL(1) = C(LCGTM+NHEAD+2*CHAN+1)
          VAL(2) = C(LCGTM+NHEAD+2*CHAN+2)
        ELSE                               !Moment bank does not exist for x1
          VAL(1)=-1
          VAL(2)=-1    !No bank
        ENDIF
      ELSE                                        !Moment bank does not exist
        VAL(1)=-1
        VAL(2)=-1    !No bank
      ENDIF
C
  999 RETURN
      END

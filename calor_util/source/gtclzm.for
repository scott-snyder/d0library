      SUBROUTINE GTCLZM(CRATE,ADR,VAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns values of higher moments (skewness and
C-                            kurtosis) from calib - ICD laser gains
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
      INTEGER LCLZM
      INTEGER NEG,DEP,TWR,BLS,ADR,ADC,IGN,CHAN
      INTEGER CRATE,KADR
      INTEGER GZCLZM
C
      INCLUDE 'D0$PARAMS:PRTPDG.DEF'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      REAL VAL(*)
C
C----------------------------------------------------------------------
      LCLZM = GZCLZM()
      IF ( LCLZM.GT.0 ) THEN
        KADR = ISHFT(ADR,16)
        CALL CADUPK(CRATE,KADR,CRATE,ADC,BLS,TWR,DEP,IGN,NEG)
        IF (IGN.EQ.1) THEN              ! x1
C
          CHAN = ADC*9+BLS+TWR+(BLS+7)/8     !ADC channel number
C
          VAL(1) = C(LCLZM+NHEAD+2*CHAN+1)
          VAL(2) = C(LCLZM+NHEAD+2*CHAN+2)
        ELSE                            !Moment bank does not exist for x8
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

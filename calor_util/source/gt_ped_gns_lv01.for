      SUBROUTINE GT_PED_GNS_LV01(TASK,CRATE,ADR,VAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns Values and Sigmas for PED/GAINS
C-                            Level-0 Channels
C-
C-   Inputs  : TASK =1,2 PEDS, =3 Gains
C-             CRATE = ADC crate number
C-             ADR = hardware address
C-
C-   Outputs : VAL(2) Value of PED/GAINS and SIGMA
C-
C-   Created   3-MAR-1994   Jan Guida    Taken from GT_PED_GNS_LSR1
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TASK,LZFIND,LCL0
      INTEGER NEG,DEP,TWR,BLS,ADR,ADC,IGN,CHAN
      INTEGER CRATE,KADR
C
      INCLUDE 'D0$PARAMS:PRTPDG.DEF'
C
      REAL VAL(*)
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPL0.LINK'
      INCLUDE 'D0$LINKS:IZCGL0.LINK'
C----------------------------------------------------------------------
      KADR = ISHFT(ADR,16)
      CALL CADUPK(CRATE,KADR,CRATE,ADC,BLS,TWR,DEP,IGN,NEG)
C
      CHAN = ADC*8+BLS                    !ADC channel number
      IF ( TASK.LT.3 ) THEN      !Pedestals
        LCPDH = LZFIND(IDVSTP,LCPDH,CRATE,9)   !Finds Bank with Crate
        LCL0 = LC(LCPDH-IZCPL0)
      ELSE                       !Gains
        LCGNH = LZFIND(IDVSTP,LCGNH,CRATE,9)   !Finds Bank with Crate
        LCL0 = LC(LCGNH-IZCGL0)
      ENDIF
C
      IF ( LCL0.GT.0 ) THEN
        VAL(1) = C(LCL0+NHEAD+2*CHAN+1)
        VAL(2) = C(LCL0+NHEAD+2*CHAN+2)
      ELSE
        VAL(1)=-1
        VAL(2)=-1    !No bank
      ENDIF
C
  999 RETURN
      END

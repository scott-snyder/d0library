      SUBROUTINE GT_PED_GNS_TRG1(TASK,CRATE,ADR,VAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns Values and Sigmas for PED/GAINS
C-                            Calorimeter Trigger Channels
C-
C-   Inputs  : TASK =1,2 PEDS, =3 Gains
C-             CRATE = ADC crate number
C-             ADR = hardware address
C-
C-   Outputs : VAL(2) Value of PED/GAINS and SIGMA
C-
C-   Created  19-OCT-1993   Jan Guida    Taken from GT_PED_GNS1
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TASK,LINK,LZFIND
      INTEGER NEG,DEP,TWR,BLS,ADR,ADC,IGN,CHAN
      INTEGER CRATE,KADR,IBRD,LM,IB
C
      INCLUDE 'D0$PARAMS:PRTPDG.DEF'
C
      REAL VAL(*)
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPTR.LINK'
      INCLUDE 'D0$LINKS:IZCGTR.LINK'
C----------------------------------------------------------------------
      KADR = ISHFT(ADR,16)
      CALL CADUPK(CRATE,KADR,CRATE,ADC,BLS,TWR,DEP,IGN,NEG)
C
      IBRD = ADC*8+BLS
      IB = IBRD/12
      IF (CRATE/10.LE.1) LM = 0
      CHAN = ADC*64 + BLS*8 + TWR*2 + (DEP-12)
C
      IF ( TASK.LT.3 ) THEN      !Pedestals
        LCPDH = LZFIND(IDVSTP,LCPDH,CRATE,9)   !Finds Bank with Crate
        LINK = LC(LCPDH-IZCPTR)
      ELSE                       !Gains
        LCGNH = LZFIND(IDVSTP,LCGNH,CRATE,9)   !Finds Bank with Crate
        LINK = LC(LCGNH-IZCGTR)
      ENDIF
C
      IF ( LINK.GT.0 ) THEN
        VAL(1) = C(LINK+NHEAD+2*CHAN+1)
        VAL(2) = C(LINK+NHEAD+2*CHAN+2)
      ELSE
        VAL(1)=-1
        VAL(2)=-1    !No bank
      ENDIF
C
  999 RETURN
      END

      SUBROUTINE GT_PED_GNS_LSR1(TASK,CRATE,ADR,VAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns Values and Sigmas for PED/GAINS
C-                            ICD Laser Channels
C-
C-   Inputs  : TASK =1,2 PEDS, =3 Gains, =10 corrected gains
C-             CRATE = ADC crate number
C-             ADR = hardware address
C-
C-   Outputs : VAL(2) Value of PED/GAINS and SIGMA
C-
C-   Created  19-OCT-1993   Jan Guida    Taken from GT_PED_GNS1
C-   Updated   2-NOV-1994   Jan Guida  Add corrected gains bank 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TASK,LINK,LZFIND,LCGLZ
      INTEGER NEG,DEP,TWR,BLS,ADR,ADC,IGN,CHAN
      INTEGER CRATE,KADR
C
      INCLUDE 'D0$PARAMS:PRTPDG.DEF'
C
      REAL VAL(*)
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCGLZ.LINK'
      INCLUDE 'D0$LINKS:IZCLZ1.LINK'
      INCLUDE 'D0$LINKS:IZCLZ8.LINK'
      INCLUDE 'D0$LINKS:IZCLZC.LINK'
C----------------------------------------------------------------------
      KADR = ISHFT(ADR,16)
      CALL CADUPK(CRATE,KADR,CRATE,ADC,BLS,TWR,DEP,IGN,NEG)
C
      CHAN = ADC*9+BLS+TWR+(BLS+7)/8                   !ADC channel number
      IF ( TASK.LT.3 ) THEN      !Pedestals
        VAL(1)=-1
        VAL(2)=-1
        CALL INTMSG(' -W-GT_PED_GNS_LSR1  called for pedestal run')
        GO TO 999
      ELSE                       !Gains
        LCGNH = LZFIND(IDVSTP,LCGNH,CRATE,9)   !Finds Bank with Crate
        LCGLZ = LC(LCGNH-IZCGLZ)
        IF (TASK.NE.10) THEN
          IF ( IGN.EQ.0 ) THEN     !X8 gains
            LINK = LC(LCGLZ-IZCLZ8)
          ELSEIF ( IGN.EQ.1 ) THEN !X1 gains
            LINK = LC(LCGLZ-IZCLZ1)
          ENDIF
        ELSE                      ! Corrected gains
          LINK = LC(LCGLZ-IZCLZC)
        ENDIF
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

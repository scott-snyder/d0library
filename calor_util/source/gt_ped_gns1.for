      SUBROUTINE GT_PED_GNS1(TASK,CRATE,ADR,VAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns Values and Sigmas for PED/GAINS
C-
C-   Inputs  : TASK =1,2 PEDS, =3 Gains
C-             CRATE = ADC crate number
C-             ADR = hardware address
C-
C-   Outputs : VAL(2) Value of PED/GAINS and SIGMA
C-
C-   Created  25-DEC-1987   Rajendran Raja
C-   Modified  2-MAR-1989   Jan Guida,   PRTPDG now in D0$PARAMS
C-   Modified  5-JUL-1989   Jan Guida,   Use Calor_util routines instead
C-                                           of TB87 routines
C-   Updated  13-NOV-1990   Jan Guida  Added CRATE argument, and ability 
C-                                      to do multiple crates 
C-   Updated   7-JUN-1991   Jan Guida  Pedestals:  TASK = 1 or 2 (add 2)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TASK,LINKH,LINK,LZFIND
      INTEGER NEG,LYR,TWR,BLS,ADR,CARD,IGN,CHAN
      INTEGER CRATE,KADR
C
      INCLUDE 'D0$PARAMS:PRTPDG.DEF'
C
      REAL VAL(*)
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPD1.LINK'
      INCLUDE 'D0$LINKS:IZCPD8.LINK'
      INCLUDE 'D0$LINKS:IZCGN1.LINK'
      INCLUDE 'D0$LINKS:IZCGN8.LINK'
C----------------------------------------------------------------------
      KADR = ISHFT(ADR,16)
      CALL CADUPK(CRATE,KADR,CRATE,CARD,BLS,TWR,LYR,IGN,NEG)
C
      CHAN = BLS*48+TWR*12+LYR               !ADC channel number
      IF ( TASK.LT.3 ) THEN      !Pedestals
        LCPDH = LZFIND(IDVSTP,LCPDH,CRATE,9)   !Finds Bank with Crate
        IF ( IGN.EQ.0 ) THEN     !X8 gains
          LINKH = LC(LCPDH-IZCPD8)
        ELSEIF ( IGN.EQ.1 ) THEN !X1 gains
          LINKH = LC(LCPDH-IZCPD1)
        ENDIF
      ELSE                       !Gains
        LCGNH = LZFIND(IDVSTP,LCGNH,CRATE,9)   !Finds Bank with Crate
        IF ( IGN.EQ.0 ) THEN     !X8 gains
          LINKH = LC(LCGNH-IZCGN8)
        ELSEIF ( IGN.EQ.1 ) THEN !X1 gains
          LINKH = LC(LCGNH-IZCGN1)
        ENDIF
      ENDIF
C
      LINK  = LZFIND(IDVSTP,LINKH,CARD,11)   !Finds Bank with Card
      IF ( LINK.GT.0 ) THEN
        VAL(1) = C(LINK+NHEAD+2*CHAN+1)
        VAL(2) = C(LINK+NHEAD+2*CHAN+2)
      ELSE
        VAL(1)=-1
        VAL(2)=-1    !No bank
      ENDIF
  999 RETURN
      END

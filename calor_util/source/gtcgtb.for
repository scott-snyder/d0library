      SUBROUTINE GTCGTB(ICRATE,ICARD,NBAD,BAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return bad flag word for all channels in 
C-                         ADC crate ICRATE, ADC card ICARD, BANK = CGTB
C-
C-   Inputs  : ICRATE  - ADC crate number
C-             ICARD   - ADC card number
C-   Outputs : NBAD    - number of bad channels found
C-   Controls: BAD(64) - flags for all possible channels (0->good)
C-
C-   Created  13-JUL-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCGTB.LINK'
      INCLUDE 'D0$LINKS:IZCGTR.LINK'
      INTEGER ICRATE,ICARD,NBAD,BAD(0:63)
      INTEGER GZCGNH,GZCGTR,GZCGTB,LCGTR,LCGTB
      INTEGER NTOTBD,IADR,IADC,IBLS,ITWR,IDEP,ISCL,NEG
      INTEGER ICHAN,I
      INTEGER LZFIND
C----------------------------------------------------------------------
      NBAD = 0
      CALL VZERO(BAD,64)
C
      LCGNH = GZCGNH()
      LCGNH = LZFIND(IDVSTP,LCGNH,ICRATE,9)   !Finds Bank with Crate
      IF (LCGNH.LE.0) THEN
        CALL INTMSG(' GTCGTB:  Pedestal header bank does not exist')
        GO TO 999
      ENDIF
C      LCGTR = GZCGTR()
      LCGTR = LC(LCGNH-IZCGTR)
      IF (LCGTR.LE.0) THEN
        CALL INTMSG(' GTCGTB:  Bank CGTR does not exist')
        GO TO 999
      ENDIF
C      LCGTB = GZCGTB()
      LCGTB = LC(LCGTR-IZCGTB)
      IF (LCGTB.LE.0) THEN
        CALL INTMSG(' GTCGTB:  Bank CGTB does not exist')
        GO TO 999
      ENDIF
C
      NTOTBD = IC(LCGTB+1)
      IF (NTOTBD.LE.0) GO TO 999
      DO I = 1,NTOTBD
        IADR = IC(LCGTB+I+1)
        CALL CADUPK(ICRATE,IADR,ICRATE,IADC,IBLS,ITWR,IDEP,ISCL,NEG)
        IF (IADC.NE.ICARD) GO TO 100
        NBAD = NBAD + 1
        ICHAN = IBLS*8 + ITWR*2 + (IDEP-12)
        BAD(ICHAN) = IAND(IC(LCGTB+I+1),'FFFF'X)
  100   CONTINUE
      ENDDO
C
  999 RETURN
      END

      SUBROUTINE GTCGB1(CRATE,ICARD,NBAD,BAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return bad flag word for all chans in ADC
C-                          card ICARD, BANK = CGN1
C-
C-   Inputs  : CRATE    = ADC crate number
C-             ICARD    = ADC card number
C-   Outputs : NBAD     = number of bad channels found
C-             BAD(384) = flags for all possible channels (0->good)
C-
C-   Created   9-FEB-1989   Jan Guida,  adopted from CPBD.
C-   Modified  5-JUL-1989   Jan Guida,  Use Calor_util routines instead
C-                                          of TB87_routines
C-   Updated   7-MAR-1991   Jan Guida  Added CRATE argument, and ability 
C-                                      to do multiple crates 
C-   Updated   2-MAR-1993   Jan Guida  Increment NBAD after checking ADC number 
C-   Updated  13-NOV-1993   Jan Guida  Replace AND with IAND (FLINT) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ICARD,NBAD,BAD(384)
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCGN1.LINK'
      INCLUDE 'D0$LINKS:IZCGB1.LINK'
      INTEGER GZCGNH,LCGN1,LCGB1,LZFIND
      INTEGER NTOTBD,IADR,ADC,BLS,TWR,LYR,SCL,NEG,CHAN
      INTEGER I,CRATE
C----------------------------------------------------------------------
      NBAD = 0
      CALL VZERO(BAD,384)
C
      LCGNH = GZCGNH()
      LCGNH = LZFIND(IDVSTP,LCGNH,CRATE,9)   !Finds Bank with Crate
      IF (LCGNH.LE.0) THEN
        CALL INTMSG(' GTCGB1:  Gains header bank does not exist')
        GO TO 999
      ENDIF
      LCGN1 = LC(LCGNH-IZCGN1)
      IF (LCGN1.LE.0) THEN
        CALL INTMSG(' GTCGB1:  Bank CGN1 does not exist')
        GO TO 999
      ENDIF
      LCGB1 = LC(LCGN1-IZCGB1)
      IF (LCGB1.LE.0) THEN
        CALL INTMSG(' GTCGB1:  Bank CGB1 does not exist')
        GO TO 999
      ENDIF
C
      NTOTBD = IC(LCGB1+1)
      IF ( NTOTBD.LE.0 ) GOTO 999
      DO 100 I = 1, NTOTBD
        IADR = IC(LCGB1+I+1)
        CALL CADUPK(CRATE,IADR,CRATE,ADC,BLS,TWR,LYR,SCL,NEG)
        IF(ADC.NE.ICARD) GOTO 100
        NBAD = NBAD + 1
        CHAN = 48*BLS + 12*TWR + LYR
        BAD(CHAN+1) = IAND(IC(LCGB1+I+1),'FFFF'X)
  100 CONTINUE
  999 RETURN
      END

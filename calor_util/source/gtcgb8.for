      SUBROUTINE GTCGB8(CRATE,ICARD,NBAD,BAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return bad flag word for all chans in ADC
C-                          card ICARD, BANK = CGN8
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
C-   Updated  22-Jan-1996   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ICARD,NBAD,BAD(384)
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCGN8.LINK'
      INCLUDE 'D0$LINKS:IZCGB8.LINK'
      INTEGER GZCGNH,LCGN8,LCGB8,LZFIND
      INTEGER NTOTBD,IADR,ADC,BLS,TWR,LYR,SCL,NEG,CHAN
      INTEGER I,CRATE
C----------------------------------------------------------------------
      NBAD = 0
      CALL VZERO(BAD,384)
C
      LCGNH = GZCGNH()
      LCGNH = LZFIND(IDVSTP,LCGNH,CRATE,9)   !Finds Bank with Crate
      IF (LCGNH.LE.0) THEN
        CALL INTMSG(' GTCGB8:  Gains header bank does not exist')
        GO TO 999
      ENDIF
      LCGN8 = LC(LCGNH-IZCGN8)
      IF (LCGN8.LE.0) THEN
        CALL INTMSG(' GTCGB8:  Bank CGN8 does not exist')
        GO TO 999
      ENDIF
      LCGB8 = LC(LCGN8-IZCGB8)
      IF (LCGB8.LE.0) THEN
        CALL INTMSG(' GTCGB8:  Bank CGB8 does not exist')
        GO TO 999
      ENDIF
C
      NTOTBD = IC(LCGB8+1)
      IF ( NTOTBD.LE.0 ) GOTO 999
      DO 100 I = 1, NTOTBD
        IADR = IC(LCGB8+I+1)
        CALL CADUPK(CRATE,IADR,CRATE,ADC,BLS,TWR,LYR,SCL,NEG)
        IF(ADC.NE.ICARD) GOTO 100
        NBAD = NBAD + 1
        CHAN = 48*BLS + 12*TWR + LYR
        BAD(CHAN+1) = IAND(IC(LCGB8+I+1), 65535) ! 0xffff
  100 CONTINUE
  999 RETURN
      END

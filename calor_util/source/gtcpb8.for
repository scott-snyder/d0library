      SUBROUTINE GTCPB8(CRATE,ICARD,NBAD,BAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return bad flag word for all chans in ADC
C-                          card ICARD, BANK = CPD8
C-
C-   Inputs  : CRATE    = ADC crate number
C-             ICARD    = ADC card number
C-   Outputs : NBAD     = number of bad channels found
C-             BAD(384) = flags for all possible channels (0->good)
C-
C-   Created  29-JUN-1988   A.M.Jonckheere  (subroutine CPBD).
C-   Created  24-JAN-1989   Jan Guida,  adopted from CPBD.
C-   Modified  5-JUL-1989   Jan Guida,  Use Calor_util routines instead
C-                                          of TB87_routines
C-   Updated  31-OCT-1990   Jan Guida  Fix input address for CADUPK 
C-   Updated  23-FEB-1991   Jan Guida  Fix address 
C-   Updated  24-FEB-1991   Jan Guida  Added CRATE argument, and ability 
C-                                      to do multiple crates 
C-   Updated   2-MAR-1993   Jan Guida  Increment NBAD after checking ADC number 
C-   Updated  13-NOV-1993   Jan Guida  Replace AND with IAND (FLINT) 
C-   Updated  22-Jan-1996   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ICARD,NBAD,BAD(0:383)
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPD8.LINK'
      INCLUDE 'D0$LINKS:IZCPB8.LINK'
      INTEGER GZCPDH,LCPD8,LCPB8,LZFIND
      INTEGER NTOTBD,IADR,ADC,BLS,TWR,LYR,SCL,NEG,CHAN
      INTEGER I,CRATE
C----------------------------------------------------------------------
      NBAD = 0
      CALL VZERO(BAD,384)
C
      LCPDH = GZCPDH()
      LCPDH = LZFIND(IDVSTP,LCPDH,CRATE,9)   !Finds Bank with Crate
      IF (LCPDH.LE.0) THEN
        CALL INTMSG(' GTCPB8:  Pedestal header bank does not exist')
        GO TO 999
      ENDIF
      LCPD8 = LC(LCPDH-IZCPD8)
      IF (LCPD8.LE.0) THEN
        CALL INTMSG(' GTCPB8:  Bank CPD8 does not exist')
        GO TO 999
      ENDIF
      LCPB8 = LC(LCPD8-IZCPB8)
      IF (LCPB8.LE.0) THEN
        CALL INTMSG(' GTCPB8:  Bank CPB8 does not exist')
        GO TO 999
      ENDIF
C
      NTOTBD = IC(LCPB8+1)
      IF ( NTOTBD.LE.0 ) GOTO 999
      DO 100 I = 1, NTOTBD
        IADR = IC(LCPB8+I+1)
        CALL CADUPK(CRATE,IADR,CRATE,ADC,BLS,TWR,LYR,SCL,NEG)
        IF(ADC.NE.ICARD) GOTO 100
        NBAD = NBAD + 1
        CHAN = 48*BLS + 12*TWR + LYR
        BAD(CHAN) = IAND(IC(LCPB8+I+1), 65535) ! 0xffff
  100 CONTINUE
  999 RETURN
      END

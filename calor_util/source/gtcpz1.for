      SUBROUTINE GTCPZ1(CRATE,ICARD,VALUE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads zero suppression bank, CPZ1
C-
C-   Inputs  : CRATE    = ADC crate number
C-             ICARD    = ADC card number
C-   Outputs : VALUE - Array of pedestals and limits for card ICARD,
C-                        Peds = VALUE(1,*), Limits = VALUE(2,*)
C-   Controls: 
C-
C-   Created  14-DEC-1989   Jan Guida
C-   Updated  24-FEB-1991   Jan Guida  Added CRATE argument, and ability 
C-                                      to do multiple crates 
C-   Updated   1-MAR-1991   Jan Guida  Changed  JBYT to IBITS
C-   Updated  17-Mar-1992   Herbert Greenlee
C-      Fix byte order
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPD1.LINK'
      INCLUDE 'D0$LINKS:IZCPZ1.LINK'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER CRATE,ICARD
      INTEGER GZCPDH,LCPD1,LCPZ1,LT,LZFIND
      INTEGER ICHAN,KCARD,FST_CARD,LST_CARD
      INTEGER FULWRD
      INTEGER*2 HLFWRD(2)
      INTEGER*2 VALUE(2,384)
      EQUIVALENCE (FULWRD,HLFWRD)
C----------------------------------------------------------------------
      CALL VZERO(VALUE,384)
C
      LCPDH = GZCPDH()
      LCPDH = LZFIND(IDVSTP,LCPDH,CRATE,9)   !Finds Bank with Crate
      IF (LCPDH.LE.0) THEN
        CALL INTMSG(' GTCPZ1:  Pedestal header bank does not exist')
        GO TO 999
      ENDIF
      LCPD1 = LC(LCPDH-IZCPD1)
      IF (LCPD1.LE.0) THEN
        CALL INTMSG(' GTCPZ1:  Bank CPD1 does not exist')
        GO TO 999
      ENDIF
      LCPZ1 = LC(LCPD1-IZCPZ1)
      IF (LCPZ1.LE.0) THEN
        CALL INTMSG(' GTCPZ1:  Bank CPZ1 does not exist')
        GO TO 999
      ENDIF
C
      FST_CARD = IC(LCPZ1+1)
      LST_CARD = IC(LCPZ1+2)
      IF(ICARD.LT.FST_CARD .OR. ICARD.GT.LST_CARD)GOTO 999
      LT = LCPZ1+5 + (ICARD-FST_CARD)*385
      KCARD = IC(LT+1)
      IF(ICARD.NE.KCARD) GOTO 999
      DO 10 ICHAN = 1,384
        FULWRD = IC(LT+ICHAN+1)
        VALUE(1,ICHAN) = HLFWRD(WORD2)      ! Pedestal
        VALUE(2,ICHAN) = HLFWRD(WORD1)      ! Limit
   10 CONTINUE
  999 RETURN
      END

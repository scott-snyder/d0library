      SUBROUTINE BKVRHT(LVRHT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book USER bank, called VRHT that has all hits
C-           in narrow road. Book bank with initial size of 24 wire-layers with
C-           5 hits on each.  Also, include in header section road definition
C-           and quantities computed by VHITS_IN_ROAD
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  16-JUN-1994   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZUSER.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C I/O:
      INTEGER LVRHT
C Locals:
      LOGICAL FIRST
      INTEGER WORDS_PER_HIT,DEFAULT_HITS,LUSER,NIO,NHEAD,ND,NL,NS
C Data:
      DATA DEFAULT_HITS/7/
      DATA WORDS_PER_HIT/6/
      DATA NHEAD/64/
      DATA NL/2/
      DATA NS/2/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      LUSER = LQ(LHEAD-IZUSER)
      IF (LUSER .GT. 0.) CALL MZDROP(IXCOM,LUSER,'L')
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL MZFORM('VRHT','5F 59I / 5F 1B',NIO)
      ENDIF
      ND = NHEAD + 24*DEFAULT_HITS*WORDS_PER_HIT
      CALL MZBOOK(IXMAIN,LVRHT,LHEAD,-IZUSER,'VRHT',NL,NS,ND,NIO,0)
      IQ(LVRHT+6) = WORDS_PER_HIT
      IQ(LVRHT+12)= NHEAD
      IQ(LVRHT+13)= 0
  999 RETURN
      END

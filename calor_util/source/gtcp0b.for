      SUBROUTINE GTCP0B(ICRATE,ICARD,NBAD,BAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return bad flag word for all channels in 
C-                         ADC crate ICRATE, ADC card ICARD, BANK = CP0B
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
      INCLUDE 'D0$LINKS:IZCPL0.LINK'
      INCLUDE 'D0$LINKS:IZCP0B.LINK'
      INTEGER ICRATE,ICARD,NBAD,BAD(0:63)
      INTEGER GZCPDH,GZCPL0,GZCP0B,LCPL0,LCP0B
      INTEGER NTOTBD,IADR,IADC,IBLS,ITWR,IDEP,ISCL,NEG
      INTEGER ICHAN,I
      INTEGER LZFIND
C----------------------------------------------------------------------
      NBAD = 0
      CALL VZERO(BAD,64)    ! ??????????????????
C
      LCPDH = GZCPDH()
      LCPDH = LZFIND(IDVSTP,LCPDH,ICRATE,9)   !Finds Bank with Crate
      IF (LCPDH.LE.0) THEN
        CALL INTMSG(' GTCP0B:  Pedestal header bank does not exist')
        GO TO 999
      ENDIF
C      LCPL0 = GZCPL0()
      LCPL0 = LC(LCPDH-IZCPL0)
      IF (LCPL0.LE.0) THEN
        CALL INTMSG(' GTCP0B:  Bank CPL0 does not exist')
        GO TO 999
      ENDIF
C      LCP0B = GZCP0B()
      LCP0B = LC(LCPL0-IZCP0B)
      IF (LCP0B.LE.0) THEN
        CALL INTMSG(' GTCP0B:  Bank CP0B does not exist')
        GO TO 999
      ENDIF
C
      NTOTBD = IC(LCP0B+1)
      IF (NTOTBD.LE.0) GO TO 999
      DO I = 1,NTOTBD
        IADR = IC(LCP0B+I+1)
        CALL CADUPK(ICRATE,IADR,ICRATE,IADC,IBLS,ITWR,IDEP,ISCL,NEG)
        IF (IADC.NE.ICARD) GO TO 100
        NBAD = NBAD + 1
        ICHAN = IBLS*8 + ITWR*2    ! ??????????
        BAD(ICHAN) = IAND(IC(LCP0B+I+1),'FFFF'X)
  100   CONTINUE
      ENDDO
C
  999 RETURN
      END

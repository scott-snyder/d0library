      SUBROUTINE GTCLB1(ICRATE,ICARD,NBAD,BAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return bad flag word for all channels in 
C-                         ADC crate ICRATE, ADC card ICARD, BANK = CLB1
C-
C-   Inputs  : ICRATE  - ADC crate number
C-             ICARD   - ADC card number
C-   Outputs : NBAD    - number of bad channels found
C-   Controls: BAD(8)  - flags for all possible channels (0->good)
C-
C-   Created  13-JUL-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCGLZ.LINK'
      INCLUDE 'D0$LINKS:IZCLZ1.LINK'
      INCLUDE 'D0$LINKS:IZCLB1.LINK'
      INTEGER ICRATE,ICARD,NBAD,BAD(0:7)
      INTEGER GZCGNH,GZCGLZ,GZCLZ1,GZCLB1,LCGLZ,LCLZ1,LCLB1
      INTEGER NTOTBD,IADR,IADC,IBLS,ITWR,IDEP,ISCL,NEG
      INTEGER ICHAN,I
      INTEGER LZFIND
C----------------------------------------------------------------------
      NBAD = 0
      CALL VZERO(BAD,9)
C
      LCGNH = GZCGNH()
      LCGNH = LZFIND(IDVSTP,LCGNH,ICRATE,9)   !Finds Bank with Crate
      IF (LCGNH.LE.0) THEN
        CALL INTMSG(' GTCLB1:  Pedestal header bank does not exist')
        GO TO 999
      ENDIF
C      LCGLZ = GZCGLZ()
      LCGLZ = LC(LCGNH-IZCGLZ)
      IF (LCGLZ.LE.0) THEN
        CALL INTMSG(' GTCLB1:  Bank CGLZ does not exist')
        GO TO 999
      ENDIF
C      LCLZ1 = GZCLZ1()
      LCLZ1 = LC(LCGLZ-IZCLZ1)
      IF (LCLZ1.LE.0) THEN
        CALL INTMSG(' GTCLB1:  Bank CLZ1 does not exist')
        GO TO 999
      ENDIF
C      LCLB1 = GZCLB1()
      LCLB1 = LC(LCLZ1-IZCLB1)
      IF (LCLB1.LE.0) THEN
        CALL INTMSG(' GTCLB1:  Bank CLB1 does not exist')
        GO TO 999
      ENDIF
C
      NTOTBD = IC(LCLB1+1)
      IF (NTOTBD.LE.0) GO TO 999
      DO I = 1,NTOTBD
        IADR = IC(LCLB1+I+1)
        CALL CADUPK(ICRATE,IADR,ICRATE,IADC,IBLS,ITWR,IDEP,ISCL,NEG)
        IF (IADC.NE.ICARD) GO TO 100
        NBAD = NBAD + 1
        ICHAN = IBLS + ITWR + (IBLS+7)/8   ! (IBLS+7)/8 to take care of PD ch
        BAD(ICHAN) = IAND(IC(LCLB1+I+1),'FFFF'X)
  100   CONTINUE
      ENDDO
C
  999 RETURN
      END

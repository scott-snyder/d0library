      SUBROUTINE GTCLB8(ICRATE,ICARD,NBAD,BAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return bad flag word for all channels in 
C-                         ADC crate ICRATE, ADC card ICARD, BANK = CLB8
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
      INCLUDE 'D0$LINKS:IZCLZ8.LINK'
      INCLUDE 'D0$LINKS:IZCLB8.LINK'
      INTEGER ICRATE,ICARD,NBAD,BAD(0:7)
      INTEGER GZCGNH,GZCGLZ,GZCLZ8,GZCLB8,LCGLZ,LCLZ8,LCLB8
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
        CALL INTMSG(' GTCLB8:  Pedestal header bank does not exist')
        GO TO 999
      ENDIF
C      LCGLZ = GZCGLZ()
      LCGLZ = LC(LCGNH-IZCGLZ)
      IF (LCGLZ.LE.0) THEN
        CALL INTMSG(' GTCLB8:  Bank CGLZ does not exist')
        GO TO 999
      ENDIF
C      LCLZ8 = GZCLZ8()
      LCLZ8 = LC(LCGLZ-IZCLZ8)
      IF (LCLZ8.LE.0) THEN
        CALL INTMSG(' GTCLB8:  Bank CLZ8 does not exist')
        GO TO 999
      ENDIF
C      LCLB8 = GZCLB8()
      LCLB8 = LC(LCLZ8-IZCLB8)
      IF (LCLB8.LE.0) THEN
        CALL INTMSG(' GTCLB8:  Bank CLB8 does not exist')
        GO TO 999
      ENDIF
C
      NTOTBD = IC(LCLB8+1)
      IF (NTOTBD.LE.0) GO TO 999
      DO I = 1,NTOTBD
        IADR = IC(LCLB8+I+1)
        CALL CADUPK(ICRATE,IADR,ICRATE,IADC,IBLS,ITWR,IDEP,ISCL,NEG)
        IF (IADC.NE.ICARD) GO TO 100
        NBAD = NBAD + 1
        ICHAN = IBLS + ITWR + (IBLS+7)/8   ! (IBLS+7)/8 to take care of PD ch
        BAD(ICHAN) = IAND(IC(LCLB8+I+1),'FFFF'X)
  100   CONTINUE
      ENDDO
C
  999 RETURN
      END

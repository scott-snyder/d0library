      SUBROUTINE GTCLBC(ICRATE,ICARD,NBAD,BAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return bad flag word for all channels in 
C-                         ADC crate ICRATE, ADC card ICARD, BANK = CLBC
C-                         Laser corrected-gains bad channel bank
C-
C-   Inputs  : ICRATE  - ADC crate number
C-             ICARD   - ADC card number
C-   Outputs : NBAD    - number of bad channels found
C-   Controls: BAD(9)  - flags for all possible channels (0->good)
C-
C-   Created   2-NOV-1994   Jan Guida
C-   Updated  22-Jan-1996   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCGLZ.LINK'
      INCLUDE 'D0$LINKS:IZCLZC.LINK'
      INCLUDE 'D0$LINKS:IZCLBC.LINK'
      INTEGER ICRATE,ICARD,NBAD,BAD(0:7)
      INTEGER GZCGNH,GZCGLZ,GZCLZC,GZCLBC,LCGLZ,LCLZC,LCLBC
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
        CALL INTMSG(' GTCLBC:  Pedestal header bank does not exist')
        GO TO 999
      ENDIF
C      LCGLZ = GZCGLZ()
      LCGLZ = LC(LCGNH-IZCGLZ)
      IF (LCGLZ.LE.0) THEN
        CALL INTMSG(' GTCLBC:  Bank CGLZ does not exist')
        GO TO 999
      ENDIF
C      LCLZC = GZCLZC()
      LCLZC = LC(LCGLZ-IZCLZC)
      IF (LCLZC.LE.0) THEN
        CALL INTMSG(' GTCLBC:  Bank CLZC does not exist')
        GO TO 999
      ENDIF
C      LCLBC = GZCLBC()
      LCLBC = LC(LCLZC-IZCLBC)
      IF (LCLBC.LE.0) THEN
        CALL INTMSG(' GTCLBC:  Bank CLBC does not exist')
        GO TO 999
      ENDIF
C
      NTOTBD = IC(LCLBC+1)
      IF (NTOTBD.LE.0) GO TO 999
      DO I = 1,NTOTBD
        IADR = IC(LCLBC+I+1)
        CALL CADUPK(ICRATE,IADR,ICRATE,IADC,IBLS,ITWR,IDEP,ISCL,NEG)
        IF (IADC.NE.ICARD) GO TO 100
        NBAD = NBAD + 1
        ICHAN = IBLS + ITWR + (IBLS+7)/8   ! (IBLS+7)/8 to take care of PD ch
        BAD(ICHAN) = IAND(IC(LCLBC+I+1), 65535) ! 0xffff
  100   CONTINUE
      ENDDO
C
  999 RETURN
      END

      SUBROUTINE GTCG0B(ICRATE,ICARD,NBAD,BAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return bad flag word for all channels in 
C-                         ADC crate ICRATE, ADC card ICARD, BANK = CG0B
C-
C-   Inputs  : ICRATE  - ADC crate number
C-             ICARD   - ADC card number
C-   Outputs : NBAD    - number of bad channels found
C-   Controls: BAD(64) - flags for all possible channels (0->good)
C-
C-   Created  13-JUL-1994   Jan Guida
C-   Updated  21-Jan-1996   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCGL0.LINK'
      INCLUDE 'D0$LINKS:IZCG0B.LINK'
      INTEGER ICRATE,ICARD,NBAD,BAD(0:63)
      INTEGER GZCGNH,GZCGL0,GZCG0B,LCGL0,LCG0B
      INTEGER NTOTBD,IADR,IADC,IBLS,ITWR,IDEP,ISCL,NEG
      INTEGER ICHAN,I
      INTEGER LZFIND
C----------------------------------------------------------------------
      NBAD = 0
      CALL VZERO(BAD,64)      ! ?????????????????
C
      LCGNH = GZCGNH()
      LCGNH = LZFIND(IDVSTP,LCGNH,ICRATE,9)   !Finds Bank with Crate
      IF (LCGNH.LE.0) THEN
        CALL INTMSG(' GTCG0B:  Pedestal header bank does not exist')
        GO TO 999
      ENDIF
C      LCGL0 = GZCGL0()
      LCGL0 = LC(LCGNH-IZCGL0)
      IF (LCGL0.LE.0) THEN
        CALL INTMSG(' GTCG0B:  Bank CGL0 does not exist')
        GO TO 999
      ENDIF
C      LCG0B = GZCG0B()
      LCG0B = LC(LCGL0-IZCG0B)
      IF (LCG0B.LE.0) THEN
        CALL INTMSG(' GTCG0B:  Bank CG0B does not exist')
        GO TO 999
      ENDIF
C
      NTOTBD = IC(LCG0B+1)
      IF (NTOTBD.LE.0) GO TO 999
      DO I = 1,NTOTBD
        IADR = IC(LCG0B+I+1)
        CALL CADUPK(ICRATE,IADR,ICRATE,IADC,IBLS,ITWR,IDEP,ISCL,NEG)
        IF (IADC.NE.ICARD) GO TO 100
        NBAD = NBAD + 1
        ICHAN = IBLS*8 + ITWR*2    ! ????????????????
        BAD(ICHAN) = IAND(IC(LCG0B+I+1), 65535)  !  0xffff
  100   CONTINUE
      ENDDO
C
  999 RETURN
      END

      SUBROUTINE GTCG0M(CRATE,ADR,VAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns values of higher moments (skewness and
C-                            kurtosis) from calib - level-0 gains
C-
C-   Inputs  : CRATE - ADC crate number
C-             ADR   - hardware address
C-   Outputs : VAL(2) - Value of skewness/kurtosis
C-   Controls: none
C-
C-   Created  15-MAR-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C      INTEGER NEG,DEP,TWR,BLS,KADR,ADC,IGN,CHAN
      INTEGER CRATE,ADR
C      INTEGER GZCG0M,LCG0M
C
C      INCLUDE 'D0$PARAMS:PRTPDG.DEF'
C      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      REAL VAL(*)
      character*80 msg
C
C----------------------------------------------------------------------
      logical lfirst
      data lfirst/.true./

      if (lfirst) then
        write (msg,10) crate,adr
   10   format(' DUMMY VERSION OF GTCG0M CALLED for crate ',I3,
     &    ' adr ',I8)
        call intmsg(msg)
        lfirst = .false.
      endif
C
C      LCG0M = GZCG0M()
C      IF ( LCG0M.GT.0 ) THEN
C        KADR = ISHFT(ADR,16)
C        IGN = 0
C        CALL CADUPK(CRATE,KADR,CRATE,ADC,BLS,TWR,DEP,IGN,NEG)
C        IF (IGN.EQ.0) THEN              ! x8
C
C          CHAN = ????
C
C          VAL(1) = C(LCG0M+NHEAD+2*CHAN+1)
C          VAL(2) = C(LCG0M+NHEAD+2*CHAN+2)
C        ELSE                               !Moment bank does not exist for x1
C          VAL(1)=-1
C          VAL(2)=-1    !No bank
C        ENDIF
C      ELSE                                        !Moment bank does not exist
        VAL(1)=-1
        VAL(2)=-1    !No bank
C      ENDIF
C
  999 RETURN
      END

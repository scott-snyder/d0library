      SUBROUTINE GTCATD (LCATD,IPNTEM,IPNTHD,IPNTMU,NEMTWR,NHDTWR,
     &                         NMUTWR,ETMNEM,ETMNHD,EMNMUO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns CATD's address, pointers, Et minimum
C-                         ( E minimum for muon's towers ) and Number of 
C-                         towers of EM, HAD and MUON associated towers.
C-
C-   Inputs   : None
C-   Outputs  : LCATD  [I] - the address of CATD bank
C-              IPNTEM [I] - pointer to EM sector
C-              IPNTHD [I] - pointer to HAD sector
C-              IPNTMU [I] - pointer to MUON sector
C-              NEMTWR [I] - No. of EM towers
C-              NHDTWR [I] - No. of HAD towers
C-              NMUTWR [I] - No. of towers asso. with muons
C-              ETMNEM [F] - min. Et of the saved EM towers
C-              ETMNHD [F] - min. Et of the saved HAD towers
C-              EMNMUO [F] - min. E(not Et!) of the towers asso. /w muons
C-
C-   Controls: none
C-
C-   Created  19-DEC-1991 Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCATD.LINK'
C
      INTEGER LCATD1, GZCATD
      INTEGER LCATD,IPNTEM,IPNTHD,IPNTMU,NEMTWR,NHDTWR,NMUTWR
      REAL    ETMNEM,ETMNHD,EMNMUO, EUNIT
C
      DATA    EUNIT / .1/
C----------------------------------------------------------------------
      LCATD1 = GZCATD()
      IF( LCATD1 .GT. 0 ) THEN
        IPNTEM = IQ(LCATD1+2)
        IPNTHD = IQ(LCATD1+3)
        IPNTMU = IQ(LCATD1+4)
        ETMNEM = IQ(LCATD1+5)*EUNIT
        ETMNHD = IQ(LCATD1+6)*EUNIT
        EMNMUO = IQ(LCATD1+7)*EUNIT
        NEMTWR = IQ(LCATD1+IPNTEM)
        NHDTWR = IQ(LCATD1+IPNTHD)
        NMUTWR = IQ(LCATD1+IPNTMU)
        LCATD  = LCATD1
      ELSE
        CALL ERRMSG('CATD','GTCATD',
     &       ' NO CATD BANKS ARE AVAILABLE !!!','W')
      ENDIF
C-
  999 RETURN
      END

      SUBROUTINE MASSFL(LMASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank MASS.
C-
C-   Inputs  :  LMASS = link of bank to be filled.
C-              LMASS < 0, routine will get link using GZMASS
C-              LMASS = 0, routine will book bank.
C-
C-   Outputs :
C-   Controls:
C-
C-   Created  22-JUN-1993 10:51:16.15  Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LMASS
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:TOP_SOLNSE.INC'
      INCLUDE 'D0$INC:KINEQ.INC'
      INCLUDE 'D0$INC:EVENT_QUAN1.INC'
      INCLUDE 'D0$INC:BTAG_ISAJ.INC'
C----------------------------------------------------------------------
      INTEGER GZMASS
      INTEGER LPROC,NDATA
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
C do initialization here if necessary.
        FIRST = .FALSE.
      ENDIF
C
      IF ( LMASS .LT. 0 ) THEN
        LMASS = GZMASS()    ! GET LINK.
      ENDIF
C
      IF ( LMASS .EQ. 0 ) THEN
        NDATA = 43
        CALL BKMASS(LPROC,LMASS,NDATA)
      ENDIF
C
C Book the bank if argument = 0.
C
      IQ(LMASS+1) = 1               ! Bank version
C
C fill in the rest of the bank here.
C
      Q(LMASS+2) = TMASS_LO
      Q(LMASS+3) = TMASS_HI
C
      Q(LMASS+4) = RR_MASS(1)
      Q(LMASS+5) = RR_LIKELY(1)
      Q(LMASS+6) = RR_LO(1)
      Q(LMASS+7) = RR_HI(1)
C
      Q(LMASS+8) = DG_MASS(1)
      Q(LMASS+9) = DG_LIKELY(1)
      Q(LMASS+10) = DG_LO(1)
      Q(LMASS+11) = DG_HI(1)
      Q(LMASS+12) = TMASS_ON(1)
C
      Q(LMASS+13) = RR_MASS(2)
      Q(LMASS+14) = RR_LIKELY(2)
      Q(LMASS+15) = RR_LO(2)
      Q(LMASS+16) = RR_HI(2)
C
      Q(LMASS+17) = DG_MASS(2)
      Q(LMASS+18) = DG_LIKELY(2)
      Q(LMASS+19) = DG_LO(2)
      Q(LMASS+20) = DG_HI(2)
      Q(LMASS+21) = TMASS_ON(1)
C
      CALL UCOPYDS(JET1_S,Q(LMASS+22),4)
      CALL UCOPYDS(JET2_S,Q(LMASS+26),4)
      CALL UCOPYDS(JET3_S,Q(LMASS+30),4)
C
      Q(LMASS+34) = COMBNUM
C
C ****  btag info here
C
      Q(LMASS+35) = BTAG_FL      ! Btag flag to be worked out
      Q(LMASS+36) = NTAG
C
      CALL UCTOH(NAME_TAG(1),Q(LMASS+37),4,2)
      Q(LMASS+38) = DIF_R(1)
      Q(LMASS+39) = DIF_ET(1)
C
      CALL UCTOH(NAME_TAG(2),Q(LMASS+40),4,2)
      Q(LMASS+41) = DIF_R(2)
      Q(LMASS+42) = DIF_ET(2)
C
      Q(LMASS+43) = NCNFGE
C
  999 RETURN
      END

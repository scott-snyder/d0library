      SUBROUTINE UDST_FIX_JETS(LJETS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : reconstruct full set of kinematic quantities
C-                         for JETS or JNEP banks from pt, eta, phi.
C-
C-   Inputs  : LJETS - pointer to bank
C-
C-   Created   5-JAN-1994   Ulrich Heintz
C-   Updated  17-NOV-1995   Ulrich Heintz  set energy correction status word 
C-   Updated  28-NOV-1995   Ulrich Heintz  compute px,py,pz only for version<5 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL    PT,ETA,PHI
      INTEGER LJETS,IER,UDST_VERSION,LUDST,GZUDST
      IF(LJETS.EQ.0)THEN
        CALL ERRMSG('LINK=0','UDST_FIX_JETS','called with LJETS=0','W')
        GOTO 999
      ENDIF
      LUDST=GZUDST()
      UDST_VERSION = IQ(LUDST+1)
      PT  = Q(LJETS+2)
      ETA = Q(LJETS+9)
      PHI = Q(LJETS+8)
C... for UDST version<5 recompute px,py,pz, for version>=5 px,py,pz are stored
      IF(UDST_VERSION.LE.4)THEN     
        Q(LJETS+2) = PT*COS(PHI)
        Q(LJETS+3) = PT*SIN(PHI)
        Q(LJETS+4) = PT*SINH(ETA)
      ENDIF
      IF(Q(LJETS+7).EQ.0) Q(LJETS+7) = 2.*ATAN(EXP(-ETA))
C... do the following only for JETS banks, not JNEP
      IF(IQ(LJETS-4).EQ.4HJETS)THEN
C... if version >=8 set bit in the energy correction status word which
C    says the jet area and ET weighted area has been calculated in RECO.
        IF(IQ(LJETS+1).GE.8) THEN
          CALL SET_JETS_BANK_CORRECTED(LJETS, 'ARA', IER )
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END

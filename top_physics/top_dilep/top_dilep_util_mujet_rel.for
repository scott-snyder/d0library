      SUBROUTINE TOP_DILEP_UTIL_MUJET_REL(LPMUO,LJETS,PT_REL1,PT_REL2,
     &  P_FRAC,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : calculate muon-jet correlations
C-
C-   Inputs  : LPMUO,LJETS = pointers for the muon,jet
C-   Outputs : PT_REL1,PT_REL2,P_FRAC = muon-jet correlations
C-             IERR = 0 if OK
C-   Controls: None
C-
C-   Created  24-MAY-1993 John M. Butler
C-   Modified 29-Nov-1994 RE Hall: changed name from TOP_LEPTONS to
C-                        TOP_DILEP and removed jet correction and rcp calls
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LPMUO,LJETS,IERR
C
      REAL PT_REL1,PT_REL2,P_FRAC
      REAL MU_P,MU_PX,MU_PY,MU_PZ
      REAL JET_E,JET_THETA,JET_PHI,JET_EX,JET_EY,JET_EZ
      REAL COS_ANG1,SIN_ANG1,COS_ANG2,SIN_ANG2,MU_PLUS_JET
C
C----------------------------------------------------------------------
      IERR = 0

C
      MU_P  = Q(LPMUO+13)
      MU_PX = Q(LPMUO+10)
      MU_PY = Q(LPMUO+11)
      MU_PZ = Q(LPMUO+12)
C
      JET_E     = Q(LJETS+5)
      JET_THETA = Q(LJETS+7)
      JET_PHI   = Q(LJETS+8)
      JET_EX = JET_E*COS(JET_PHI)*SIN(JET_THETA)
      JET_EY = JET_E*SIN(JET_PHI)*SIN(JET_THETA)
      JET_EZ = JET_E*COS(JET_THETA)
C
C Calculate p_T relative to jet not including the muon energy in the jet
C
      COS_ANG1 = (MU_PX*JET_EX + MU_PY*JET_EY + MU_PZ*JET_EZ)/
     &           (MU_P*JET_E)
      IF(COS_ANG1 .LT. 1.) THEN
        SIN_ANG1 = SQRT(1. - COS_ANG1**2)
      ELSE
        SIN_ANG1 = 0.
        IERR = IERR+1
      ENDIF
      PT_REL1 = MU_P*SIN_ANG1
C
C Calculate p_T relative to jet including the muon energy in the jet
C
      MU_PLUS_JET = SQRT((MU_PX+JET_EX)**2 + (MU_PY+JET_EY)**2 +
     &                   (MU_PZ+JET_EZ)**2)
      COS_ANG2 = (JET_E*COS_ANG1 + MU_P)/MU_PLUS_JET
      IF(COS_ANG2 .LT. 1.) THEN
        SIN_ANG2 = SQRT(1. - COS_ANG2**2)
      ELSE
        SIN_ANG2 = 0.
        IERR = IERR+1
      ENDIF
      PT_REL2 = MU_P*SIN_ANG2
C
C Calculate the fraction of the jet energy carried by the muon
C
      P_FRAC = MU_P/MU_PLUS_JET
  999 RETURN
      END

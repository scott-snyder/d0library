      SUBROUTINE TOP_LEPTONS_UTIL_MUJET_REL(LPMUO,LJETS,PT_REL1,PT_REL2,
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
C-   Created  24-MAY-1993   John M. Butler
C-             9-JUL-1993   Joey Thompson  Name change to add to Top_Leptons
C-                                         Add jet energy corrections
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LPMUO,LJETS,IERR,IER
C
      REAL PT_REL1,PT_REL2,P_FRAC
      REAL MU_P,MU_PX,MU_PY,MU_PZ
      REAL JET_E,JET_THETA,JET_PHI,JET_EX,JET_EY,JET_EZ
      REAL JET_ETA,JET_ET
      REAL COS_ANG1,SIN_ANG1,COS_ANG2,SIN_ANG2,MU_PLUS_JET
C
      LOGICAL CORR_JETS
C
      LOGICAL FIRST
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
      IERR = 0
      IF(FIRST) THEN
        CALL EZPICK('TOP_LEPTONS_RCP')
        CALL EZGET('JETS_CORR',CORR_JETS,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_MU_JET_REL',' ','F')
        FIRST=.FALSE.
      ENDIF

C
      MU_P  = Q(LPMUO+13)
      MU_PX = Q(LPMUO+10)
      MU_PY = Q(LPMUO+11)
      MU_PZ = Q(LPMUO+12)
C
      IF (CORR_JETS) THEN
        JET_THETA = Q(LJETS+7)
        JET_PHI   = Q(LJETS+8)
        CALL TOP_LEPTONS_CORR_JETPARM(LJETS,JET_E,JET_ET,JET_EX,JET_EY,
     &    JET_EZ,JET_PHI,JET_ETA,IER)
        IF(IER.LT.0) THEN
          JET_E     = Q(LJETS+5)
          JET_THETA = Q(LJETS+7)
          JET_PHI   = Q(LJETS+8)
          JET_EX = JET_E*COS(JET_PHI)*SIN(JET_THETA)
          JET_EY = JET_E*SIN(JET_PHI)*SIN(JET_THETA)
          JET_EZ = JET_E*COS(JET_THETA)
        ENDIF
        IERR = IERR + IER
      ELSE
        JET_E     = Q(LJETS+5)
        JET_THETA = Q(LJETS+7)
        JET_PHI   = Q(LJETS+8)
        JET_EX = JET_E*COS(JET_PHI)*SIN(JET_THETA)
        JET_EY = JET_E*SIN(JET_PHI)*SIN(JET_THETA)
        JET_EZ = JET_E*COS(JET_THETA)
      ENDIF
C
C Calculate p_T relative to jet not including the muon energy in the jet
C
      COS_ANG1 = (MU_PX*JET_EX + MU_PY*JET_EY + MU_PZ*JET_EZ)/
     &           (MU_P*JET_E)
      IF(COS_ANG1 .LT. 1.) THEN
        SIN_ANG1 = SQRT(1. - COS_ANG1**2)
      ELSE
        SIN_ANG1 = 0.
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
      ENDIF
      PT_REL2 = MU_P*SIN_ANG2
C
C Calculate the fraction of the jet energy carried by the muon
C
      P_FRAC = MU_P/MU_PLUS_JET
  999 RETURN
      END

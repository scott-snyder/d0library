      SUBROUTINE TOP_LEPTONS_FIND_ISOLPH(I_PH_IN,LPPHO_IN,
     1 I_PH_OUT,LPPHO_OUT,I_JT_IN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Loop through muon candidates and return a
C-                         reduced list of isolated candidates
C-
C-   Inputs  : 
C-                I_PH_IN   - no. of good photon candidates
C-                LPPHO_IN  - input array of bank pointers
C-                I_JT_IN   - no. of good jets
C-   Outputs : 
C-                I_PH_OUT  - no. of good isolated photon candidates
C-                LPPHO_OUT - outout array of bank pointers
C-   Controls: 
C-                None
C-
C-   Created   3-MAY-1993   Stephen J. Wimpenny
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      EXTERNAL TOP_LEPTONS_EM_CORRECTION
C
      LOGICAL FIRST,CORR_EM,CORR_JETS
C
      INTEGER I_PH_IN,LPPHO_IN(5),I_PH_OUT,LPPHO_OUT(5)
      INTEGER I_JT_IN,LJETS_DRMIN,LJETS_DPHI
      INTEGER I,IER
C
      REAL PHOT_PTMIN,DR_MIN_PHJET,DR_MIN,DPHI_MIN
      REAL PHOT_PT,TOP_LEPTONS_EM_CORRECTION
      REAL ETMIN_CUT,ETA_DRMIN,DUM
      REAL E_DRMIN,ET_DRMIN,PX_DRMIN,PY_DRMIN,PZ_DRMIN,PHI_DRMIN
C
      DATA FIRST/.TRUE./
C
C *** Read cut parameters from RCP file
C
      IER=0
      IF(FIRST) THEN
        CALL EZPICK('TOP_LEPTONS_RCP')
C
        CALL EZGET('ISOLPH_PTMIN',PHOT_PTMIN,IER)
        IF(IER.EQ.0) CALL EZGET('PH_JET_DRMIN',DR_MIN_PHJET,IER)
        IF(IER.EQ.0) CALL EZGET('EM_CORR',CORR_EM,IER)
        IF (IER.EQ.0) CALL EZGET('ETMIN_JET_DRMIN',ETMIN_CUT,IER)
        IF (IER.EQ.0) CALL EZGET('JETS_CORR',CORR_JETS,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_FIND_ISOLPH',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      I_PH_OUT=0
      IF(I_PH_IN.LT.1) GO TO 999
      DO I=1,I_PH_IN
C
C *** Pt min cut for muon from t-decay
C
        PHOT_PT=Q(LPPHO_IN(I)+7)
        IF(CORR_EM) THEN
          DUM=TOP_LEPTONS_EM_CORRECTION(LPPHO_IN(I))
          PHOT_PT=PHOT_PT*DUM
        ENDIF
        IF(Q(LPPHO_IN(I)+7).LT.PHOT_PTMIN) GO TO 100
C
C *** dR(e-jet) cut
C
        IF(I_JT_IN.LT.1) THEN
          I_PH_OUT=I_PH_OUT+1
          LPPHO_OUT(I_PH_OUT)=LPPHO_IN(I)
        ELSE
          CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPPHO_IN(I)+9),
     1      Q(LPPHO_IN(I)+10),DR_MIN,LJETS_DRMIN,DPHI_MIN,LJETS_DPHI)
C
C *** Get corrected energy for nearest jet to test against min energy
C *** threshold
C
          IF(CORR_JETS) THEN
            CALL TOP_LEPTONS_CORR_JETPARM(LJETS_DRMIN,E_DRMIN,
     1        ET_DRMIN,PX_DRMIN,PY_DRMIN,PZ_DRMIN,PHI_DRMIN,
     2        ETA_DRMIN,IER)
            IF(IER.LT.0) THEN
              ET_DRMIN=Q(LJETS_DRMIN+6)
            ENDIF
          ELSE
            ET_DRMIN=Q(LJETS_DRMIN+6)
          ENDIF
          IF(DR_MIN.LT.DR_MIN_PHJET.AND.ET_DRMIN.GT.ETMIN_CUT) THEN
            GO TO 100
          ELSE
            I_PH_OUT=I_PH_OUT+1
            LPPHO_OUT(I_PH_OUT)=LPPHO_IN(I)
          ENDIF
        ENDIF
  100   CONTINUE
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
      END

      LOGICAL FUNCTION LJTOP_CUT_DIAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       dialog for TOP CUT analysis
C-
C-   Created   4-MAY-1992   Serban Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    ETCUTB(2),ETCUTW,ELEC_CUT,MUON_CUT,MISET_CUT
      REAL    JET_ETA_CUT,CORR_MISET,DIST_EL_CUT
      LOGICAL CORRJ,DO_DUMPS,ALGO(3)
      INTEGER ICHOICE,IALGO
C----------------------------------------------------------------------
C
      LJTOP_CUT_DIAL=.TRUE.
      ETCUTB(1)=10.
      CALL GETPAR(1,'b-jet 1 ET cut [10.]>','R',ETCUTB(1))
      ETCUTB(2)=10.
      CALL GETPAR(1,'b-jet 1 ET cut [10.]>','R',ETCUTB(2))
      ETCUTW=10.
      CALL GETPAR(1,'W-jets ET cut [10.]>','R',ETCUTW)
      ELEC_CUT=10.
      CALL GETPAR(1,'electron ET cut [10.]>','R',ELEC_CUT)
      MUON_CUT=10.
      CALL GETPAR(1,'muon ET cut [10.]>','R',MUON_CUT)
      MISET_CUT=10.
      CALL GETPAR(1,'missing ET cut [10.]>','R',MISET_CUT)
      JET_ETA_CUT=2.0
      CALL GETPAR(1,'Eta cut on jets [10.]>','R',JET_ETA_CUT)
      IALGO=5
      CALL GETPAR1('ALGORITHM 1,2,3 (1,2=4,all>4) [5]:','I',IALGO)
      IF(IALGO.EQ.1.OR.IALGO.GT.3) ALGO(1)=.TRUE.
      IF(IALGO.EQ.2.OR.IALGO.GT.3) ALGO(2)=.TRUE.
      IF(IALGO.EQ.3.OR.IALGO.GT.4) ALGO(3)=.TRUE.
      ICHOICE=2
      CALL GETPAR1('jet algorithm [2]:','I',ICHOICE)
      CORRJ=.TRUE.
      CALL GETPAR1l('Correct jets [Y]:','L',CORRJ)
      CORR_MISET=1.0
      CALL GETPAR(1,'Correct missing Et [1.0]:','R',CORR_MISET)
      DIST_EL_CUT=20.
      CALL GETPAR(1,'electron distance cut [20.]:','R',DIST_EL_CUT)
      DO_DUMPS=.FALSE.
      CALL GETPAR1l('DO_DUMPS [F]:','L',DO_DUMPS)
C
      CALL LJTOP_SET_CUTS(ETCUTB,ETCUTW,ELEC_CUT,MUON_CUT,MISET_CUT,
     &  JET_ETA_CUT,DIST_EL_CUT)
      CALL LJTOP_SET_CUT_OPT(ALGO,CORR_MISET,CORRJ,DO_DUMPS,ICHOICE)
C
  999 RETURN
      END

      SUBROUTINE TOP_LEPTONS_FIND_ISOLMU(I_MU_IN,LPMUO_IN,
     1 I_MU_OUT,LPMUO_OUT,I_JT_IN,MODE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Loop through muon candidates and return a
C-                         reduced list of isolated candidates
C-
C-   Inputs  : 
C-                I_MU_IN   - no. of good muon candidates
C-                LPMUO_IN  - input array of bank pointers
C-                I_JT_IN   - no. of good jets
C-                MODE      - 1 / -1 select isolated / non-isolated muons
C-   Outputs : 
C-                I_MU_OUT  - no. of good muon candidates
C-                LPMUO_OUT - outout array of bank pointers
C-   Controls: 
C-                None
C-
C-   Created   3-MAY-1993   Stephen J. Wimpenny
C-   Modified  7-Jun-1993   Fix bug in RCP paramater definition 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL FIRST,DO_MUISO_CONES,CORR_JETS
C
      INTEGER I_MU_IN,LPMUO_IN(5),I_MU_OUT,LPMUO_OUT(5)
      INTEGER I_JT_IN,LJETS_DRMIN,LJETS_DPHI
      INTEGER I,IER,MODE,IOK
C
      REAL MUON_PTMIN,MUON_PTMIN_NISOL,DR_MIN_MUJET,DR_MAX_MUJET,DR_MIN
      REAL DPHI_MIN,DR_MU_CORE,DR_MU_ISOL,ETMIN_CUT,ETA_DRMIN
      REAL E_DRMIN,ET_DRMIN,PX_DRMIN,PY_DRMIN,PZ_DRMIN,PHI_DRMIN
      REAL MAX_EDIF_CONES,MAX_EDIF_CONES_CF,MAX_EDIF_CONES_EF
      REAL ERADG,ERAD,ECOR,EISO
C
      DATA FIRST/.TRUE./
C
C *** Read cut parameters from RCP file
C
      IER=0
      IF(FIRST) THEN
        CALL EZPICK('TOP_LEPTONS_RCP')
C
        CALL EZGET('ISOLMU_PTMIN',MUON_PTMIN,IER)
        IF (IER.EQ.0) CALL EZGET('NISOLMU_PTMIN',MUON_PTMIN_NISOL,IER)
        IF (IER.EQ.0) CALL EZGET('MU_JET_DRMIN',DR_MIN_MUJET,IER)
        IF (IER.EQ.0) CALL EZGET('MU_JET_DRMAX',DR_MAX_MUJET,IER)
        IF (IER.EQ.0) CALL EZGET_l('DO_ISOL_CONE_SUB',DO_MUISO_CONES,
     &       IER)
        IF (IER.EQ.0) CALL EZGET('CORECONE_SIZE',DR_MU_CORE,IER)
        IF (IER.EQ.0) CALL EZGET('ISOCONE_SIZE',DR_MU_ISOL,IER)
        IF (IER.EQ.0) CALL EZGET('ISOCONE_CUT_CF',MAX_EDIF_CONES_CF,IER)
        IF (IER.EQ.0) CALL EZGET('ISOCONE_CUT_EF',MAX_EDIF_CONES_EF,IER)
        IF (IER.EQ.0) CALL EZGET('ETMIN_JET_DRMIN',ETMIN_CUT,IER)
        IF (IER.EQ.0) CALL EZGET_l('JETS_CORR',CORR_JETS,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_FIND_ISOLMU',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      I_MU_OUT=0
      IF(I_MU_IN.LT.1) GO TO 999
      DO I=1,I_MU_IN
C
C *** Define cuts for energy isolation
C
        IF(IQ(LPMUO_IN(I)+7).GT.4) THEN
          MAX_EDIF_CONES=MAX_EDIF_CONES_EF
        ELSE
          MAX_EDIF_CONES=MAX_EDIF_CONES_CF
        ENDIF
        IF(MODE.GT.0) THEN
C
C *** Mode = 1 ......Isolated Muon Selection
C
          IF(I_JT_IN.LT.1) THEN
C
C *** isolation-core cone algorithm
C
            IOK=1
            IF(DO_MUISO_CONES) THEN
              CALL TOP_LEPTONS_UTIL_MUISO_CORE(LPMUO_IN(I),
     1          DR_MU_CORE,DR_MU_ISOL,MAX_EDIF_CONES,ERADG,
     2          ERAD,ECOR,EISO,IOK)
              IF(IOK.LT.0) GO TO 10
            ENDIF
C
C *** Pt min cut for muon
C
            IF(Q(LPMUO_IN(I)+14).LT.MUON_PTMIN) GO TO 10
            I_MU_OUT=I_MU_OUT+1
            LPMUO_OUT(I_MU_OUT)=LPMUO_IN(I)
   10       CONTINUE
          ELSE
C
C *** isolation-core cone algorithm
C
            IOK=1
            IF(DO_MUISO_CONES) THEN
              CALL TOP_LEPTONS_UTIL_MUISO_CORE(LPMUO_IN(I),
     1          DR_MU_CORE,DR_MU_ISOL,MAX_EDIF_CONES,ERADG,
     2          ERAD,ECOR,EISO,IOK)
              IF(IOK.LT.0) GO TO 20
            ENDIF
C
C *** dRmin(mu-jet) algorithm
C
            CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPMUO_IN(I)+16),
     1        Q(LPMUO_IN(I)+17),DR_MIN,LJETS_DRMIN,DPHI_MIN,LJETS_DPHI)
C
C *** Get corrected energy for nearest jet to test against min energy
C *** threshold
C
            IF(LJETS_DRMIN.LE.0) THEN
              WRITE(12,1000) LJETS_DRMIN
              GO TO 20
            ENDIF
            IF(CORR_JETS) THEN
              CALL TOP_LEPTONS_CORR_JETPARM(LJETS_DRMIN,E_DRMIN,
     1          ET_DRMIN,PX_DRMIN,PY_DRMIN,PZ_DRMIN,PHI_DRMIN,
     2          ETA_DRMIN,IER)
              IF(IER.LT.0) THEN
                ET_DRMIN=Q(LJETS_DRMIN+6)
              ENDIF
            ELSE
              ET_DRMIN=Q(LJETS_DRMIN+6)
            ENDIF
            IF(DR_MIN.LT.DR_MIN_MUJET.AND.ET_DRMIN.GT.ETMIN_CUT) THEN
              GO TO 20
            ELSE
C
C *** Pt min cut for muon 
C
              IF(Q(LPMUO_IN(I)+14).LT.MUON_PTMIN) GO TO 20
              I_MU_OUT=I_MU_OUT+1
              LPMUO_OUT(I_MU_OUT)=LPMUO_IN(I)
            ENDIF
   20       CONTINUE
          ENDIF
        ELSE
C
C *** Mode = -1 ..... Non-Isolated Muon Selection
C
          IF(I_JT_IN.LT.1) THEN
C
C *** isolation-core cone algorithm
C
            IOK=1
            IF(DO_MUISO_CONES) THEN
              CALL TOP_LEPTONS_UTIL_MUISO_CORE(LPMUO_IN(I),
     1          DR_MU_CORE,DR_MU_ISOL,MAX_EDIF_CONES,ERADG,ERAD,
     2          ECOR,EISO,IOK)
            ENDIF
            IF(IOK.GT.0) THEN
              GO TO 25
            ELSE
C
C *** Pt min cut for muon
C
              IF(Q(LPMUO_IN(I)+14).GT.MUON_PTMIN) THEN
                I_MU_OUT=I_MU_OUT+1
                LPMUO_OUT(I_MU_OUT)=LPMUO_IN(I)
              ENDIF
            ENDIF
   25       CONTINUE
          ELSE
C
C *** isolation-core cone algorithm
C
            IOK=1
            IF(DO_MUISO_CONES) THEN
              CALL TOP_LEPTONS_UTIL_MUISO_CORE(LPMUO_IN(I),
     1          DR_MU_CORE,DR_MU_ISOL,MAX_EDIF_CONES,ERADG,ERAD,
     2          ECOR,EISO,IOK)
            ENDIF
            IF(IOK.GT.0) THEN
              GO TO 36
            ELSE
              GO TO 35
            ENDIF
C
C *** dRmax(mu-jet) algorithm
C
   36       CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPMUO_IN(I)+16),
     1        Q(LPMUO_IN(I)+17),DR_MIN,LJETS_DRMIN,DPHI_MIN,LJETS_DPHI)
            IF(DR_MIN.GT.DR_MAX_MUJET) THEN
              GO TO 30
            ENDIF
C
C *** Pt min cut for muon
C
   35       IF(Q(LPMUO_IN(I)+14).GT.MUON_PTMIN_NISOL) THEN
              I_MU_OUT=I_MU_OUT+1
              LPMUO_OUT(I_MU_OUT)=LPMUO_IN(I)
            ENDIF
   30       CONTINUE
          ENDIF
        ENDIF
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
 1000 FORMAT(' ====> TOP_LEPTONS_FIND_ISOLMU <===== ',/,
     1 5X,' JETS Bank Problem , Pointer = ',I10,/)
      END

      REAL FUNCTION TOP_LEPTONS_JET_CORRECTION(LJETS,JET_CONE,ICOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get corrected jet energies
C-
C-   Inputs  : 
C-              LJETS    - pointer to current jet bank
C-              JET_CONE - cone size for current jet
C-              ICOR     - correction flag :
C-                             -1 => doing missing Et calculation
C-                              1 => doing standard jet calculation
C-
C-   Outputs : corrected jet energy
C-
C-   Created   3-JUL-1993   Meenakshi Narain
C-   Modified  9-Jul-1993   Uses new version of Andy Milder's Jet 
C-                          correction code (as of 7/9/93)
C-   Modified  1-Aug-1993   Uses different corrections in QCD_JET_CORRECTION
C-                          for jets and missing Et
C-   Modified 14-Oct-1993   Uses Physics$Util versions of JET_CONE_CORR
C-                          and QCD_JET_CORRECTIONS (note chenges in calling
C-                          arguments).
C-   Modified 20_Nov-1993   Uses Physics$Util version of Missing Et logic +
C-                          modified calling arguments in Jet Correction 
C-                          routines.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVERT.LINK/LIST'
C
      LOGICAL DO_ZSP_CORR,DO_UNDEVT_CORR,DO_OUTOFCONE_CORR
      LOGICAL MATCH,FIRST
C
      INTEGER LJETS,LVERT,LVERH,GZVERH,ISYS,IER,ICOR,N
C
      REAL NEW_JET_E,NEW_JET_ET,NEW_JET_ETA,EM_COR(3)
      REAL JET_CONE
      REAL EOUT, ETOUT, ETAOUT, PHIOUT, OUTVEC(3), ZVERT
C
      DATA FIRST/.TRUE./
C
C *** Read Correction controls from TOP_LEPTONS RCP
C ***  (These are used by S/R QCD_JET_CORRECTION)
C
      IF(FIRST) THEN
        CALL EZPICK('TOP_LEPTONS_RCP')
C
        CALL EZGET_i('JETS_ISYS',ISYS,IER)
        IF(IER.EQ.0) CALL EZGET_l('DO_ZSP_CORR',DO_ZSP_CORR,IER)
        IF(IER.EQ.0) CALL EZGET('DO_UNDEVT_CORR',DO_UNDEVT_CORR,IER)
        IF(IER.EQ.0) 
     1    CALL EZGET('DO_OUTOFCONE_CORR',DO_OUTOFCONE_CORR,IER)
        IF(IER.EQ.0) CALL EZGETA_i('EM_SCALE_FACTORS',0,0,0,N,IER)
        IF(IER.EQ.0) CALL EZGETA('EM_SCALE_FACTORS',1,N,1,EM_COR,IER)
C
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     1    'TOP_LEPTONS_JET_CORRECTION',' ','F')
        CALL EZRSET
        FIRST=.FALSE.
      ENDIF
C
      IF (JET_CONE.LT.0.49.OR.JET_CONE.GT.0.71) THEN
        TOP_LEPTONS_JET_CORRECTION = Q(LJETS+5)
      ENDIF
C
C *** Check on useage flag (ICOR)
C
      IF (ICOR.LT.-1.OR.ICOR.GT.1) THEN
        WRITE(12,1000) ICOR
        CALL ERRMSG(' Illegal Jet Correction Request',
     1 'TOP_LEPTONS_JET_CORRECTION',' ','F')
      ENDIF
      IF (ICOR.EQ.-1.AND.JET_CONE.LT.0.69) WRITE(12,1100) ICOR,JET_CONE
C
C *** Set Appropriate Correction flags for QCD_JET_CORRECTION
C 
      IF(ICOR.LT.0) THEN
C
C *** for missing Et calulations -> force only use of basic resonse
C *** correction
C
        DO_UNDEVT_CORR=.FALSE.
        DO_ZSP_CORR=.FALSE.
        DO_OUTOFCONE_CORR=.FALSE.
      ENDIF
C
      ZVERT=0.
      LVERH=GZVERH()
      IF(LVERH.GT.0)THEN
        LVERT=LQ(LVERH-IZVERT)
        IF(LVERT.GT.0)THEN
          ZVERT=Q(LVERT+5)   ! z-position of vertex
        ENDIF
      ENDIF
      IF (JET_CONE.EQ.0.5) THEN
C
C *** For 0.5 cone jets - first match with 0.7 cone jets
C
        CALL JET_CONE_CORR(LJETS,DO_ZSP_CORR,DO_UNDEVT_CORR,
     1    DO_OUTOFCONE_CORR,EM_COR,ZVERT,ISYS,EOUT,ETOUT,ETAOUT,
     2    PHIOUT,OUTVEC,MATCH)
        TOP_LEPTONS_JET_CORRECTION = EOUT
      ELSE
C
C *** Use QCD group code for cone jets (version - new as of 10/14/93)
C
        CALL QCD_JET_CORRECTION(LJETS,DO_ZSP_CORR,DO_UNDEVT_CORR,
     1    DO_OUTOFCONE_CORR,ZVERT,ISYS,NEW_JET_E,NEW_JET_ET,
     2    NEW_JET_ETA,IER)
        TOP_LEPTONS_JET_CORRECTION = NEW_JET_E
      ENDIF
  999 RETURN
 1000 FORMAT(//,' ==> Illegal Correction Request : ICOR = ',I3,
     1 ' TOP_LEPTONS_JET_CORRECTION <== ',//)
 1100 FORMAT(//,' ==> Illegal Correction/Jet Cone Request : ICOR',
     1 ',JET_CONE = ',I3,F6.3,' <== ',//)
      END

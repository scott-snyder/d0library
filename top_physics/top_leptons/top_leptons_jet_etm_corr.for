      SUBROUTINE TOP_LEPTONS_JET_ETM_CORR(DMET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute Missing Et Correction vector
C-
C-   Inputs  : None except via RCP file
C-   Outputs : DMET : vectorial correction to Missing Et
C-   Controls: None except via RCP file
C-
C-   Created  12-JUL-1993   Stephen J. Wimpenny
C-   Modifed  27-Jul-1993   Monte Carlo corrections added
C-   Modified 29-Jul-1993   Format of calls to SET CAPH modified
C-   Modified 14-Oct-1993   Calls to MC_ET_CORR changed to matched
C-                          new PHYSICS$UTIL version
C-   Re-written 20-Nov-93   Uses Physics_Util version of Missing Et
C-                          correction logic. Note that e/photon/jets
C-                          are all now handled by this one routine.
C-   Cleaned up 30-Mar-1994 LJETS DO loop removed. Also old hot cell
C-                          jet killer removed.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVERT.LINK/LIST'
C
      EXTERNAL TOP_LEPTONS_JET_CORRECTION,TOP_LEPTONS_UTIL_MONTECARLO
C 
      LOGICAL FIRST,TOP_LEPTONS_UTIL_MONTECARLO
C
      REAL DMET(3),MET_CORR(3),ZVERT,EM_COR(3)
      REAL TOP_LEPTONS_JET_CORRECTION
      REAL CONE_TEMPLATE_7(3)
C
      INTEGER I,IER,LJETS,VERSION,PASS,N,LVERH,LVERT,GZVERH
      INTEGER GZJETS
C
      DATA FIRST/.TRUE./
      DATA CONE_TEMPLATE_7/ 1., 6., 0.7/
C
      IF (FIRST) THEN
        IER = 0
C
C *** Get all latest parameter/Options Values
C
        CALL EZPICK('TOP_LEPTONS_RCP')
C
C *** Calorimeter em-scale corrections (ECN,CC,ECS)
C
        IF(IER.EQ.0) CALL EZGETA_i('EM_SCALE_FACTORS',0,0,0,N,IER)
        IF(IER.EQ.0) CALL EZGETA('EM_SCALE_FACTORS',1,N,1,EM_COR,IER)
C
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_JET_ETM_CORR',' ','F')
        FIRST = .FALSE.
        CALL EZRSET
      ENDIF
C
C *** Initialized local missing Et correction array
C
      DO I = 1, 3
        DMET(I) = 0.0
      ENDDO
C
C *** Jet correction to missing Et - loop over all 0.7 cone jets,
C *** suppressing jets which match to identified electrons or photons
C *** since this is the first cal to jet logic -> setup 0.7 cone
C *** CAPH
C
      CALL SET_CAPH('CONE_JET',CONE_TEMPLATE_7,IER)
      IF(IER.LT.0) THEN
        WRITE(12,1000) IER
        CALL ERRMSG(' Problem with SET_CAPH','TOP_LEPTONS_JET_ETM_CORR',
     1    ' ','F')
      ENDIF
C
      LJETS=GZJETS()
      IF(LJETS.NE.0) THEN
C
C *** Get vertex z-position for Etmiss calculations
C
        ZVERT=0.
        LVERH=GZVERH()
        IF(LVERH.GT.0)THEN
          LVERT=LQ(LVERH-IZVERT)
          IF(LVERT.GT.0)THEN
            ZVERT=Q(LVERT+5) 
          ENDIF
        ENDIF
C
        IF(.NOT.TOP_LEPTONS_UTIL_MONTECARLO()) THEN
C
C *** Data
C *** set iuse Reco Version flag to appropriate RECO version Number
C
          CALL RECO_VERSION(VERSION,PASS)
C
C *** Protect against corrupted header data
C
          IF(VERSION.LT.1) VERSION=11
C
C *** Dhiman's missing Et correction routine
C
          CALL MISS_ET_CORR(VERSION,ZVERT,EM_COR,MET_CORR)
C
C *** Invert corrections to change to TOP_LEPTONS standard (ie. additive
C *** corrections) and store results.
C
          DMET(1)=-1.*MET_CORR(1)
          DMET(2)=-1.*MET_CORR(2)
          DMET(3)=-1.*MET_CORR(3)
        ELSE
C
C *** Monte Carlo
C *** set RECO Version to 0 to tell correction logic
C
          VERSION=0
C
C *** Dhiman's missing Et correction routine
C
          CALL MISS_ET_CORR(VERSION,ZVERT,EM_COR,MET_CORR)
C
C *** Invert corrections to change to TOP_LEPTONS standard (ie. additive
C *** corrections) and store results.
C
          DMET(1)=-1.*MET_CORR(1)
          DMET(2)=-1.*MET_CORR(2)
          DMET(3)=-1.*MET_CORR(3)
        ENDIF
      ENDIF
C
C *** Reset CAPH Algorithm
C
      CALL RESET_CAPH
C
  999 RETURN
 1000 FORMAT(//,' ==> Error from SET_CAPH : IER = ',I5,
     1 ' TOP_LEPTONS_JET_ETM_CORR <== ',//)
      END

      SUBROUTINE TOP_LEPTONS_FIND_WMU(IFGOOD,NOMU,NOEL,NOPH,
     1  NOJT,MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : event search routine to look for top->mu mu
C-                                event candidates
C-
C-   Inputs  : 
C-             NOMU         - no of 'good' PMUO candidates
C-             NOEL         - no of 'good' PELC candidates
C-             NOPH         - no of 'good' PPHO candidates
C-             NOJT         - no of 'good' JETS candidates
C-
C-   RCP file parameters :
C-
C-             MISSET_MIN_CORR - minimum missing Et (muon corrected)
C-
C-   Outputs : 
C-              IFGOOD = .TRUE./.FALSE. - event is/isnt a candidate
C-
C-   Controls: 
C-
C-   Created  29-JUL-1992   Stephen J. Wimpenny
C-   Modified 15-Sep-1992   Modified to use Good_Muon Logical
C-   Modified 17-Sep-1992   Read cuts from RCP file
C-   Modified 20-Oct-1992   Isolation cuts added
C-   Modified 15-Mar-1993   Name change for Good_Muon logical,
C-                          Nearjet
C-   Modified 17-Jul-1993   Uses new isolated muon logic
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      EXTERNAL TOP_LEPTONS_GOOD_MUON,TOP_LEPTONS_GOOD_JET
C
      LOGICAL FIRST,IFGOOD
      LOGICAL TOP_LEPTONS_GOOD_MUON,TOP_LEPTONS_GOOD_JET
C
      INTEGER NOMU,NOEL,NOPH,NOJT,IER
      INTEGER LPMUO,LPNUT,LJETS,GZPMUO,GZPNUT,GZJETS
      INTEGER I_MU,I_JT,I_MU_ISOL,MODE
      INTEGER LJETS_VEC(10),LPMUO_VEC(5),LPMUO_VEC_ISOL(5)
C
      REAL MISSET_CORR,MISSET_MIN_CORR
      REAL MET_VEC(3)
C
      DATA FIRST,MODE/.TRUE.,1/
C
C *** Read cut parameters from RCP file
C
      IER=0
      IF(FIRST) THEN
        CALL EZPICK('TOP_LEPTONS_RCP')
C
        CALL EZGET('PNUT3_ETMIN_W',MISSET_MIN_CORR,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_FIND_WMU',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      IFGOOD=.FALSE.
      I_MU=0
C
C *** Require at least 1 'good' PMUO bank
C
      IF(NOMU.LT.1) GO TO 999
C
C *** Get PNUT Bank Info
C
      MISSET_CORR=0.
      LPNUT=GZPNUT(3)
      IF(LPNUT.NE.0) THEN
        MISSET_CORR=Q(LPNUT+7)
      ENDIF
C
      IF(NOMU.GT.5) CALL ERRMSG('Muon Store Trucated at 5',
     1 'TOP_LEPTONS_FIND_WMU',' ','W')
C
      LPMUO=GZPMUO(0)
      DO WHILE(LPMUO.NE.0)
        IF(TOP_LEPTONS_GOOD_MUON(LPMUO)) THEN
          I_MU=I_MU+1
          IF(I_MU.LT.6) LPMUO_VEC(I_MU)=LPMUO
        ENDIF
        LPMUO=LQ(LPMUO)
      ENDDO
      IF(I_MU.LT.1) GO TO 999
C
C *** JETS
C
      I_JT=0
      LJETS=GZJETS()
      DO WHILE(LJETS.GT.0)
        IF(TOP_LEPTONS_GOOD_JET(LJETS)) THEN
          I_JT=I_JT+1
          IF(I_JT.LT.11) LJETS_VEC(I_JT)=LJETS
        ENDIF
        LJETS=LQ(LJETS)
      ENDDO
      IF(I_JT.GT.10) I_JT=10
C
C *** Look for an isolated muon above threshold
C
      CALL TOP_LEPTONS_FIND_ISOLMU(I_MU,LPMUO_VEC,I_MU_ISOL,
     1  LPMUO_VEC_ISOL,I_JT,MODE)
      IF(I_MU_ISOL.LT.1) GO TO 999
C
C *** Cut on missing energy
C
      IF(MISSET_CORR.LT.MISSET_MIN_CORR) GO TO 999
      IFGOOD=.TRUE.
C----------------------------------------------------------------------
  999 RETURN
      END

      SUBROUTINE ETA_DEPENDENCE(CONESIZE,ET,DET_ETA,ETA_CORR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To correct jets due to the eta dependence
C-                         of the energy scale
C-
C-  Eta dependence is fit in bins of 0.1 in eta from 0 to 2.0
C-  correction factor =m*et+b for each conesize
C-  i.e. pb3   b for positive eta conesize=0.3
C-       nm7   m for negative eta conesize=0.7
C-
C-   Inputs  : Conesize  R -- Conesize of jet
C-             Et        R -- Et of jet
C-             Det_Eta   R -- Detector eta of jet
C-
C-
C-   Outputs : Eta_corr  R -- Eta dependent correction factor
C-
C-   Controls:
C-
C-   Created   7-OCT-1996   Brad Abbott -- now cafix51 correction
C-   Modified 28-OCT-1996   Brad Abbott  positive and negative Eta 
C-                                       get different corrections.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL   CONESIZE,ET,DET_ETA,ETA_CORR,NEW_ETA,NEW_ET
      REAL   pB3(20),pM3(20),pB5(20),pM5(20)
      REAL   pB7(20),pM7(20),pB10(20),pM10(20)
      REAL   nB3(20),nM3(20),nB5(20),nM5(20)
      REAL   nB7(20),nM7(20),nB10(20),nM10(20)
      INTEGER I,lrcp,ier
      LOGICAL first,ok
      DATA first /.true./
C----------------------------------------------------------------------
      IF(first) THEN
        first=.false.
        CALL ezloc('QCD_JET_CORRECTION_RCP',lrcp)
        ok = lrcp .GT. 0
        IF (.NOT. ok) THEN
          CALL inrcp('QCD_JET_CORRECTION_RCP',ier)
          IF (ier.EQ.0) CALL ezpick('QCD_JET_CORRECTION_RCP')
          IF (ier.EQ.0) CALL ezerr(ier)
          IF (ier.NE.0) THEN
            CALL errmsg('RCP not found','ETA_DEPENDENCE',
     &        'QCD_JET_CORRECTION_RCP','F')
          ENDIF
          CALL ezrset
        ENDIF
        CALL ezpick('QCD_JET_CORRECTION_RCP')
        CALL ezerr(ier)
        IF (ier.EQ.0) THEN    ! *** read in RCP parameters ***
          IF(ier.EQ.0) CALL EZGETA('PB3',1,20,1,PB3,IER)
          IF(ier.EQ.0) CALL EZGETA('PM3',1,20,1,PM3,IER)
          IF(ier.EQ.0) CALL EZGETA('PB5',1,20,1,PB5,IER)
          IF(ier.EQ.0) CALL EZGETA('PM5',1,20,1,PM5,IER)
          IF(ier.EQ.0) CALL EZGETA('PB7',1,20,1,PB7,IER)
          IF(ier.EQ.0) CALL EZGETA('PM7',1,20,1,PM7,IER)
          IF(ier.EQ.0) CALL EZGETA('PB10',1,20,1,PB10,IER)
          IF(ier.EQ.0) CALL EZGETA('PM10',1,20,1,PM10,IER)
          IF(ier.EQ.0) CALL EZGETA('NB3',1,20,1,NB3,IER)
          IF(ier.EQ.0) CALL EZGETA('NM3',1,20,1,NM3,IER)
          IF(ier.EQ.0) CALL EZGETA('NB5',1,20,1,NB5,IER)
          IF(ier.EQ.0) CALL EZGETA('NM5',1,20,1,NM5,IER)
          IF(ier.EQ.0) CALL EZGETA('NB7',1,20,1,NB7,IER)
          IF(ier.EQ.0) CALL EZGETA('NM7',1,20,1,NM7,IER)
          IF(ier.EQ.0) CALL EZGETA('NB10',1,20,1,NB10,IER)
          IF(ier.EQ.0) CALL EZGETA('NM10',1,20,1,NM10,IER)
          CALL ezrset
          IF (ier.NE.0) THEN
            CALL errmsg('RCP error','ETA_DEPENDENCE',
     &        'Read error:abort ','F')
            ier = -3
            GOTO 999
          ENDIF
        ELSE
          CALL errmsg('NO QCD_JET_CORRECTION_RCP',
     &      'Eta_dependence','NO RCP file to work with','F')
          ier = -2
          GOTO 999
        ENDIF
      ENDIF
C__________________________________________________________________-

      IF(DET_ETA.GE.0) NEW_ETA=DET_ETA+.04999
      IF(DET_ETA.LT.0) NEW_ETA=DET_ETA-.04999

      IF(det_eta.GE.0) THEN
        I=NINT(ABS(NEW_ETA)*10.)
        ETA_CORR=1.0
        IF((I.GT.0).AND.(I.LE.20)) THEN

          IF(CONESIZE.EQ.0.3) THEN
            NEW_ET=.99*ET+8.12
            ETA_CORR=pM3(I)*NEW_ET+pB3(I)
          ENDIF

          IF(CONESIZE.EQ.0.5) THEN
            NEW_ET=.98*ET+4.52
            ETA_CORR=pM5(I)*NEW_ET+pB5(I)
          ENDIF

          IF(CONESIZE.EQ.0.7) ETA_CORR=pM7(I)*ET+pB7(I)

          IF(CONESIZE.EQ.1.0) THEN
            NEW_ET=.94*ET-3.26
            IF(NEW_ET.LT.0) NEW_ET=0
            ETA_CORR=pM10(I)*NEW_ET+pB10(I)
          ENDIF

        ENDIF
      ENDIF

      IF(det_eta.LT.0) THEN
        I=NINT(ABS(NEW_ETA)*10.)
        ETA_CORR=1.0
        IF((I.GT.0).AND.(I.LE.20)) THEN

          IF(CONESIZE.EQ.0.3) THEN
            NEW_ET=.99*ET+8.12
            ETA_CORR=nM3(I)*NEW_ET+nB3(I)
          ENDIF

          IF(CONESIZE.EQ.0.5) THEN
            NEW_ET=.98*ET+4.52
            ETA_CORR=nM5(I)*NEW_ET+nB5(I)
          ENDIF

          IF(CONESIZE.EQ.0.7) ETA_CORR=nM7(I)*ET+nB7(I)

          IF(CONESIZE.EQ.1.0) THEN
            NEW_ET=.94*ET-3.26
            IF(NEW_ET.LT.0) NEW_ET=0
            ETA_CORR=nM10(I)*NEW_ET+nB10(I)
          ENDIF
        ENDIF
      ENDIF

      IF(eta_corr.LT.1.5) ETA_CORR=1.0/(2.0-ETA_CORR)
      IF(eta_corr.GT.1.5) eta_corr=1.5

  999 RETURN
      END

      FUNCTION NP_LQ_2EM_TIGHT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Selects Ngood e + Ngood phot .ge. 2, where
C-           good e == et.gt.15
C-                     emfrac.gt.0.9
C-                     isol.lt.0.2 for 15<et<20 (W/Z iso variable)
C-                     chisq.lt.300. for 15<et<20
C-           good phot == et.gt.10
C-                     emfrac.gt.0.9
C-                     isol.lt.0.2
C-                     chisq.lt.300. for 10<et<20
C-                     
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  ??-???-1993   K. Wyatt Merritt
C-   Updated  23-JUN-1993   K. Wyatt Merritt  Change electron isolation cut
C-                            to apply only below 20 GeV, like the chisq cut
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL NP_LQ_2EM_TIGHT,NP_LQ_2EM_TIGHT_EOJ
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LPELC,LPPHO,LHMTE,LHMTP
      INTEGER GZPELC,GZPPHO,LCACL
      INTEGER NELEC,NPHOT
      INTEGER LPELCMX,ICHOICE,IER,I
C
      REAL RSUM(20),RSUMMARY(20)
      REAL    ET,ELC_ET,ELC_ETA,ELC_PHI,EM_EMF
      REAL ISOLATE, PHOT_ET,PH_EMF,TCHISQR
      REAL ISOCUT,TCHISQ_CUT,EMFRAC_CUT
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C
C
      IF(FIRST) THEN
        FIRST=.FALSE.
C
        CALL INRCP ('NP_LQ_2EM_TIGHT_RCP', IER)
        IF (IER.EQ.0) THEN
          CALL EZPICK ('NP_LQ_2EM_TIGHT_RCP')
          CALL EZGET ('ELE_PTCUT', ELC_ET, IER)
          CALL EZGET ('PHOT_PTCUT', PHOT_ET, IER)
          CALL EZGET ('ISOCUT', ISOCUT, IER)
          CALL EZGET ('TCHISQ_CUT', TCHISQ_CUT, IER)
          CALL EZGET ('EMFRAC_CUT', EMFRAC_CUT, IER)
          CALL EZRSET
        ELSE
          CALL ERRMSG ('No NP_LQ_2EM_TIGHT_RCP', 'NP_LQ_2EM_TIGHT',
     &        'Could not find NP_LQ_2EM_TIGHT_RCP', 'F')
        ENDIF                           ! if ier .eq. 0
        CALL VZERO (RSUMMARY, 20)
      ENDIF
C
C
C       electrons
C
      NELEC=0
      LPELC=GZPELC()
C
      IF(LPELC.NE.0) THEN
C
C         loop through electron banks and pick maximum
        DO WHILE (LPELC.GT.0)
          LHMTE=LQ(LPELC-1)
          LCACL=LQ(LPELC-2)
          ET=Q(LPELC+7)
          IF(ET.GT.ELC_ET) THEN
            ELC_ETA=Q(LPELC+9)
            ELC_PHI=Q(LPELC+10)
            IF ( Q(LPELC+17).NE.0. .AND. ET.LT.20. ) THEN
              ISOLATE = (Q(LPELC+16)-Q(LPELC+17))/Q(LPELC+17)
            ELSE
              ISOLATE = 0.
            ENDIF
            IF(ET.LT.20.) THEN
              TCHISQR=Q(LHMTE+7)
            ELSE
              TCHISQR=0.0
            ENDIF
            EM_EMF = (Q(LCACL+7)-Q(LCACL+19))/Q(LCACL+7)
            IF((ISOLATE.LE.ISOCUT).AND.(EM_EMF.GE.EMFRAC_CUT).AND.
     &        (TCHISQR.LT.TCHISQ_CUT)) NELEC=NELEC+1
          ENDIF
          LPELC=LQ(LPELC)          ! pointer to next electron
        ENDDO
C
C
      ENDIF
C
C
C       photons
C
      NPHOT=0
      LPPHO=GZPPHO()
C
      IF(LPPHO.NE.0) THEN
C
C         loop through photon banks and pick maximum
        DO WHILE (LPPHO.GT.0)
          LHMTP=LQ(LPPHO-1)
          LCACL=LQ(LPPHO-2)
          ET=Q(LPPHO+7)
          IF(ET.GT.PHOT_ET) THEN
            IF ( Q(LPPHO+17) .NE. 0.) THEN
              ISOLATE = (Q(LPPHO+16)-Q(LPPHO+17))/Q(LPPHO+17)
            ELSE
              ISOLATE = 0.
            ENDIF
            IF(ET.LT.20.) THEN
              TCHISQR=Q(LHMTP+7)
            ELSE
              TCHISQR=0.0
            ENDIF
            EM_EMF = (Q(LCACL+7)-Q(LCACL+19))/Q(LCACL+7)
            IF((ISOLATE.LE.ISOCUT).AND.(EM_EMF.GE.EMFRAC_CUT).AND.
     &         (TCHISQR.LT.TCHISQ_CUT)) NPHOT=NPHOT+1
          ENDIF
          LPPHO=LQ(LPPHO)          ! pointer to next photon
        ENDDO
C
      ENDIF
C
C
      IF ((NPHOT+NELEC).GE.2) THEN
        NP_LQ_2EM_TIGHT = .TRUE.
        IF (NELEC .GE. 2) THEN
          RSUMMARY(1) = RSUMMARY(1) + 1
        ENDIF
        IF (NPHOT .GE. 2) THEN
          RSUMMARY(2) = RSUMMARY(2) + 1
        ENDIF
        IF (NELEC .GE. 1 .AND. NPHOT .GE. 1) THEN
          RSUMMARY(3) = RSUMMARY(3) + 1
        ENDIF
      ELSE
        NP_LQ_2EM_TIGHT = .FALSE.
      END IF
C
C
C
  999 RETURN
C
      ENTRY NP_LQ_2EM_TIGHT_EOJ(RSUM)
      NP_LQ_2EM_TIGHT_EOJ = .TRUE.
      DO I = 1 , 20
        RSUM(I) = RSUMMARY(I)
      ENDDO
      RETURN
      END

      LOGICAL FUNCTION TOP_EJET() 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Filter W/e+jet events for RGE stream according to 
C-                         the following criteria 
C-     - PELC  with pt > ELEC_MIN_ET 
C-     - PPHO  with pt > PHOT_MIN_ET 
C-     - PNUT2 with pt > ETMISS_MIN 
C-
C-   Controls: TOP_EJET_RCP 
C-   Returned value  : true for events that pass 
C-
C-   Modified  26-JUL-1993   Pushpa Bhat  modified from Marcel's ELF_W 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER I
      INTEGER LPELC,LPPHO,LPNUT
      INTEGER GZPELC,GZPPHO,GZPNUT 
      INTEGER EVTNUM,RUNNUM,RUNNO
      INTEGER NELEC,NPHOT,NELPH
      INTEGER IER
      REAL    PHOT_ET
      REAL    ELETMIN, PHETMIN, ETMISS, ETMISSMIN
      REAL    ET,ECLUS,ELC_ET 
      REAL    W_STAT(20),W_SUMRY(20)
      LOGICAL FIRST,FLAG_ELEC,FLAG_PHOT,FLAG_MET
      LOGICAL PASSED,LOK
      LOGICAL TOP_EJET_EOJ
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
C
C ****  initialization 
C
      PASSED    = .FALSE.
      TOP_EJET     = .FALSE.           
      FLAG_ELEC = .FALSE.
      FLAG_PHOT = .FALSE.
      FLAG_MET  = .FALSE. 
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('TOP_EJET_RCP',IER)       
        IF(IER.EQ.0) THEN
          CALL EZPICK('TOP_EJET_RCP')  
          CALL EZGET('ELEC_MIN_ET',ELETMIN,IER)
          CALL EZGET('PHOT_MIN_ET',PHETMIN,IER)
          CALL EZGET('ETMISS_MIN', ETMISSMIN,IER)
          IF (IER.NE.0) THEN
            CALL ERRMSG('TOP_EJET','TOP_EJET_RCP',
     &        'ERROR GETTING TOP_EJET RCP VALUES','W')
          ENDIF
          CALL EZRSET
          CALL VZERO(W_STAT,20)
        ELSE
          CALL ERRMSG('TOP_EJET','TOP_EJET_RCP',
     &      'ERROR READING TOP_EJET RCP FILE','F')
        ENDIF
      ENDIF
C
C------------------------------------------------------------------------------
C
      IF(LHEAD.NE.0) EVTNUM = IQ(LHEAD+9) 
      RUNNUM = RUNNO()
C
C
C ****  electrons
C
      W_STAT(1)=W_STAT(1)+1.
C
      NELEC=0
      LPELC=GZPELC()
      ELC_ET=ELETMIN
      IF(LPELC.NE.0) THEN
        DO WHILE (LPELC.GT.0)
          ET=Q(LPELC+7)
          IF(ET.LT.ELC_ET) GOTO 60 
C
          NELEC=NELEC+1
          FLAG_ELEC = .TRUE. 
C
   60     CONTINUE
          LPELC=LQ(LPELC)          
        ENDDO
      ENDIF
C
C
C ****  missing ET
C
      ETMISS=ETMISSMIN
      LPNUT=GZPNUT(2)     
      IF(LPNUT.GT.0) THEN
        ET=Q(LPNUT+7)
        IF(ET.GT.ETMISS) FLAG_MET = .TRUE. 
      ENDIF
C
C
C ****  photons
C
      NPHOT=0
      LPPHO=GZPPHO()
      PHOT_ET=PHETMIN
      IF(LPPHO.NE.0) THEN
        DO WHILE (LPPHO.GT.0)
          ET=Q(LPPHO+7)
          IF(ET.LT.PHOT_ET) GOTO 80 
C
          NPHOT=NPHOT+1
          FLAG_PHOT = .TRUE. 
C
   80     CONTINUE
          LPPHO=LQ(LPPHO)          ! pointer to next photon
        ENDDO
      ENDIF
C
C
C
      IF(FLAG_MET) THEN 
        IF(NELEC.EQ.1.AND.NPHOT.EQ.0) THEN
          W_STAT(3)=W_STAT(3)+1.
        ELSEIF(NELEC.GT.1.AND.NPHOT.EQ.0) THEN 
          W_STAT(5)=W_STAT(5)+1.
        ENDIF
        IF(NPHOT.EQ.1.AND.NELEC.EQ.0) THEN
          W_STAT(4)=W_STAT(4)+1.
        ELSEIF(NPHOT.GT.1.AND.NELEC.EQ.0) THEN 
          W_STAT(6)=W_STAT(6)+1.
        ENDIF
        IF(NELEC.GE.1.AND.NPHOT.GE.1) W_STAT(7)=W_STAT(7)+1.
        IF(NELEC.GE.1.OR. NPHOT.GE.1) W_STAT(2)=W_STAT(2)+1.
      ENDIF
C
C
C ****  Write out event?
C
      NELPH = NELEC+NPHOT
      IF (NELPH.GE.1 .AND. FLAG_MET) PASSED = .TRUE. 
C
      TOP_EJET = PASSED 
C
  999 RETURN
C
C
C----------------------------------------------------------------------
      ENTRY TOP_EJET_EOJ(W_SUMRY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Some end of job statistics for W/e+jet filter
C-
C-   W_SUMRY  1 = total number of events on which filter was run 
C-            2 = total number of candidates
C-            3 = # of single electron candidates, no gamma's (%)
C-            4 = # of single gamma candidates, no electrons (%)
C-            5 = # evts with more than 1 electron candidate, no gamma's (%)
C-            6 = # evts with more than 1 gamma candidate, no electrons (%)
C-            7 = # evts with more than 1 elec and 1 gamma candidate (%)
C-
C----------------------------------------------------------------------
      TOP_EJET_EOJ=.TRUE.
      CALL UCOPY(W_STAT,W_SUMRY,20)
      IF(W_STAT(1).EQ.0.) RETURN
      DO I=3,20
        W_SUMRY(I)=W_STAT(I)/W_STAT(1)*100.
      ENDDO
      RETURN
      END

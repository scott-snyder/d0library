      LOGICAL FUNCTION TOP_EE() 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Filter ee events for RGE stream according to 
C-                         the following criteria 
C-     - PELC  with pt > ELEC_MIN_ET 
C-     - PPHO  with pt > PHOT_MIN_ET 
C-
C-   Controls: TOP_EE_RCP 
C-   Returned value  : true for events that pass 
C-
C-   Created  19-JUL-1993   Pushpa Bhat  Modified from Marcel's ELF_Z
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER I,IER
      INTEGER LPELC,LPPHO 
      INTEGER GZPELC,GZPPHO 
      INTEGER EVTNUM,RUNNUM,RUNNO
      INTEGER NELEC,NPHOT,NELPH
      REAL    PHOT_ET
      REAL    ELETMIN,PHETMIN,ET,ELC_ET
      REAL    EE_STAT(20),EE_SUMRY(20)
      LOGICAL FIRST,FLAG_ELEC,FLAG_PHOT
      LOGICAL PASSED,LHOT,LOK
      LOGICAL TOP_EE_EOJ
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
C
C ****  initialization 
C
      PASSED    = .FALSE.
      TOP_EE    = .FALSE.           
      FLAG_ELEC = .FALSE.
      FLAG_PHOT = .FALSE.
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('TOP_EE_RCP',IER)       
        IF(IER.EQ.0) THEN
          CALL EZPICK('TOP_EE_RCP')  
          CALL EZGET('ELEC_MIN_ET',ELETMIN,IER)
          CALL EZGET('PHOT_MIN_ET',PHETMIN,IER)
          IF (IER.NE.0) THEN
            CALL ERRMSG('TOP_EE','TOP_EE_RCP',
     &        'ERROR GETTING TOP_EE RCP VALUES','W')
          ENDIF
          CALL EZRSET
          CALL VZERO(EE_STAT,20)
        ELSE
          CALL ERRMSG('TOP_EE','TOP_EE_RCP',
     &      'ERROR READING TOP_EE RCP FILE','F')
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
      EE_STAT(1)=EE_STAT(1)+1.
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
      IF(NELEC.EQ.2.AND.NPHOT.EQ.0) THEN
        EE_STAT(3)=EE_STAT(3)+1.
      ELSEIF(NELEC.GT.2.AND.NPHOT.EQ.0) THEN 
        EE_STAT(6)=EE_STAT(6)+1.
      ENDIF
      IF(NPHOT.EQ.2.AND.NELEC.EQ.0) THEN
        EE_STAT(4)=EE_STAT(4)+1.
      ELSEIF(NPHOT.GT.2.AND.NELEC.EQ.0) THEN 
        EE_STAT(7)=EE_STAT(7)+1.
      ENDIF
      IF(NELEC.EQ.1.AND.NPHOT.EQ.1) EE_STAT(5)=EE_STAT(5)+1.
      IF(NELEC.GE.2.AND.NPHOT.GE.1 .OR. 
     &   NPHOT.GE.2.AND.NELEC.GE.1) EE_STAT(8)=EE_STAT(8)+1.
C
C
C
C ****  Write out event?
C
      NELPH = NELEC + NPHOT    
      IF (NELPH .GE. 2) PASSED = .TRUE. 
      IF (NELPH .GE. 2) EE_STAT(2) = EE_STAT(2)+1. 
C
      TOP_EE = PASSED 
C
  999 RETURN
C
C
C----------------------------------------------------------------------
      ENTRY TOP_EE_EOJ(EE_SUMRY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Some end of job statistics for Z filter
C-
C-   EE_SUMRY 1 = total number of events on which filter was run 
C-            2 = total number of candidates
C-            3 = # of di-electron candidates, no gamma's (%)
C-            4 = # of di-gamma candidates, no electrons (%)
C-            5 = # of electron-gamma candidates (%)
C-            6 = # evts with more than 2 electron candidates, no gamma's (%)
C-            7 = # evts with more than 2 gamma candidates, no electrons (%)
C-            8 = # evts with more than 1 elec and 1 gamma candidate (%)
C-
C----------------------------------------------------------------------
      TOP_EE_EOJ=.TRUE.
      CALL UCOPY(EE_STAT,EE_SUMRY,20)
      IF(EE_STAT(1).EQ.0.) RETURN
      DO I=3,10
        EE_SUMRY(I)=EE_STAT(I)/EE_STAT(1)*100.
      ENDDO
      RETURN
      END

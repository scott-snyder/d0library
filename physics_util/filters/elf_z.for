      LOGICAL FUNCTION ELF_Z() 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Filter Z events from ELF stream according to 
C-                         the following criteria 
C-     - PELC  with pt > ELEC_MIN_ET 
C-     - PPHO  with pt > PHOT_MIN_ET 
C-
C-   Controls: ELF_Z_RCP 
C-   Returned value  : true for events that pass 
C-
C-   Created  11-DEC-1992   Marcel Demarteau
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
      REAL    Z_STAT(20),Z_SUMRY(20)
      LOGICAL FIRST,FLAG_ELEC,FLAG_PHOT
      LOGICAL PASSED,LHOT,LOK
      LOGICAL ELF_Z_EOJ
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
C
C ****  initialization 
C
      PASSED    = .FALSE.
      ELF_Z     = .FALSE.           
      FLAG_ELEC = .FALSE.
      FLAG_PHOT = .FALSE.
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('ELF_Z_RCP',IER)       
        IF(IER.EQ.0) THEN
          CALL EZPICK('ELF_Z_RCP')  
          CALL EZGET('ELEC_MIN_ET',ELETMIN,IER)
          CALL EZGET('PHOT_MIN_ET',PHETMIN,IER)
          IF (IER.NE.0) THEN
            CALL ERRMSG('ELF_Z','ELF_Z_RCP',
     &        'ERROR GETTING ELF_Z RCP VALUES','W')
          ENDIF
          CALL EZRSET
          CALL VZERO(Z_STAT,20)
        ELSE
          CALL ERRMSG('ELF_Z','ELF_Z_RCP',
     &      'ERROR READING ELF_Z RCP FILE','F')
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
      Z_STAT(1)=Z_STAT(1)+1.
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
        Z_STAT(3)=Z_STAT(3)+1.
      ELSEIF(NELEC.GT.2.AND.NPHOT.EQ.0) THEN 
        Z_STAT(6)=Z_STAT(6)+1.
      ENDIF
      IF(NPHOT.EQ.2.AND.NELEC.EQ.0) THEN
        Z_STAT(4)=Z_STAT(4)+1.
      ELSEIF(NPHOT.GT.2.AND.NELEC.EQ.0) THEN 
        Z_STAT(7)=Z_STAT(7)+1.
      ENDIF
      IF(NELEC.EQ.1.AND.NPHOT.EQ.1) Z_STAT(5)=Z_STAT(5)+1.
      IF(NELEC.GE.2.AND.NPHOT.GE.1 .OR. 
     &   NPHOT.GE.2.AND.NELEC.GE.1) Z_STAT(8)=Z_STAT(8)+1.
C
C
C
C ****  Write out event?
C
      NELPH = NELEC + NPHOT    
      IF (NELPH .GE. 2) PASSED = .TRUE. 
      IF (NELPH .GE. 2) Z_STAT(2) = Z_STAT(2)+1. 
C
      ELF_Z = PASSED 
C
  999 RETURN
C
C
C----------------------------------------------------------------------
      ENTRY ELF_Z_EOJ(Z_SUMRY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Some end of job statistics for Z filter
C-
C-   Z_SUM    1 = total number of events on which filter was run 
C-            2 = total number of candidates
C-            3 = # of di-electron candidates, no gamma's (%)
C-            4 = # of di-gamma candidates, no electrons (%)
C-            5 = # of electron-gamma candidates (%)
C-            6 = # evts with more than 2 electron candidates, no gamma's (%)
C-            7 = # evts with more than 2 gamma candidates, no electrons (%)
C-            8 = # evts with more than 1 elec and 1 gamma candidate (%)
C-
C----------------------------------------------------------------------
      ELF_Z_EOJ=.TRUE.
      CALL UCOPY(Z_STAT,Z_SUMRY,20)
      IF(Z_STAT(1).EQ.0.) RETURN
      DO I=3,10
        Z_SUMRY(I)=Z_STAT(I)/Z_STAT(1)*100.
      ENDDO
      RETURN
      END

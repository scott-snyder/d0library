      FUNCTION CTAUS_ANL(DO_ANALYSIS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-         histograms and summaries for CTAUS package 
C-   Input:
C-     DO_ANALYSIS= true, do full analysis
C-
C-   ENTRY CTAUS_FIN
C-      contribution to job summary
C-
C-   Created  27-SEP-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL DO_ANALYSIS
      INTEGER IER,LPTAU,GZPTAU,LZTRK,NTRK,NSUM,LZFIT
      INTEGER LISV1,GZISAE,GZISV1,ID
      INTEGER LUN,SSUNIT
      REAL    ET,TH,PHI,ETA,RMS,CALLS,AVRG,ETSUM,ETAVRG
      REAL    DEL_PHI,DEL_TH,DEL_R
      LOGICAL CTAUS_ANL, CTAUS_FIN
      LOGICAL FIRST,FLGVAL,VERIFY,MONTE,NO_MATCH
      SAVE FIRST,NSUM,CALLS
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      CALL DHDIR('CTAUS_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory for CTAUS
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('CALORIMETER','CTAUS_ANL',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      VERIFY=DO_ANALYSIS.OR.FLGVAL('VERIFY')
      MONTE=GZISAE().GT.0
      IF(FIRST)THEN
        NSUM=0
        CALLS=0.
        FIRST = .FALSE.
C $$$$$$$$$$$$$$$$$$$$$$$$$$
C $$$$  BOOK HISTOGRAMS HERE
C $$$$$$$$$$$$$$$$$$$$$$$$$$
C
        CALL HBOOK1(1,' ET  of Taus',50,0.,100.,0.)
        IF(VERIFY) THEN
          CALL HBOOK1(2,' R (rms) of Taus',25,0.,.25,0.)
          CALL HBOOK1(3,' R (rms) of 1 trk Taus',25,0.,.25,0.)
          CALL HBOOK1(4,' delta(phi) for 1 trk tau',50,-.25,.25,0.)
          CALL HBOOK1(5,' delta(theta) for 1 trk tau',50,-.25,.25,0.)
          IF(MONTE) THEN
            CALL HBOOK1(6,' R(rms) of matching taus',25,0.,.25,0.)
            CALL HBOOK1(7,' R (rms) of non-matching taus',25,0.,.25,0.)
            CALL HBOOK1(8,' del(R) of matching taus',25,0.,.25,0.)
            CALL HBOOK1(9,' del(R) of 1-trk matching taus',25,0.,.25,0.)
          ENDIF
C
        ENDIF
      ENDIF
C
C $$$$$$$$$$$$$$$$$$$$$$$$$$
C $$$$  FILL HISTOGRAMS HERE
C $$$$$$$$$$$$$$$$$$$$$$$$$$
C
      CALLS=CALLS+1.
      LPTAU=GZPTAU()
      DO WHILE (LPTAU.GT.0)
        NSUM=NSUM+1
        ET=Q(LPTAU+7)
        ETSUM=ETSUM+ET
        RMS=Q(LPTAU+11)
        ETA=Q(LPTAU+10)
        PHI=Q(LPTAU+9)
        TH=Q(LPTAU+8)
        CALL HFILL(1,ET,0,1.)
        IF(VERIFY) CALL HFILL(2,RMS,0,1.)
        NTRK=IQ(LPTAU-3)-IQ(LPTAU-2)-1
C          check track matching for single track taus
        IF(NTRK.EQ.1) THEN
          LZTRK=LQ(LPTAU-3)
          LZFIT=LQ(LZTRK-1)
          CALL HFILL(3,RMS,0,1.)
          IF(VERIFY) THEN
            DEL_PHI=Q(LZFIT+10)-PHI
            DEL_TH=Q(LZFIT+13)-TH
            CALL HFILL(4,DEL_PHI,0,1.)
            CALL HFILL(5,DEL_TH,0,1.)
          ENDIF
        ENDIF
C         check tau matching with Montecarlo
        IF ( VERIFY.AND.MONTE ) THEN
          NO_MATCH=.TRUE.
          LISV1=GZISV1()
          DO WHILE (LISV1.GT.0)
            ID=IABS(IQ(LISV1+1))
            IF ( ID.EQ.16 ) THEN
              CALL ISPETA(Q(LISV1+2),TH,PHI,ETA)
              DEL_R=SQRT((Q(LPTAU+9)-PHI)**2+(Q(LPTAU+10)-ETA)**2)
              IF (DEL_R.LT.RMS+.1 ) THEN
                CALL HFILL(6,RMS,0,1.)   
                CALL HFILL(8,DEL_R,0,1.)
                IF(NTRK.EQ.1) CALL HFILL(9,DEL_R,0,1.)
                NO_MATCH=.FALSE.
              ENDIF
            ENDIF
            LISV1=LQ(LISV1)
          ENDDO
          IF(NO_MATCH) CALL HFILL(7,RMS,0,1.)   
        ENDIF
        LPTAU=LQ(LPTAU)
      ENDDO
      GOTO 999
C
      ENTRY CTAUS_FIN
      CTAUS_FIN=.TRUE.
      LUN=SSUNIT()
      AVRG=NSUM/CALLS
      ETAVRG=ETSUM/NSUM
      WRITE(LUN,FMT=100) NSUM,AVRG,ETAVRG
C        
  999 RETURN
  100 FORMAT(//' CTAUS package:'/,' Total number of taus',I4,
     &  ', average no. of taus/event',F6.3,', average ET',F6.2)
      END

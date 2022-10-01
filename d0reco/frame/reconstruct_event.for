      SUBROUTINE RECONSTRUCT_EVENT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Do event reconstruction
C-
C-   Created   8-SEP-1989   Serban D. Protopopescu
C-   Updated  13-APR-1994   Serban D. Protopopescu  (option to drop MUHT) 
C-   Updated   8-AUG-1994   Qizhong Li-Demarteau  added CPU measurement 
C-   Updated  13-OCT-1994   Qizhong Li-Demarteau  Added ENTRY point 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZGEAN.LINK'
      INCLUDE 'D0$LINKS:IZFAKE.LINK'
      INCLUDE 'D0$LINKS:IZRECO.LINK'
      INTEGER SUM_UNIT,SSUNIT,VERSION,PASS
      INTEGER IER,LPROC,GZPROC,LHSTR,GZHSTR,GZMUHT,LMUHT
      LOGICAL EVENT_RECO_PBD
      LOGICAL FIRST,GEAN,ISAE,FAKE,RECO,PROC,BY_VERSION,MUHT
      LOGICAL CPU_TIMING, L2OK, L2BIT_PASSED
      REAL    BEFORE_RECO, AFTER_RECO, DUMP_RESET, TWRITE, TSUM, FNEVT
      REAL    TOTRECO, TTSUM
      INTEGER NID(5), EVT1, NUMEVT, NEVT, EVTCNT, TOTEVT, FIRSTEVT
      INTEGER L2BIT
      SAVE FIRST,GEAN,ISAE,FAKE,RECO,PROC,MUHT
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST)THEN
        CALL EZPICK('D0RECO_RCP')
        CALL EZGET('GEAN',GEAN,IER)   
        CALL EZGET('ISAE',ISAE,IER)   
        CALL EZGET('FAKE',FAKE,IER)   
        CALL EZGET('RECO',RECO,IER)   
        CALL EZGET('PROC',PROC,IER)   
        CALL EZGET('MUHT',MUHT,IER)   
        CALL EZGET('BY_VERSION',BY_VERSION,IER)   
        CALL EZGET('CPU_TIMING',CPU_TIMING,IER)   
        CALL EZRSET
        IF (CPU_TIMING) THEN
          CALL DHDIR('D0RECO_RCP','HBOOK_DIRECTORY',IER,' ')
          IF (IER .NE. 0) THEN
            CALL ERRMSG('D0RECO','HISTOGRAM',
     &        ' ERROR SETTING HBOOK DIRECTORY ','W')
            CPU_TIMING = .FALSE.
          ELSE
            CALL HBOOK1(11,'READ INPUT CPU$',100,0.0,1.0,0.)
            CALL HBOOK1(12,'RECONSTRUCTION CPU$',200,0.0,100.0,0.)
            CALL HBOOK1(13,'RESET POINTER CPU$',100,0.0,1.0,0.)
            CALL HBOOK1(14,'WRITE OUTPUT CPU$',100,0.0,5.0,0.)
            CALL HBOOK1(15,'RECONSTRUCT EVENT CPU$',200,0.0,100.0,0.)
            CALL HBOOK1(21,'READ INPUT CPU$',600,0.0,600.0,0.)
            CALL HBOOK1(22,'RECONSTRUCTION CPU$',600,0.0,600.0,0.)
            CALL HBOOK1(23,'RESET POINTER CPU$',600,0.0,600.0,0.)
            CALL HBOOK1(24,'WRITE OUTPUT CPU$',600,0.0,600.0,0.)
            CALL HBOOK1(25,'RECONSTRUCT EVENT CPU$',600,0.0,600.0,0.)
            CALL HBOOK2(31,'L2BITS vs RECONSTRUCT EVENT CPU$',
     &        100,0.0,300.0,128,0.0,127.0,0.)
          ENDIF
        ENDIF
      ENDIF
C
C            drop MC banks from input file or previous RECO bank
C
      IF(GEAN) CALL MZDROP(IXCOM,LQ(LHEAD-IZGEAN),' ')
      IF(ISAE) CALL MZDROP(IXCOM,LQ(LHEAD-IZISAE),' ')
      IF(FAKE) CALL MZDROP(IXCOM,LQ(LHEAD-IZFAKE),' ')
      IF (BY_VERSION  ) THEN
        RECO=.FALSE.
        PROC=.FALSE.
        CALL FULL_D0RECO_VERSION(VERSION,PASS)
        IF(VERSION.LT.12) RECO=.TRUE.
        IF(VERSION.GT.11) PROC=.TRUE.
      ENDIF
      IF(RECO) CALL MZDROP(IXCOM,LQ(LHEAD-IZRECO),' ')
      IF(PROC) THEN
        LPROC=GZPROC()
        IF(LPROC.GT.0) CALL MZDROP(IXCOM,LPROC,' ')
      ENDIF
      IF(MUHT) THEN
        LMUHT=GZMUHT()
        IF(LMUHT.GT.0) CALL MZDROP(IXCOM,LMUHT,' ')
      ENDIF
      CALL MKPATH
      CALL HSTRFL
      IF(FIRST) THEN    ! print 1st history bank
        FIRST=.FALSE.
        LHSTR=GZHSTR()
        SUM_UNIT=SSUNIT()
        CALL PRHSTR(SUM_UNIT,LHSTR,0,'ONE',0)
        EVT1 = IQ(LHEAD+9)
        EVTCNT = 0
        TTSUM = 0.0
      ENDIF
      EVTCNT = EVTCNT + 1
      IF (CPU_TIMING) THEN 
        NUMEVT = IQ(LHEAD+9)
        NEVT = NUMEVT - EVT1 + 1
        FNEVT = FLOAT(NEVT)
        BEFORE_RECO =0.0
        CALL TIMED(BEFORE_RECO)
        IF(EVENT_RECO_PBD()) THEN      ! succesfull reconstruction
          AFTER_RECO = 0.0
          CALL TIMED(AFTER_RECO)
          CALL DMPPRO                  ! dump events
          CALL RESET_RECO_POINTERS_PBD ! reset pointers
          DUMP_RESET = 0.0
          CALL TIMED(DUMP_RESET)
          CALL EVTWOS                  ! write event to output streams
          TWRITE = 0.0
          CALL TIMED(TWRITE)
          TSUM = 0.0
          TSUM = BEFORE_RECO + AFTER_RECO + DUMP_RESET + TWRITE
          TTSUM = TTSUM + TSUM
          CALL DHDIR('D0RECO_RCP','HBOOK_DIRECTORY',IER,' ')
          IF (IER .NE. 0) THEN
            CALL ERRMSG('D0RECO','HISTOGRAM',
     &        ' ERROR SETTING HBOOK DIRECTORY ','W')
          ELSE
            CALL HFF1(11,NID(1),BEFORE_RECO,1.)
            CALL HFF1(12,NID(2),AFTER_RECO,1.)
            CALL HFF1(13,NID(3),DUMP_RESET,1.)
            CALL HFF1(14,NID(4),TWRITE,1.)
            CALL HFF1(15,NID(5),TSUM,1.)
            CALL HF1(21,FNEVT,BEFORE_RECO)
            CALL HF1(22,FNEVT,AFTER_RECO)
            CALL HF1(23,FNEVT,DUMP_RESET)
            CALL HF1(24,FNEVT,TWRITE)
            CALL HF1(25,FNEVT,TSUM)
            DO 100 L2BIT = 0, 127
              L2OK = L2BIT_PASSED(L2BIT)
              IF (L2OK) THEN
                CALL HFILL(31,TSUM,FLOAT(L2BIT),1.)
              ENDIF
  100       CONTINUE
          ENDIF
        ENDIF
      ELSE
        IF(EVENT_RECO_PBD()) THEN      ! succesfull reconstruction
          CALL DMPPRO                  ! dump events
          CALL RESET_RECO_POINTERS_PBD ! reset pointers
          CALL EVTWOS                  ! write event to output streams
        ENDIF
      ENDIF
  999 RETURN
C
      ENTRY RECO_TIMING(TOTRECO,TOTEVT,FIRSTEVT)
      TOTRECO = TTSUM
      TOTEVT = EVTCNT
      FIRSTEVT = EVT1
      RETURN
      END

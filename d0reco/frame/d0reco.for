C----------------------------------------------------------------------
C
C                   DDDDDDDD           0000   0
C                   D       D         0    0 0
C                   D        D       0      0
C                   D         D     0      0 0
C                   D         D     0     0  0
C                   D         D     0    0   0
C                   D         D     0   0    0
C                   D         D     0  0     0
C                   D        D       00     0
C                   D       D        00    0
C                   DDDDDDDD        0  0000
C
C
C----------------------------------------------------------------------
C
      PROGRAM D0RECO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-       Main Program for D0 Event Reconstruction
C-
C-   Created   6-SEP-1989   Serban D. Protopopescu
C-   Updated   6-AUG-1994   Qizhong Li-Demarteau  added CPU counting
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER INUNIT
      LOGICAL OK
C
      INTEGER SUM_UNIT, SSUNIT
      INTEGER NFILES, NID(3), TOTEVT, FIRSTEVT
      REAL    TSUM, TINI, TEND, TRUN_INI, TRECO, TTRECO1
      REAL    TTRUN_INI, TARUN_INI, TTRECO, TARECO, TAEVT, TAVERAGE
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
C ****  Init timming measurements
C
      TSUM = 0.0
      CALL TIMED( TSUM )
      TINI = 0.0
      TTRUN_INI = 0.0
      TTRECO = 0.0
      TEND = 0.0
      NFILES = 0
C
      CALL INITIALIZE_D0RECO
      CALL TIMED(TINI)
   1  CALL D0RECO_NEW_FILE(INUNIT,OK)
      IF(OK) THEN
        NFILES = NFILES + 1
        TRUN_INI = 0.0
        CALL TIMED(TRUN_INI)
        TTRUN_INI = TTRUN_INI + TRUN_INI
        CALL START_RECONSTRUCTION(INUNIT)
        TRECO = 0.0
        CALL TIMED(TRECO)
        TTRECO1 = TTRECO1 + TRECO
        GOTO 1
      ENDIF
      CALL RECO_TIMING(TTRECO,TOTEVT,FIRSTEVT)
      CALL END_D0RECO
      CALL TIMED(TEND)
C
      IF (TTRECO .LE. 0) TTRECO = TTRECO1
      TSUM = TINI + TTRUN_INI + TTRECO + TEND
      TARUN_INI = TTRUN_INI / NFILES
      TAEVT = TTRECO / TOTEVT
      TAVERAGE = TSUM / TOTEVT
      SUM_UNIT = SSUNIT()
      WRITE (SUM_UNIT, 1000)
      WRITE (SUM_UNIT, 1001)
      WRITE (SUM_UNIT, 1002) TSUM, TINI, TTRUN_INI, TTRECO, TEND
      WRITE (SUM_UNIT, 1003)
      WRITE (SUM_UNIT, 1004) NFILES, TOTEVT, FIRSTEVT
      WRITE (SUM_UNIT, 1005) 
      WRITE (SUM_UNIT, 1006) TARUN_INI, TAEVT
      WRITE (SUM_UNIT, 1007) 
      WRITE (SUM_UNIT, 1008) TAVERAGE
 1000 FORMAT (/,10X, '******** CPU SUMMARY (in seconds) ********')
 1001 FORMAT (/,2X,
     &     'Total:       SUM      INIT   RUN_INI      RECO       END')
 1002 FORMAT (8X,5F10.2)
 1003 FORMAT (/,2X,
     &     'TOTAL:     NFILES    NEVENTS  FIRST_EVENT_NUMBER')
 1004 FORMAT (8X,2I10,10X,I10)
 1005 FORMAT (/,2X,
     &     'Average:    CPU_RUN_INI/FILE      CPU_RECO/EVENT')
 1006 FORMAT (20X,F10.2,10X,F10.2)
 1007 FORMAT (/,2X,
     &     'Total Average:     Total_CPU/EVENT')
 1008 FORMAT (26X,F10.2)
C
      STOP
      END

      SUBROUTINE FDC_AVE_PH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill average pulse height histograms (one for
C-    each HV power supply).
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  18-SEP-1992   Susan K. Blessing
C-   Updated  22-OCT-1993   Susan K. Blessing  Split sectors 0 and 1 from
C-    sectors 2-5 in PH histograms
C-   Updated  30-NOV-1993   Susan K. Blessing  Use sigma for error rather
C-    than the error on the mean.  For automatic histogram comparison.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER IER,I
      INTEGER ID,ID_TYPE,SUM_ID
      INTEGER HALF,UNIT,QDRT
      INTEGER TYPE
C
      REAL MEAN(0:15),ERR_MEAN(0:15)
      REAL SIGMA(0:15),SUM
      REAL HSTATI
C
C----------------------------------------------------------------------
C
      DO TYPE = 0,4
C
        SUM_ID = 440 + TYPE
        UNIT = 0
C
        IF (TYPE.EQ.0) THEN
C SW0, S0-S1
          ID_TYPE = 30000
        ELSE IF (TYPE.EQ.1) THEN
C SW, S0-S1
          ID_TYPE = 31000
        ELSE IF (TYPE.EQ.2) THEN
C SW0, S2-S5
          ID_TYPE = 33000
        ELSE IF (TYPE.EQ.3) THEN
C SW, S2-S5
          ID_TYPE = 34000
        ELSE IF (TYPE.EQ.4) THEN
C PHI
          ID_TYPE = 36000
          UNIT = 1
        END IF
C
        IER = 0
        CALL DHDIR('FDC_RCP','HBOOK_FDC',IER,' ')
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('FDC','FDC_EXM_END_RUN',
     &      ' ERROR SETTING HBOOK DIRECTORY ','W')
        ENDIF
C
        DO HALF = 0, MXHALF
C
          DO QDRT = 0, MXQUAD
C
            ID = ID_TYPE + 100*HALF + 10*UNIT + QDRT
C
            MEAN(QDRT+8*HALF) = HSTATI(ID,1,'HIST',0)
            SIGMA(QDRT+8*HALF) = HSTATI(ID,2,'HIST',0)
            SUM = HSTATI(ID,3,'HIST',0)
C
            IF (SUM.GT.0) THEN
              ERR_MEAN(QDRT+8*HALF) = SIGMA(QDRT+8*HALF)/SQRT(SUM)
            ELSE
              MEAN(QDRT+8*HALF) = 0.
              ERR_MEAN(QDRT+8*HALF) = 0.
            END IF
C
          END DO
        END DO
C
        IER = 0
        CALL DHDIR('FDC_RCP','HBOOK_CD',IER,' ')
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('FDC','FDC_EXM_END_RUN',
     &        ' ERROR SETTING HBOOK DIRECTORY ','W')
        ENDIF
C
        CALL HPAK(SUM_ID,MEAN)
        CALL HPAKE(SUM_ID,ERR_MEAN)
C        CALL HPAKE(SUM_ID,SIGMA)
C
      END DO
C
  999 RETURN
      END

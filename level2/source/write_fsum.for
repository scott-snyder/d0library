      SUBROUTINE WRITE_FSUM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : write the stored filter summary of one run
C-
C-   Inputs  : /filter_com/, /filter_timing/
C-   Outputs : the file FSUM_runno.DAT
C-   Controls: None
C-
C-   Created  13-APR-1992   James T. Linnemann
C-   Updated  21-JUN-1992   James T. McKinley - Allow logical name
C-                          FSUM_FILE to supersede default file name.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L2_TYPE.DEF'
      INCLUDE 'D0$INC:FILTER_COM.INC'
      INCLUDE 'D0$INC:FILTER_TIMING.INC'
      INCLUDE 'D0$INC:FSUM.INC'
      REAL    X
      INTEGER NRUN,RUNNO,N,IER,LUN,NCHR,LENGTH
      INTEGER I,IDME,ISTAT,TRNLNM
      LOGICAL GOTIT
      CHARACTER*80 FNAME,LOGNAME,LOGTRANS
      DATA LOGNAME/'FSUM_FILE'/
      INTEGER FSUM_USER
      PARAMETER( FSUM_USER = 40909  )
C----------------------------------------------------------------------
C
C     normalize times by doing some computation and timing it
C
        CALL TM_CREATE_TIMER(IDME,1,IGOOD)
        CALL TM_START_TIMER(IDME,IGOOD)
        X = 0.
        DO I=1,100000
          X = SQRT(X + FLOAT(I))
        ENDDO
        CALL TM_STOP_TIMER(IDME,TIME,IGOOD)
        CALL TM_DELETE_TIMER(IDME,IGOOD)
        TIME = .001*TIME
        CALL GTUNIT(FSUM_USER,LUN,IER)
        NRUN = RUNNO()
        CALL STRINT ('FSUM_',NRUN,FNAME,N)
        ISTAT=TRNLNM(LOGNAME,LOGTRANS,LENGTH)
        IF(ISTAT.AND.(LENGTH.GT.0))THEN
          WRITE(FNAME,1000)
          CALL D0OPEN(LUN,FNAME,'OU',GOTIT)
        ELSE
          CALL D0OPEN(LUN,FNAME(1:N)//'.DAT','OU',GOTIT)
        ENDIF
        WRITE(LUN)NRUN,EVENT_WEIGHT,CROSS_SECT,FILTER_COUNT,TOT_EVENTS,
     &    FILTER_SET_COUNT,FILTER_WRITE_COUNT,WRITE_UNFILT_COUNT,
     &    FILT_EVENTS,AVGBITS,FNCALL,FAVG,FSIGMA,
     &    SNCALL,SAVG,SSIGMA,TNCALL,TAVG,TSIGMA,TIME
        CLOSE (LUN)
        CALL RLUNIT(FSUM_USER,LUN,IER)
1000  FORMAT('FSUM_FILE')
  999 RETURN
      END


      FUNCTION ZTR_SUMMARY()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : used for JOB_SUMMARY hook to produce a
C-                         summary output
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  15-NOV-1991   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL ZTR_SUMMARY
      LOGICAL OK1, OK2, VER_SUMMARY, EZERROR, CALL_VERTEX
      INTEGER SSUNIT, LUN, IER
      INTEGER IDVECT(50), NUMHIS, NUMENT, NX, RUNNO
      INTEGER UNDERF, OVERF, I, YDUM, MWOR, LOC
      REAL    MEAN, SIGMA, XLO, XHI, HSTATI, YDUM1, YDUM2, HI
      CHARACTER*80 TITLE
C----------------------------------------------------------------------
      LUN = SSUNIT()       ! this gives you the unit for the standard summary
      WRITE (LUN,1000) RUNNO()
      WRITE (LUN,1001) 
C
      CALL DHDIR('ZTRAKS_RCP','HBOOK_DIRECTORY',IER,' ')
      IF (IER .NE. 0) THEN
        CALL ERRMSG('ZTRAKS','ZTR_SUMMARY',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
      CALL HID1(IDVECT,NUMHIS)
        DO 100 I = 1,NUMHIS
          CALL HNOENT(IDVECT(I),NUMENT)
          MEAN  = HSTATI(IDVECT(I),1,'HIST',1)
          SIGMA = HSTATI(IDVECT(I),2,'HIST',1)
          CALL 
     &      HGIVE(IDVECT(I),TITLE,NX,XLO,XHI,YDUM,YDUM1,YDUM2,MWOR,LOC)
          UNDERF = HI(IDVECT(I),0)
          OVERF  = HI(IDVECT(I),NX+1)
          WRITE (LUN,1002) IDVECT(I), TITLE(1:30), NUMENT, MEAN, SIGMA, 
     &      NX, XLO, XHI, UNDERF, OVERF
  100   CONTINUE
        OK1 = .TRUE.
C
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZTREVT',
     &    'Unable to find bank ZTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('CALL_VERTEX',CALL_VERTEX,IER)
        CALL EZRSET
        IF (CALL_VERTEX) OK2 = VER_SUMMARY()
        ZTR_SUMMARY = OK1 .AND. OK2
C
 1000 FORMAT (///10X,' ZTRAKS Summary (Histogram statistic) for run',
     &  I8,/8X,
     &  '========================================================',//)
 1001 FORMAT (
     &  '   ID     Title',21X,'  Entries     Mean       Sigma      ',
     &  '  Nbin     Low    High Under  Over',/)
 1002 FORMAT (' ',I4,1X,A30,I8,2X,2G12.5,2X,I6,F8.1,F8.1,I6,I6)
C      
  999 RETURN
      END

      SUBROUTINE DBKCHD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Book histograms: <charge> vs drift distance
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: called by DTRDIA
C-             ENTRY point: DTSCHD(HSTCHD)
C-
C-   Created  18-AUG-1988   Qizhong Li-Demarteau
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL YES, HSTCHD, ANSWER
      LOGICAL EZERROR
      INTEGER IER
      INTEGER I, ID, NBINS, J, MAXSEC, ERR
      REAL XMIN, XMAX, YMIN, YMAX
      CHARACTER*80 TITLE
      CHARACTER*45 PROMPT(5)
      CHARACTER*1 TYPARR(5)
      DATA PROMPT/
     &  '  Number of distance bins = 75? (integer)>',
     &            '  Min. distance = -7.5(cm)? (real number)>',
     &            '  Max. distance = 7.5(cm)? (real number)>',
     &            '  Min. charge = 0.0? (real number)>',
     &            '  Max. charge = 3000.0? (real number)>'/
      DATA TYPARR/'I','R','R','R','R'/
C
      DATA NBINS /75/, XMIN /-7.5/, XMAX /7.5/, YMIN /1./, YMAX /3001./
C
C----------------------------------------------------------------------
C
      CALL EZPICK('DTRAKS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('DTRAKS','DBKCHD',
     &    'Unable to find bank DTRAKS_RCP','W')
        GOTO 999
      ENDIF
      CALL EZGET('MAXSEC',MAXSEC,ERR)
      CALL EZRSET
C
      YES=.FALSE.
      CALL GETPAR(1,' Book histograms: <charge> vs drift distance? 
     &  Y/N>','L',YES)
      IF(.NOT.YES) GOTO 999
C
      CALL GETPAR(1,'  Change histogram booking parameters?
     &  Y/N>','L',ANSWER)
      IF (ANSWER) 
     &  CALL GETPAR(5,PROMPT,TYPARR,NBINS,XMIN,XMAX,YMIN,YMAX)
C      DO 200 J = 0, MAXSEC
C        DO 100 I = 0, 27
C          ID = 3000 + I + J * 28
C          WRITE(TITLE,1000) J, I
C 1000     FORMAT(1X,'<charge> VS drift distance (sec ',I2,
C     &    ' wire ',I2,')$')
C          CALL HBPROF(ID,TITLE,NBINS,XMIN,XMAX,YMIN,YMAX,' ') 
C 100    CONTINUE  
C 200  CONTINUE  
        DO 100 I = 0, 27
          ID = 3000 + I 
          WRITE(TITLE,1000) I
 1000     FORMAT(1X,'<charge> VS drift distance (wire ',I2,')$')
          CALL HBPROF(ID,TITLE,NBINS,XMIN,XMAX,YMIN,YMAX,' ') 
 100    CONTINUE  
C
 999  RETURN
C
      ENTRY DTSCHD(HSTCHD)
      HSTCHD = YES
      RETURN
      END


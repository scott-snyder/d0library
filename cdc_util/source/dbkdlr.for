      SUBROUTINE DBKDLR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Book histograms for delay lines: <residuals> vs distance in Z 
C-                                      <resolution> vs distance in Z
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: called by DTRDIA
C-             ENTRY point: DTSDLR(HSTDLR)
C-
C-   Created  18-AUG-1988   Qizhong Li-Demarteau
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL YES, HSTDLR
      LOGICAL EZERROR
      INTEGER IER
      INTEGER I, ID, J, MAXSEC, ERR
      CHARACTER*80 TITLE
      CHARACTER*2 DLWIRN(8)
      DATA DLWIRN /'0-','0+','1-','1+','2-','2+','3-','3+'/
C
C----------------------------------------------------------------------
C
      CALL EZPICK('DTRAKS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('DTRAKS','DBKDLR',
     &    'Unable to find bank DTRAKS_RCP','W')
        GOTO 999
      ENDIF
      CALL EZGET('MAXSEC',MAXSEC,ERR)
      CALL EZRSET
C
      YES=.FALSE.
      CALL GETPAR(1,' Book residuals and resolution histograms 
     &  for DL? Y/N>','L',YES)
      IF(.NOT.YES) GOTO 999
C
C      DO 100 J = 0, MAXSEC
C        DO 200 I = 1,8
C          ID = 1300 + J * 8 + I
C          WRITE(TITLE,1000) J, DLWIRN(I)
C 1000     FORMAT(1X,'residuals VS distance in Z (sec ',I2,
C     &    ' DL ',A2,')$')
C          CALL HBAVER (ID,TITLE,90,-90.0,90.0,-0.5,0.5)
C 200    CONTINUE  
C 100  CONTINUE  
        DO 200 I = 1,8
          ID = 1300 +  I
          WRITE(TITLE,1000) DLWIRN(I)
 1000     FORMAT(1X,'residuals VS distance in Z (DL ', A2,')$')
         CALL HBOOK2(ID,TITLE,90,-90.0,90.0,50,-1.0,1.0,0.)
 200    CONTINUE  
C
 999  RETURN
C---------------------------------------------------------------------------
      ENTRY DTSDLR(HSTDLR)
      HSTDLR = YES
      RETURN
      END

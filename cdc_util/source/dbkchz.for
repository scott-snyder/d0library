      SUBROUTINE DBKCHZ
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Book histograms: <DL charge>/<SW charge> VS distance in Z
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: called by DTRDIA
C-             ENTRY point: DTSCHZ(HSTCHZ) 
C-
C-   Created  18-AUG-1988   Qizhong Li-Demarteau
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL YES, HSTCHZ
      LOGICAL EZERROR
      INTEGER IER
      INTEGER I, ID1, ID2, J, MAXSEC, ERR
      CHARACTER*80 TITLE1, TITLE2
C
C----------------------------------------------------------------------
C
      CALL EZPICK('DTRAKS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('DTRAKS','DBKCHZ',
     &    'Unable to find bank DTRAKS_RCP','W')
        GOTO 999
      ENDIF
      CALL EZGET('MAXSEC',MAXSEC,ERR)
      CALL EZRSET
C
      YES=.FALSE.
      CALL GETPAR(1,' Book histograms: <DL charge/SW_o charge> vs Z ? 
     &  Y/N>','L',YES)
      IF(.NOT.YES) GOTO 999
C
      DO 100 J = 0, MAXSEC
        DO 200 I = 1,8
          ID1 = 4000 + J * 8 + I
          WRITE(TITLE1,1001) J, I
 1001     FORMAT(1X,'<DL- charge/SW_o charge> VS distance in Z (sec ',
     &      I2, ' wire ',I2,')$')
          CALL HBAVER(ID1,TITLE1,90,-90.,90.,0.,5.) 
          ID2 = 4300 + J * 8 + I
          WRITE(TITLE2,1002) J, I
 1002     FORMAT(1X,'<DL+ charge/SW_o charge> VS distance in Z (sec ',
     &      I2, ' wire ',I2,')$')
          CALL HBAVER(ID2,TITLE2,90,-90.,90.,0.,5.) 
 200    CONTINUE  
 100  CONTINUE  
C
 999  RETURN
C---------------------------------------------------------------------------
      ENTRY DTSCHZ(HSTCHZ)
      HSTCHZ = YES
      RETURN
      END

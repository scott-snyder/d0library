      SUBROUTINE DTRDRP(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : read in CDC banks and drop
C-
C-   Inputs  : 
C-   Outputs : 
C-            IER = error return, succesful=0, failure=1
C-   Controls: 
C-
C-   Created  28-JUN-1989   Qizhong Li-Demarteau
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
      INTEGER MAXDRP
      PARAMETER( MAXDRP = 20 )
      CHARACTER*4 DROPS(MAXDRP)
      INTEGER BANKS(MAXDRP)
      INTEGER I,NUMDRP
      LOGICAL EZERROR
C----------------------------------------------------------------------
C
      IER=0
      CALL EZPICK('DTRAKS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('DTRAKS','DTRDRP',
     &    'Unable to find bank DTRAKS_RCP','W')
        GOTO 999
      ENDIF
C
C ****  Tell which banks must be dropped from standard output (STA)
C
      CALL EZGET('NUM_DROPS_STA',NUMDRP,IER)   ! get number of banks
C
      IF (NUMDRP.LE.0) GOTO 999
       IF(NUMDRP.LE.MAXDRP)THEN
         CALL EZGET('DROPS_STA_BANKS',BANKS,IER) ! get list of banks
       ELSE
         CALL ERRMSG('DTRAKS','DTRDRP',
     &      'DROP REQUEST EXCEEDS MAXIMUM ALLOWED ','W')
         IER=1
         CALL EZRSET
         GOTO 999                 ! failed
       ENDIF
C
      CALL EZRSET
C
      DO 100 I = 1,NUMDRP
        CALL UHTOC(BANKS(I),4,DROPS(I),4)
C
        CALL EVDROP('STA',DROPS(I))       ! add bank to the drop list
C                                         ! for Standard output
  100 CONTINUE
  999 RETURN
      END

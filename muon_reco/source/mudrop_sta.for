      SUBROUTINE MUDROP_STA(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Add muon banks to STA drop list
C-
C-   Inputs  : 
C-   Outputs : 
C-            IER = error return, succesful=0, failure=1
C-   Controls: 
C-
C-   Created  12-APR-1994   M. Fortner from CDC_UTIL:DTRDRP
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
      CALL EZPICK('MURECO_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('MURECO','MUDROP_STA',
     &    'Unable to find bank MURECO_RCP','W')
        GOTO 999
      ENDIF
C
C ****  Tell which banks must be dropped from standard output (STA)
C
      CALL EZGET_i('NUM_DROPS_STA',NUMDRP,IER)   ! get number of banks
C
      IF (NUMDRP.LE.0) GOTO 999
       IF(NUMDRP.LE.MAXDRP)THEN
         CALL EZGET_iarr('DROPS_STA_BANKS',BANKS,IER) ! get list of banks
       ELSE
         CALL ERRMSG('MURECO','MUDROP_STA',
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

      SUBROUTINE CALDRP(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Read in calorimeter banks to drop
C-
C-   Output:
C-    IER = error return, succesful=0, failure=1
C-
C-   Created  22-MAY-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
      INTEGER MAXDRP
      PARAMETER( MAXDRP = 20 )
      CHARACTER*4 DROPS(MAXDRP)
      INTEGER BANKS(MAXDRP)
      INTEGER I,NUMDRP
C----------------------------------------------------------------------
C
      IER=0
      CALL EZPICK('CALEVT_RCP')
C
C ****  Tell which banks must be dropped from standard output (STA)
C
      CALL EZGETA('DROP_STA_BANKS',0,0,0,NUMDRP,IER)   ! get number of banks
C
      IF(NUMDRP.LE.MAXDRP)THEN
        CALL EZGET('DROP_STA_BANKS',BANKS,IER) ! get list of banks
      ELSE
        CALL ERRMSG('CALORIMETER','CALDRP',
     &      'DROP REQUEST EXCEEDS MAXIMUM ALLOWED ','W')
        IER=1
        CALL EZRSET
        GOTO 999                 ! failed
      ENDIF
C
      DO 100 I = 1,NUMDRP
        CALL UHTOC(BANKS(I),4,DROPS(I),4)
C
        CALL EVDROP('STA',DROPS(I))       ! add bank to the drop list
C                                         ! for Standard output
  100 CONTINUE
C
C ****  Tell which banks must be dropped from DST output
C
      CALL EZGETA('DROP_DST_BANKS',0,0,0,NUMDRP,IER)   ! get number of banks
C
      IF(NUMDRP.LE.MAXDRP)THEN
        CALL EZGET('DROP_DST_BANKS',BANKS,IER) ! get list of banks
      ELSE
        CALL ERRMSG('CALORIMETER','CALDRP',
     &      'DROP REQUEST EXCEEDS MAXIMUM ALLOWED ','W')
        IER=1
        CALL EZRSET
        GOTO 999                 ! failed
      ENDIF
C
      DO 200 I = 1,NUMDRP
        CALL UHTOC(BANKS(I),4,DROPS(I),4)
C
        CALL EVDROP('DST',DROPS(I))       ! add bank to the drop list
C                                         ! for DST output
  200 CONTINUE
C
      CALL EZRSET
  999 RETURN
      END

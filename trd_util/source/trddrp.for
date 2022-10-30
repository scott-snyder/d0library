      SUBROUTINE TRDDRP(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Read in TRD banks to drop
C-
C-   Output:
C-    IER = error return, succesful=0, failure=1
C-
C-   Daria Zieminska 
C-   Updated  17-NOV-1989   J.Fr. Glicenstein  Error corrected in 
C-                          dropping routine name  
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
      CALL EZPICK('TRD_RCP')
C
C ****  Tell which banks must be dropped from standard output (STA)
C
      CALL EZGET('NUM_DROPS_STA',NUMDRP,IER)   ! get number of banks
C
      IF(NUMDRP.LE.MAXDRP)THEN
        CALL EZGET_iarr('DROPS_STA_BANKS',BANKS,IER) ! get list of banks
      ELSE
        CALL ERRMSG('TRD','TRDDRP',
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
      CALL EZGET('NUM_DROPS_DST',NUMDRP,IER)   ! get number of banks
C
      IF(NUMDRP.LE.MAXDRP)THEN
        CALL EZGET_iarr('DROPS_DST_BANKS',BANKS,IER) ! get list of banks
      ELSE
        CALL ERRMSG('TRD','TRDDRP',
     &      'DROP REQUEST EXCEEDS MAXIMUM ALLOWED ','W')
        IER=1
        CALL EZRSET
        GOTO 999                 ! failed
      ENDIF
C
      CALL EZRSET
C
      DO 200 I = 1,NUMDRP
        CALL UHTOC(BANKS(I),4,DROPS(I),4)
C
        CALL EVDROP('DST',DROPS(I))       ! add bank to the drop list
C                                         ! for DST output
  200 CONTINUE
  999 RETURN
      END


      SUBROUTINE FTRDRP(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Read in FDC banks to drop
C-
C-   Output:
C-    IER = error return, succesful=0, failure=1
C-
C-   Created  17-OCT-1990   Daria Zieminska (based on CALDRP)
C-   Updated  29-APR-1991   Jeffrey Bantly  use EZERROR 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IER
      INTEGER MAXDRP
      PARAMETER( MAXDRP = 20 )
      INTEGER BANKS(MAXDRP)
      INTEGER I,NUMDRP
C
      CHARACTER*4 DROPS(MAXDRP)
C
      LOGICAL EZERROR
      EXTERNAL EZERROR
C----------------------------------------------------------------------
C
      IER=0
      CALL EZPICK('FTRAKS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('FTRAKS','FTRDRP','FTRAKS_RCP not found.','W')
      ELSE
C
C ****  Tell which banks must be dropped from standard output (STA)
C
        CALL EZGET('NUM_DROPS_STA',NUMDRP,IER)   ! get number of banks
C
        IF (NUMDRP.GT.0) THEN
          IF(NUMDRP.LE.MAXDRP)THEN
            CALL EZGET('DROPS_STA_BANKS',BANKS,IER) ! get list of banks
          ELSE
            CALL ERRMSG('FTRAKS','FTRDRP',
     &      'DROP REQUEST EXCEEDS MAXIMUM ALLOWED ','W')
            IER=1
            CALL EZRSET
            GOTO 999                 ! failed
          ENDIF
        END IF
C
        CALL EZRSET
      ENDIF
C
      DO 100 I = 1,NUMDRP
        CALL UHTOC(BANKS(I),4,DROPS(I),4)
C
        CALL EVDROP('STA',DROPS(I))       ! add bank to the drop list
C                                         ! for Standard output
  100 CONTINUE
C-----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END

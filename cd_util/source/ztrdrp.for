      SUBROUTINE ZTRDRP(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Read in banks to drop
C-
C-   Output:
C-    IER = error return, succesful=0, failure=1
C-
C-   Daria Zieminska (based on CALDRP)
C-   Updated  14-DEC-1993   Qizhong Li-Demarteau   increased the array size
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
      INTEGER MAXDRP
      PARAMETER( MAXDRP = 30 )
      CHARACTER*4 DROPS(MAXDRP)
      INTEGER BANKS(MAXDRP)
      INTEGER I,NUMDRP
C----------------------------------------------------------------------
C
      IER=0
      CALL EZPICK('ZTRAKS_RCP')
C
C ****  Tell which banks must be dropped from standard output (STA)
C
      CALL EZGET('NUM_DROPS_STA',NUMDRP,IER)   ! get number of banks
C
      IF (NUMDRP.GT.0) THEN
       IF(NUMDRP.LE.MAXDRP)THEN
         CALL EZGET('DROPS_STA_BANKS',BANKS,IER) ! get list of banks
       ELSE
         CALL ERRMSG('ZTRAKS','ZTRDRP',
     &      'DROP REQUEST EXCEEDS MAXIMUM ALLOWED ','W')
         IER=1
         CALL EZRSET
         GOTO 999                 ! failed
       ENDIF
      END IF
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

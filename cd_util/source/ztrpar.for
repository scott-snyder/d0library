      FUNCTION ZTRPAR()
C----------------------------------------------------------------------
C
C  Run initialization for the ZTRAKS package:
C  Initialize parameters for central detector
C
C  Returns .TRUE. if run is to be processed (.FALSE. if skipped)
C
C  Daria Zieminska Feb. 1989
C-   Updated  20-MAR-1991   Qizhong Li-Demarteau  added switches for
C-                                                subdetectors
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZERROR
C-   Updated  13-AUG-1991   Susan K. Blessing  Move LOGICAL to declarations.
C-    Add ().
C-   Updated  12-FEB-1994   Ed Oltman  Remove call to VTRPAR since access needed
C-                                     to /ZEBCOM/ 
C
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL ZTRPAR
      LOGICAL DTRPAR,FTRPAR,TRDPAR
      LOGICAL CDCON, FDCON, TRDON, OK
      LOGICAL EZERROR
C
      INTEGER IER
C----------------------------------------------------------------------
      ZTRPAR=.FALSE.
C
      CALL EZPICK('ZTRAKS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('ZTRAKS','ZTRPAR',
     &    'Unable to find bank ZTRAKS_RCP','W')
        GOTO 999
      ENDIF
C
      CALL EZGET('CDCON',CDCON,IER)
      CALL EZGET('FDCON',FDCON,IER)
      CALL EZGET('TRDON',TRDON,IER)
      CALL EZRSET
C
      OK = .TRUE.
C
      IF (CDCON) OK = DTRPAR().AND.OK
      IF (FDCON) OK = FTRPAR().AND.OK
      IF (TRDON) OK = TRDPAR().AND.OK
C
      ZTRPAR = OK
C
  999 RETURN
      END

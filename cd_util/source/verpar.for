      LOGICAL FUNCTION VERPAR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : run initialization for VERTEX package
C-
C-   Inputs  : 
C-   Outputs : .TRUE. if run is to be processed (.FALSE. if skipped) 
C-   Controls: 
C-
C-   Created  12-OCT-1988   Daria Zieminska
C-   Updated  12-APR-1990   Qizhong Li-Demarteau  added a call to CDINIT
C-   Updated  19-SEP-1990   Qizhong Li-Demarteau  rewritten 
C-   Updated  22-MAR-1991   Qizhong Li-Demarteau  added switches for
C-                                                subdetectors 
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZERROR
C-   Updated  25-FEB-1994   Liang-Ping Chen move CALL VTRPAR to
C-                          VTX_DYNADJ, similar to ZTRPAR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FTRPAR
      LOGICAL CDCON, FDCON
      LOGICAL EZERROR
      INTEGER IER
C----------------------------------------------------------------------
C
      VERPAR = .FALSE.
      CALL EZPICK('ZTRAKS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('ZTRAKS','VERPAR',
     &    'Unable to find bank ZTRAKS_RCP','W')
        GOTO 999
      ENDIF
      CALL EZGET('CDCON',CDCON,IER)
      CALL EZGET('FDCON',FDCON,IER)
      CALL EZRSET
      IF (CDCON) CALL CDINIT
      IF (FDCON) VERPAR = FTRPAR()
      VERPAR = .TRUE.
C
  999 RETURN
      END

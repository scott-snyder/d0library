      FUNCTION PDINIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INIT Interface Routine for PIXIE package
C-                         CDCDIS
C-
C-   Read RCP file PX_CDCDIS.RCP into memory.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  21-SEP-1990   PXBUILD V1.00
C-   Updated  17-SEP-1991   Qizhong Li-Demarteau  added reading DTRAKS_RCP
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PDINIT
C----------------------------------------------------------------------
      INTEGER IER, LRCP
      CHARACTER*(*) RCPFILE, RCPFILD
      PARAMETER (RCPFILE = 'PX_CDCDIS_RCP')
      PARAMETER (RCPFILD = 'DTRAKS_RCP')
      LOGICAL OK, FIRST
      SAVE OK, FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL INRCP (RCPFILE,IER)
        OK = IER .EQ. 0
C
C ****  Initialize menu
C
        CALL EZ_SETUP_COMPACK(RCPFILE,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG
     &      ('PIXIE','PDINIT','Problem accessing PX_CDCDIS_RCP','F')
        ENDIF
C
C  read DTRAKS_RCP if needed
C
        CALL EZLOC(RCPFILD,LRCP)
        IF (LRCP.LE.0) THEN
          CALL INRCP(RCPFILD,IER)
          OK = OK .AND. (IER.EQ.0)
          IF (IER .NE. 0) CALL ERRMSG('INRCP ERROR','PDINIT',
     &    'Problem accessing DTRAKS_RCP','F')
        ENDIF
      ENDIF
      CALL CDISTP('CDC_STPFILE',IER)
      OK = OK .AND. (IER.EQ.0)
      PDINIT = OK
  999 RETURN
      END


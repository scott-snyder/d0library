      FUNCTION PSINIT ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INIT Interface Routine for PIXIE package
C-                         SAMDIS
C-
C-   Read RCP file PX_SAMDIS.RCP into memory.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  12-JUN-1991 S. Hagopian
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PSINIT
C----------------------------------------------------------------------
      INTEGER IER
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_SAMDIS_RCP' )
      LOGICAL EZERROR
C----------------------------------------------------------------------
      LOGICAL OK, FIRST
      CHARACTER*1 CVAL, REM
      INTEGER TYP
      DATA FIRST/.TRUE./
      SAVE OK, FIRST
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL INRCP (RCPFILE,IER)
C
C ****  Initialize menu
C
        CALL EZ_SETUP_COMPACK(RCPFILE,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG
     &      ('PIXIE','PSINIT','Problem accessing PX_SAMDIS_RCP','F')
        ENDIF
        OK = IER .EQ. 0
      ENDIF
C
C ****  Read in Muon Geometry file
C
        CALL SAISTP('d0$stp:SAM_D0STPFILE.DAT',IER)
        OK = OK .AND. (IER.EQ.0)
        IF(IER.EQ.0)THEN
          CALL INTMSG(' SAM_STPFILE read in OK')
        ELSE
          CALL INTMSG(' SAM_STPFILE read ERROR')
        ENDIF
C
C
C ****  Reseting RCP file
C
  900   CONTINUE
        CALL EZRSET
C
      PSINIT = OK
  999 RETURN
      END

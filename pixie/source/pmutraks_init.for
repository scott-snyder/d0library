      FUNCTION PMUTRAKS_INIT ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INIT Interface Routine for PIXIE package
C-                         MUTRAKSDIS
C-
C-   Read RCP file PX_MUTRAKSDIS.RCP into memory.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  17-JAN-1992   PXBUILD V1.00
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PMUTRAKS_INIT
C----------------------------------------------------------------------
      INTEGER IER
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_MUTRAKSDIS_RCP' )
C----------------------------------------------------------------------
      LOGICAL OK, FIRST
      DATA FIRST/.TRUE./
      SAVE OK, FIRST
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL INRCP (RCPFILE,IER)
        OK = IER .EQ. 0
        IF ( .NOT. OK ) THEN
          CALL ERRMSG
     &  ('PIXIE','INRCP','Unable to open PX_MUTRAKSDIS_RCP','F')
        ENDIF
C
C ****  Initialize menu
C
        CALL EZ_SETUP_COMPACK(RCPFILE,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG
     &  ('PIXIE','EZ_SETUP_COMPACK','No PX_MUTRAKSDIS_RCP bank','F')
        ENDIF
      ENDIF
      PMUTRAKS_INIT = OK
  999 RETURN
      END

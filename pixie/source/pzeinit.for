      FUNCTION PZEINIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INIT Interface Routine for PIXIE package
C-                         CDC ELECTRONICS
C-
C-   Read RCP file PX_CD_ELECTRONICSDIS.RCP into memory.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  14-AUG-1991   Lupe Howell   
C-   Updated  25-JUN-1992   Robert E. Avery   Book "SET SCALE" flag.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PZEINIT
C----------------------------------------------------------------------
      INTEGER IER,PFFLAGS
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_CD_ELECTRONICSDIS_RCP' )
C----------------------------------------------------------------------
      LOGICAL OK, FIRST
      DATA FIRST/.TRUE./
      SAVE OK, FIRST
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
     &   ('PIXIE','PZEINIT',
     &    'Problem accessing PX_CD_ELECTRONICSDIS_RCP','F')
        ENDIF
        OK = OK .AND. (IER.EQ.0)
C
C ****  Book flags
C
        IER = PFFLAGS()
        OK = OK .AND. (IER .EQ. 0)
      ENDIF
      PZEINIT = OK
  999 RETURN
      END

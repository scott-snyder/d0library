      FUNCTION PCOM3DIS_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read into memory the RCP file PX_COM3DIS_RCP.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   23-JUN-1992   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PCOM3DIS_INIT
C
      INTEGER IER
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_COM3DIS_RCP' )   ! Logical name of control file
C
      LOGICAL OK,FIRST
      DATA FIRST/.TRUE./
      SAVE OK,FIRST
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
        CALL INRCP (RCPFILE,IER)! Read parameter file into an SRCP bank
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG
     &    ('PIXIE','PCOM3DIS_INIT','Problem reading PX_COM3DIS_RCP','F')
        ENDIF
C
C ****  Build up the menus
C
        CALL EZ_SETUP_COMPACK(RCPFILE,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG
     &  ('PIXIE','PCOM3DIS_INIT','Problem accessing PX_COM3DIS_RCP','F')
        ENDIF
        OK = IER .EQ. 0
      ENDIF
      PCOM3DIS_INIT = OK
  999 RETURN
      END

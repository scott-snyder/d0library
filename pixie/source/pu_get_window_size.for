      SUBROUTINE PU_GET_WINDOW_SIZE(IOPT,XMIN,XMAX,YMIN,YMAX,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the boundary coordinates of the currently
C-   active screen. Get the information from the current RCP-bank.
C-
C-   Inputs  : IOPT     [I]     1 - window coordinates
C-                              2 - virtual coordinates
C-
C-   Outputs : XMIN     [R]     Boundary coordinates
C-             XMAX
C-             YMIN
C-             YMAX
C-             IER      [I]     0 - ok
C-   Controls:
C-
C-   Created  16-MAY-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IOPT
      REAL    XMIN,XMAX,YMIN,YMAX
      INTEGER IER
C----------------------------------------------------------------------
      INTEGER ISCREEN,NSCREEN,IDX
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
C
C ****  Get Screen Number for active screen
C
      CALL PU_GET_ACTIVE_SCREEN(COMMAND)
      CALL PU_GET_SCREEN_NUMBER(COMMAND,ISCREEN,NSCREEN,IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('BAD_SCREEN','PU_GET_WINDOW_SIZE',
     &    'Error accessing screen '//COMMAND,'W')
        GOTO 999
      ENDIF
C
C ****  Get port and window values from PXSCREEN array
C
      CALL PU_GOTO_SCREEN(ISCREEN,IDX)
      IF     ( IOPT .EQ. 1 ) THEN
        CALL PU_GET_SCREEN_PARAM(IDX,'WINDOWXMIN',XMIN,IER)
        CALL PU_GET_SCREEN_PARAM(IDX,'WINDOWXMAX',XMAX,IER)
        CALL PU_GET_SCREEN_PARAM(IDX,'WINDOWYMIN',YMIN,IER)
        CALL PU_GET_SCREEN_PARAM(IDX,'WINDOWYMAX',YMAX,IER)
      ELSEIF ( IOPT .EQ. 2 ) THEN
        CALL PU_GET_SCREEN_PARAM(IDX,'VPORTXMIN',XMIN,IER)
        CALL PU_GET_SCREEN_PARAM(IDX,'VPORTXMAX',XMAX,IER)
        CALL PU_GET_SCREEN_PARAM(IDX,'VPORTYMIN',YMIN,IER)
        CALL PU_GET_SCREEN_PARAM(IDX,'VPORTYMAX',YMAX,IER)
      ELSE
        IER =-1
      ENDIF
C
  999 RETURN
      END

      FUNCTION P0INIT ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INIT Interface Routine for PIXIE package
C-                         LV0DIS
C-
C-   Read RCP file PX_LV0DIS.RCP into memory, and initialize LV0 geometry.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  29-JUN-1992   Jeffrey Bantly   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL P0INIT
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER IER,RUNTYPE
      INTEGER LRCP
      INTEGER IMODE
C
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_LV0DIS_RCP' )
C
      LOGICAL OK, OKF, FIRST
C
C Functions:
      LOGICAL LV0INI
      LOGICAL EZERROR
C
      SAVE OK, FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  Read in LV0DIS Pixie RCP file
C
        CALL INRCP (RCPFILE,IER)
C
C ****  Build COMPACK menus
C
        CALL EZ_SETUP_COMPACK(RCPFILE,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG('PIXIE-l0px-no-rcp','P0INIT',
     &                  'Problem accessing PX_LV0DIS_RCP','F')
        ENDIF
        OK = IER .EQ. 0
C
C ****  Fetch STP file name from LEVEL0.RCP
C
        CALL EZLOC('LEVEL0_RCP',LRCP)                        
        IF(LRCP.LE.0) THEN                                   
          OKF=LV0INI()
          IF(.NOT. OKF) THEN
            CALL ERRMSG('PIXIE-l0-no-rcp','P0INIT',
     &                             'LEVEL0_RCP not available','F')
            OK = .FALSE.
            GOTO 990
          ENDIF
        ENDIF
C
      ENDIF
C
  990 CONTINUE
      P0INIT = OK
C----------------------------------------------------------------------------
  999 RETURN
      END

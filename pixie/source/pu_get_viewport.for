      SUBROUTINE PU_GET_VIEWPORT(PACKAGE,COMMAND,XMIN,XMAX,YMIN,YMAX,
     &  VPORTXMIN,VPORTXMAX,VPORTYMIN,VPORTYMAX,PACK,COMM,IDDX,NPORT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a screen command return the viewports
C-   which contain the specified box. The point should be given in
C-   screen virtual coordinates. The command can be a combined one.
C-
C-   Inputs  : PACKAGE  [C*]    Current Package
C-             COMMAND  [C*]    Command
C-             XMIN     [R]     Boundary of box
C-             XMAX
C-             YMIN
C-             YMAX
C-
C-   Outputs : VPORTXMIN(*) [R] Viewport boundary
C-             VPORTXMAX(*)
C-             VPORTYMIN(*)
C-             VPORTYMAX(*)
C-             PACK(*)  [C*]    Package containing viewport (screen)
C-             COMM(*)  [C*]    Command associated with viewport (screen)
C-             IDDX(*)  [I]     Index of command
C-             NPORT    [I]     Number of viewports containing box
C-
C-   Created   5-DEC-1990   Harrison B. Prosper
C-   Updated  29-JAN-1991   Lupe Howell
C-   Updated  26-FEB-1991   Lupe Howell  Updating PX_GET_NEXT_ACTION call
C-   Updated   7-MAY-1991   Harrison B. Prosper
C-      Changed arguments; tidy up
C-   Updated  10-JUL-1992   Lupe Howell  Check if a system command in the 
C-      combined display, if so skip checking for viewport selection
C-   Updated  22-OCT-1992   Lupe Howell  Updating call to PX_GET_NEXT_ACTION 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PACKAGE
      CHARACTER*(*) COMMAND
      REAL    XMIN,XMAX,YMIN,YMAX
      REAL    VPORTXMIN(*)
      REAL    VPORTXMAX(*)
      REAL    VPORTYMIN(*)
      REAL    VPORTYMAX(*)
      CHARACTER*(*) PACK(*)
      CHARACTER*(*) COMM(*)
      INTEGER IDDX(*)
      INTEGER NPORT
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
C----------------------------------------------------------------------
      INTEGER ITYPE,PARAM_TYPE(MAXCPARAM),NUMBER_PARAMS,IDX,IER
      INTEGER I,J,K,L,II,JJ,KK,LCOMB,PTR,NUMBER_SCREENS
      INTEGER PARAM_VALUE(MAXCPARAM),ACTION_NUMBER
C
      REAL    VPRTXMIN,VPRTXMAX,VPRTYMIN,VPRTYMAX
C
      CHARACTER*32 CURRENT_PACKAGE(MAXCPARAM),PARAM(MAXCPARAM)
      CHARACTER*32 ACTION_COMMAND(MAXCPARAM),ACTION_CALLS(10)
      CHARACTER*40 REM
C
      LOGICAL WITHIN,EZERROR,ACTIVE,PU_SET_RCP_BANK
      LOGICAL PX_CHECK_SYSTEM_COMMAND
C----------------------------------------------------------------------
C
C ****  Check command type
C
      L = LEN(COMMAND)
      IF ( INDEX(COMMAND(1:L),'%') .GT. 0 ) THEN
C
C ****  This is a combined view command.
C
        CALL SWORDS(COMMAND(1:L),I,J,LCOMB)
        NUMBER_SCREENS = 0
        PTR    = 1                        ! Go to start of array
        ACTIVE = .TRUE.
        DO WHILE ( ACTIVE )
          NUMBER_SCREENS = NUMBER_SCREENS + 1
          CALL PX_GET_NEXT_ACTION(COMMAND(1:LCOMB),
     &                            CURRENT_PACKAGE(NUMBER_SCREENS),
     &                            ACTION_CALLS,
     &                            ACTION_NUMBER,
     &                            PARAM,
     &                            PARAM_VALUE,
     &                            PARAM_TYPE,
     &                            NUMBER_PARAMS,
     &                            REM,
     &                            PTR,IER)
          ACTION_COMMAND(NUMBER_SCREENS) = ACTION_CALLS(ACTION_NUMBER)
          ACTIVE = IER .EQ. 0             ! Exit if this is last action
        ENDDO
      ELSE
        NUMBER_SCREENS = 1
        ACTION_COMMAND(1)  = COMMAND(1:LEN(COMMAND))
        CURRENT_PACKAGE(1) = PACKAGE(1:LEN(PACKAGE))
      ENDIF
C
C ****  Loop over possible viewports
C
      NPORT = 0
      DO I = 1 , NUMBER_SCREENS
C
C ****  Pick RCP bank
C
        IF ( PU_SET_RCP_BANK(CURRENT_PACKAGE(I)) ) THEN
C
C ****  Get pointer into screen array
C ****  If the action command is a system command
C ****  don't try to get viewport boundaries
C
          IF ( .NOT. PX_CHECK_SYSTEM_COMMAND(ACTION_COMMAND(I)) ) THEN
            CALL PU_GET_SCREEN_INDEX(ACTION_COMMAND(I),IDX,IER)
     &
            IF ( IER .EQ. 0 ) THEN
C
C ****  Check if area is within viewport boundary
C
              CALL PU_GET_SCREEN_PARAM
     &            (IDX,'VPORTXMIN',VPRTXMIN,IER)
              WITHIN = .FALSE.
              IF ( XMIN .GE. VPRTXMIN ) THEN
                CALL PU_GET_SCREEN_PARAM
     &              (IDX,'VPORTXMAX',VPRTXMAX,IER)
                IF ( XMAX .LE. VPRTXMAX ) THEN
                  CALL PU_GET_SCREEN_PARAM
     &                (IDX,'VPORTYMIN',VPRTYMIN,IER)
                  IF ( YMIN .GE. VPRTYMIN ) THEN
                    CALL PU_GET_SCREEN_PARAM
     &                  (IDX,'VPORTYMAX',VPRTYMAX,IER)
                    IF ( YMAX .LE. VPRTYMAX ) THEN
                      WITHIN = .TRUE.
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
C **** If the coordinates are within a screen return viewport
C **** parameters.
C
              IF ( WITHIN ) THEN
                NPORT = NPORT + 1
                PACK(NPORT)      = CURRENT_PACKAGE(I)
                COMM(NPORT)      = ACTION_COMMAND(I)    ! Note command
                IDDX(NPORT)      = IDX
                VPORTXMIN(NPORT) = VPRTXMIN
                VPORTXMAX(NPORT) = VPRTXMAX
                VPORTYMIN(NPORT) = VPRTYMIN
                VPORTYMAX(NPORT) = VPRTYMAX
              ENDIF
            ENDIF
          ENDIF
C
C ****  Reset RCP bank
C
          CALL PU_RESET_RCP_BANK
        ENDIF
      ENDDO
C
      IF ( NPORT .LE. 0 ) THEN
        CALL ERRMSG('BAD_POINTS','PU_GET_VIEWPORT',
     &    ' Points outside viewport(s)','W')
      ENDIF
  999 RETURN
      END

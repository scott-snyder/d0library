      SUBROUTINE PU_GET_VIEWPORT2(CURRENT_PACKAGE,COMMAND,XMIN,XMAX,
     &  YMIN,YMAX,VPORTXMIN,VPORTXMAX,VPORTYMIN,VPORTYMAX,PACK,COMM,
     &  IDDX,NPORT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a screen command return the viewports
C-   contained by the specified box. The point should be given in
C-   screen virtual coordinates. The command can be a combined one.
C-
C-   Inputs  : CURRENT_PACKAGE  [C*]    Current Package
C-             COMMAND  [C*]    Command
C-             XMIN     [R]     XMIN Boundary of box
C-             XMAX     [R]     XMAX Boundary of box
C-             YMIN     [R]     YMIN Boundary of box
C-             YMAX     [R]     YMAX Boundary of box
C-
C-   Outputs : VPORTXMIN(*) [R] XMIN Viewport boundary
C-             VPORTXMAX(*) [R] XMAX Viewport boundary
C-             VPORTYMIN(*) [R] YMIN Viewport boundary
C-             VPORTYMAX(*) [R] YMAX Viewport boundary
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
C-   Updated  19-JUN-1991   Lupe Howell  tidy up 
C-   Updated  19-FEB-1992   Lupe Howell  Use COMMAND through a temporary var to
C-      solve SGI problems
C-   Updated  22-OCT-1992   Lupe Howell  Updateing PX_GET_NEXT_ACTION call 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) CURRENT_PACKAGE
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
      INTEGER ITYPE,PARAM_TYPE(MAXSAVE_P),NUMBER_PARAMS,IDX(NBMENU),IER
      INTEGER I,J,K,L,II,JJ,KK,LCOMB,PTR,NUMBER_SCREENS
      INTEGER PARAM_VALUE(MAXSAVE_P),ACTION_NUMBER,VALTYPE
C
      REAL    VPRTXMIN,VPRTXMAX,VPRTYMIN,VPRTYMAX
C
      CHARACTER*32 COMBINED_PACKAGE(NBMENU),PARAM(MAXSAVE_P),ARRAY_NAME
      CHARACTER*32 ACTION_COMMAND(NBMENU),ACTION_CALLS(MAXCACT)
      CHARACTER*80 COMM_STRING
      CHARACTER*40 REM
C
      LOGICAL WITHIN,EZERROR,ACTIVE,PU_SET_RCP_BANK
C----------------------------------------------------------------------
C
C ****  Check command type
C
      COMM_STRING = ' '
      COMM_STRING = COMMAND
      L = LEN(COMM_STRING)
      IF ( INDEX(COMM_STRING(1:L),'%') .GT. 0 ) THEN
C
C ****  This is a combined view command.
C
        CALL SWORDS(COMM_STRING(1:L),I,J,LCOMB)
        ARRAY_NAME = COMM_STRING(1:LCOMB)
        NUMBER_SCREENS = 0
        PTR    = 1                        ! Go to start of array
        ACTIVE = .TRUE.
        DO WHILE ( ACTIVE )
          NUMBER_SCREENS = NUMBER_SCREENS + 1
          IDX(NUMBER_SCREENS)= PTR
          CALL PX_GET_NEXT_ACTION(COMM_STRING(1:LCOMB),
     &                            COMBINED_PACKAGE(NUMBER_SCREENS),
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
        ARRAY_NAME = 'PXSCREEN'
        ACTION_COMMAND(1)  = COMM_STRING(1:L)
        CALL PU_GET_SCREEN_INDEX(ACTION_COMMAND(1),IDX(1),IER)
        COMBINED_PACKAGE(1) = CURRENT_PACKAGE(1:LEN(CURRENT_PACKAGE))
      ENDIF
C
C ****  Loop over possible viewports
C
      NPORT = 0
      DO I = 1 , NUMBER_SCREENS
C
C ****  Pick RCP bank using the current PACKAGE the viewport values
C ****  in the combined array are the values we need to get
C
        IF ( PU_SET_RCP_BANK(CURRENT_PACKAGE) ) THEN
C
C ****  Check if area is within viewport boundary
C
          CALL EZ_GET_ELEMENT(ARRAY_NAME,
     &            'VPORTXMIN',IDX(I),1,VPRTXMIN,VALTYPE,IER)
          WITHIN = .FALSE.
          IF ( XMIN .LE. VPRTXMIN ) THEN
            CALL EZ_GET_ELEMENT(ARRAY_NAME,
     &              'VPORTXMAX',IDX(I),1,VPRTXMAX,VALTYPE,IER)
            IF ( XMAX .GE. VPRTXMAX ) THEN
              CALL EZ_GET_ELEMENT(ARRAY_NAME,
     &                'VPORTYMIN',IDX(I),1,VPRTYMIN,VALTYPE,IER)
              IF ( YMIN .LE. VPRTYMIN ) THEN
                CALL EZ_GET_ELEMENT(ARRAY_NAME,
     &                  'VPORTYMAX',IDX(I),1,VPRTYMAX,VALTYPE,IER)
                IF ( YMAX .GE. VPRTYMAX ) THEN
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
            PACK(NPORT)      = COMBINED_PACKAGE(I)
            COMM(NPORT)      = ACTION_COMMAND(I)    ! Note command
            IDDX(NPORT)      = IDX(I)
            VPORTXMIN(NPORT) = VPRTXMIN
            VPORTXMAX(NPORT) = VPRTXMAX
            VPORTYMIN(NPORT) = VPRTYMIN
            VPORTYMAX(NPORT) = VPRTYMAX
          ENDIF
CC          ENDIF
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

      SUBROUTINE PU_CHECK_MOD_OPTIONS(OPTION,REM_OPTION,ADJUSTCOM,
     &     NUM_OPTION)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Checks the current package for existance of 
C-   PXPARAM, PXSCREENS, and Combined views to build the options for
C-   modify parameters in this package.
C-
C-   Inputs  : None
C-             
C-   Outputs : OPTION    [C*(*)]: Array with the list of options available
C-             REM_OPTION[C*(*)]: Array with the remarks of the list
C-             ADJUSTCOM     [I]: Adjustment that modifies the menu display.
C-             NUM_OPTION    [I]: Total number of options set
C-
C-   Created  16-MAR-1992   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) OPTION(*)
      CHARACTER*(*) REM_OPTION(*)
      INTEGER ADJUSTCOM
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
      INCLUDE 'D0$INC:PXCOMK.INC'
C----------------------------------------------------------------------
      CHARACTER*16 ALL_OPTIONS(NOPTION)
      CHARACTER*80 ALL_REM(NOPTION)
      CHARACTER*40 CURR_RCP,LAST_RCP,ELEMENT_FOUND
      INTEGER N,ID,IER,J,I,NUM_OPTION,NEXT
      LOGICAL PXSCREEN,PXPARAM,MOD_COMB,COMBINED
C----------------------------------------------------------------------
      DATA LAST_RCP /' '/
      DATA ALL_OPTIONS/'PARAMETERS',
     &            'SCREENS',
     &            'COMBINED VIEWS',
     &            'SELECT PACKAGE',
     &            'RESTORE PACKAGE'/
      DATA ALL_REM/
     &        ' Modify General Parameters in current package',
     &        ' Modify Screen Parameters in current package',
     &        ' Modify paramters in Combined/Multiple views',
     &        ' Select a package to modify its paramters',
     &        ' Restores the parameters to their original values'/
C----------------------------------------------------------------------
      SAVE LAST_RCP,PXSCREEN,PXPARAM
C----------------------------------------------------------------------
C
C ****  Getting the name of the current rcp and check it with
C ****  last one.  If the current RCp is not equal to the old one
C ****  setup the options availabel to the user and adjust
C ****  the command idex if necessary
C
      CALL EZTELL(CURR_RCP,N)
      IF ( CURR_RCP .NE. LAST_RCP ) THEN
C
C ****  Check if PXPARAMS/SCREEN arrays in the current RCP file
C
        PXPARAM  = .FALSE.
        PXSCREEN = .FALSE.
        CALL EZGETI(PARNAME,ID,IER)
        IF( IER .EQ. 0 ) THEN
          PXPARAM  = .TRUE.
        ENDIF
        CALL EZGETI(SCRENAME,ID,IER)
        IF( IER .EQ. 0 ) THEN
          PXSCREEN = .TRUE.
        ENDIF
        NEXT = 0
        COMBINED = .FALSE.
        DO WHILE ( NEXT .NE. -1 )
          CALL EZGET_NEXT_NAME(ELEMENT_FOUND,NEXT)
          CALL WORD(ELEMENT_FOUND,I,J,N)
          IF ( ELEMENT_FOUND(J:J) .EQ. '%' ) THEN
            COMBINED = .TRUE.
            NEXT = -1
          ENDIF
        ENDDO
C
C ****  Filling up the option menu.  If the array PXSCREEN
C ****  is not in the current RCP file leave SCREENS out
C ****  of the options.  If PXPARAMS is not in the current
C ****  RCP file leave PARAMETERS out.
C
        J = 0
        I = 0
   05   DO WHILE ( I .LT. NOPTION )
          I = I + 1
          IF ( I .EQ. 1 ) THEN
            IF ( .NOT. PXPARAM ) THEN
              GOTO 05
            ENDIF
          ELSEIF ( I .EQ. 2 ) THEN
            IF ( .NOT. PXSCREEN ) THEN
              GOTO 05
            ENDIF
          ELSEIF ( I .EQ. 3 ) THEN
            IF ( .NOT. COMBINED ) THEN
              GOTO 05
            ENDIF
          ENDIF
          J = J + 1
          OPTION(J) = ALL_OPTIONS(I)
          REM_OPTION(J) = ALL_REM(I)
        ENDDO
        NUM_OPTION = J
        DO WHILE ( J .LT. NOPTION )
          J = J + 1
          OPTION(J) = ' '
        ENDDO
C
C ****  Calculating the command menu index if necessary.
C ****  If there is no PXPARAMS or PXSCREEN in the current
C ****  RCP subtract 1 form the all menu otpions to exclude
C ****  those items.
C
        ADJUSTCOM = 0
        IF ( ( .NOT. PXPARAM ) .AND. (.NOT. PXSCREEN ) )THEN
          ADJUSTCOM = 2
        ELSEIF( ( PXPARAM ) .AND. (.NOT. PXSCREEN ) ) THEN
          ADJUSTCOM = -1
        ENDIF

        LAST_RCP = CURR_RCP
      ENDIF
  999 RETURN
      END

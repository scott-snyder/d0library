      SUBROUTINE DISPLAYMOD(LAST_COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display Parameters to be modified and allow
C-   the user to modify them.
C-
C-   Inputs  : LAST_COMMAND[C*]: Last command executed by the menu system
C-                               before modify was called.
C-   Outputs : None
C-
C-   Created  18-MAY-1990   Lupe Howell
C-   Updated   3-OCT-1990   Lupe Howell  Checking for error reading the
C-      parameter array.
C-   Updated  15-MAR-1991   LUPE HOWELL
C-      Added PXMOD_SCREEN and PXMOD_COMBINED_VIEWS
C-   Updated  20-MAR-1991   Harrison B. Prosper
C-      Put in a check on PARATOT
C-   Updated  29-MAR-1991   Lupe Howell  The use of array names and index to
C-      set parameters
C-   Updated   9-OCT-1991   Lupe Howell LAST_COMMAND added to skip first
C-   modify menu to modify last combined command entered
C-   Updated   7-JAN-1992   Lupe Howell  Modify call to PX_DISPLAY_ITEMS
C-   Updated  17-MAR-1992   Lupe Howell  Modify PX_SYSTEM fixed
C-   Updated   2-NOV-1992   Lupe Howell  Make call to PX_DISPLAY_ITEMS 
C-   Updated   8-JUL-1993   Lupe Howell  Display the SYSTEM package when modify
C-          a combined view 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) LAST_COMMAND
      INCLUDE 'D0$INC:PXCOMK.INC'
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
C----------------------------------------------------------------------
      CHARACTER*40 PARA_NAME(NBMENU ), PARA_REM( NBMENU ),
     &  PARA_CVAL( NBMENU ),ARRAY_NAME,CURR_RCP,ORIGINAL_RCP,
     &  SAVE_RCP
      CHARACTER*40 COMBINE_ARRAY
      CHARACTER*1  CPARATY(NBMENU)
      CHARACTER*16 OPTION(NOPTION),ALL_OPTIONS(NOPTION)
      CHARACTER*80 REM_OPTION(NOPTION),STRING,MESS
      CHARACTER*32 COMMAND
C
      LOGICAL MODFLGS( NBMENU ),MODIFY,FLGVAL,COMBINED,MODIFY_VIEW
      LOGICAL MOD_COMB, REAL_COMB
C
      INTEGER PARA_VAL(NBMENU), OLD_PARA_VAL(NBMENU),ICOM,ID
      INTEGER IER, PARATY(NBMENU), PARATOT, I,IDX,J,K,N,ADJUSTCOM
      INTEGER NUM_OPTION,II
C----------------------------------------------------------------------
      DATA PARA_NAME/50*' '/
      DATA PARA_CVAL/50*' '/
      DATA PARA_REM/50*' '/
      DATA MODFLGS/50*.FALSE./
      DATA MODIFY /.FALSE./
      DATA OPTION/NOPTION * ' '/
      DATA ALL_OPTIONS/'PARAMETERS',
     &            'SCREENS',
     &            'COMBINED VIEWS',
     &            'SELECT PACKAGE',
     &            'RESTORE PACKAGE'/
C----------------------------------------------------------------------
      COMBINE_ARRAY = ' '
      IER = 0
      ICOM = 1
      MODIFY_VIEW = .FALSE.
      COMBINED = .FALSE.
      MOD_COMB = .FALSE.
      CALL EZTELL(ORIGINAL_RCP,N)
      SAVE_RCP = ' '
C-
C--- Check real combined RCP or not...
C-
      IF (ORIGINAL_RCP .EQ. 'PX_COMBDIS_RCP' .OR.
     &    ORIGINAL_RCP .EQ. 'PX_MUTRAKSDIS_RCP' .OR.
     &    ORIGINAL_RCP .EQ. 'PX_ZTRAKSDIS_RCP') THEN
        REAL_COMB = .TRUE.
      ELSE
        REAL_COMB = .FALSE.
      ENDIF
C---
C-
C
C ****  Check the last command. If the last command is a blank it was
C ****  a system command.   Otherwise check if the command was a
C ****  combined or not.
C
      CALL SWORDS(LAST_COMMAND,I,J,K)
      IF( K .GT. 0 ) THEN
        IF ( LAST_COMMAND(K:K) .EQ. '%' .AND. REAL_COMB ) THEN
          MODIFY_VIEW = .TRUE.
          COMBINED = .TRUE.
        ENDIF
      ENDIF
C
C ****  MODIFY menu display.
C
      DO WHILE ( ICOM .GT. 0 )
        CALL EZTELL(CURR_RCP,N)
        CALL STAMSG(' ',.TRUE.)
C
C ****  If a combined view was modified earlier set RCP
C ****  bank to its original
C
        IF ( MOD_COMB ) THEN
          MOD_COMB = .FALSE.
          CALL PXCOMB_RESET_MOD
          IF ( SAVE_RCP .NE. CURR_RCP ) THEN
            CALL PX_SET_PACKAGE(SAVE_RCP)
            CALL EZTELL(CURR_RCP,N)
            SAVE_RCP = ' '
          ENDIF
        ENDIF
C
C ****  Check the options that the modify menu will
C ****  have available acording to the active RCP file
C
        CALL PU_CHECK_MOD_OPTIONS
     &    (OPTION,REM_OPTION,ADJUSTCOM,NUM_OPTION)
C
C ****  If a combined view was executed as the
C ****  last command the MODIFY menu will be skipped
C ****  to execute COMBINED VIEW submenu
C
        IF ( MODIFY_VIEW ) THEN
          ICOM = 3
        ELSE
          STRING = 'MODIFY PARAMETERS FOR '//CURR_RCP(1:N)
          CALL SWORDS(STRING,I,J,K)
          CALL DISPLAY_ITEMS
     &      (NUM_OPTION,OPTION,REM_OPTION,STRING(I:J),ICOM)
          IF ( ADJUSTCOM .LT. 0 ) THEN ! Adjusting special case
            IF ( ICOM .GE. 2 )
     &        ICOM = ICOM + 2            ! PXPARAMS with NOT PXSCREEN
          ELSEIF ( ICOM .GT. 0 ) THEN  ! Adjust the index picked
            ICOM = ICOM + ADJUSTCOM
          ENDIF
        ENDIF
C
   10   CONTINUE
        IF ( ICOM .GT. 0 ) THEN

          MODIFY = .FALSE.
          IF (  ALL_OPTIONS(ICOM) .EQ. 'PARAMETERS' ) THEN
C
C ****  Getting PXPARAMs parameters from RCP array
C ****  If the parameter requested comes from a combined view get the
C ****  parameter values from the combined view itself to update the list
C ****  with the values that have in there
C
            STRING = ' MODIFY PARAMETERS FOR '//CURR_RCP(1:N)
            CALL STAMSG(STRING,.TRUE.)
            PARA_NAME(1) = ' '
            CALL EZ_GET_ARRAY(PARNAME,PARA_NAME,PARATOT,PARA_VAL,
     &         PARA_CVAL,PARATY,PARA_REM,IER)
            IF( MOD_COMB ) THEN
              CALL EZPICK(CURR_RCP)
              CALL  PXGET_COMBINED_PARAM(COMBINE_ARRAY,PARA_NAME,
     &          PARA_VAL,PARATOT,II)
              CALL EZRSET
            ENDIF
            IF( ( IER .NE. 0 ) .AND. ( II .NE. 0 ) )THEN
              CALL STAMSG(' No PXPARAMS in RCP file',.TRUE.)
            ELSE
              MODIFY = .TRUE.
              ARRAY_NAME = PARNAME
              IDX = 1
            ENDIF
C
C ****  SCREENS parameters
C
          ELSEIF ( ALL_OPTIONS(ICOM) .EQ. 'SCREENS' ) THEN
            MESS = ' MODIFY SCREEN PARAMETERS'
            CALL STAMSG(MESS,.TRUE.)
            CALL PXMOD_SCREEN(CURR_RCP,PARA_NAME,PARA_VAL,
     &             PARATY,PARA_REM,PARATOT,IDX,IER)
            IF ( IER .EQ. -1 )THEN
              CALL STAMSG(' No PXSCREEN in RCP file',.TRUE.)
            ELSE
              MODIFY = .TRUE.
              ARRAY_NAME = SCRENAME
            ENDIF
C
C ****  COMBINED VIEWS paramters
C
          ELSEIF ( ALL_OPTIONS(ICOM) .EQ. 'COMBINED VIEWS' ) THEN
            SAVE_RCP = CURR_RCP
            CALL PXMOD_COMBINED_VIEWS
     &        (LAST_COMMAND(1:K),MODIFY_VIEW,ICOM,COMBINE_ARRAY,IER)
            IF ( IER .EQ. -1 )THEN
              CALL STAMSG(' Error accessing combined-views',.TRUE.)
              GOTO 999
            ELSEIF ( IER .EQ. 0 ) THEN
              MOD_COMB = .TRUE.
              GOTO 10
            ENDIF
C
C ****  SELECT PACKAGE
C
          ELSEIF ( ALL_OPTIONS(ICOM) .EQ. 'SELECT PACKAGE' ) THEN
            CALL PX_RESET_SELECT_PACKAGE !Reset to previous bank if one was sel
            CALL PX_SELECT_PACKAGE(' ',0,I)
            MODIFY = .FALSE.
C
C ****  RESTORE PACKAGE
C
          ELSEIF ( ALL_OPTIONS(ICOM) .EQ. 'RESTORE PACKAGE' ) THEN
            CALL PX_RESTORE_PARAMS
            MODIFY = .FALSE.
          ENDIF
C
C ****  Setting up the values in a string array
C
          IF ( MODIFY .AND. (PARATOT .GE. 1) ) THEN
            DO I = 1, PARATOT
              CALL PXGTVAL(PARA_VAL(I),PARATY(I),PARA_CVAL(I))
            ENDDO
C
C ****  Display the parameters to be modified
C
            CALL MODIFY_PARAMS(PARATOT,PARA_NAME,PARA_CVAL,PARA_REM,
     &        MODFLGS)
C
C ****  Check if there were any changes; if so, update the
C ****  RCP array.
C
            DO I = 1,  PARATOT
              IF ( MODFLGS(I) ) THEN
C
C ****  If a combined view is being modify try to modify
C ****  any changes in the combined array.  If the combined array
C ****  does not have that parameter  modify it in its RCP file
c
                IER = -1
                IF ( MOD_COMB ) THEN
                  CALL EZPICK(CURR_RCP)
                  CALL PXMOD_COMBINED_ARRAY(COMBINE_ARRAY,PARA_NAME(I),
     &              PARA_CVAL(I),PARATY(I),IER)
                  CALL EZRSET
                ENDIF
                IF( IER .NE. 0 ) THEN
                  CALL PXRDVAL(PARA_NAME(I),PARA_CVAL(I),PARATY(I),
     &              ARRAY_NAME,IDX,IER)
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDDO
C
C ****  Reset to previous bank if one was one selected
C
      CALL PX_RESET_SELECT_PACKAGE
  999 RETURN
      END

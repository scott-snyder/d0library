      SUBROUTINE PX_DISPLAY_SCREEN
     &  (COMBINED,OUTNUM,SCREENAME,NSCREEN,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays the available screens for the user to
C-   choose.  If COMBINED flag is ON the screens that will be displayed 
C-   will be the ones available in the combined views chosen.
C-   If there is only one screen on this list the list will not shown 
C-   and the default screen will be the only one present.
C-
C-   Inputs  : COMBINED   [L]: Flag that determins to display
C-   
C-   Outputs : OUTNUM    [I ]: Number of the screen selected
C-             SCREENAME [C*]: Name of the screen selected
C-             NSCREEN   [I ]: Total number of screens found
C-             IER       [I ]: Error flag
C-
C-      ENTRY PXGET_IDX_SCREEN(MODIDX)
C-      Returns the index corresponding to the screen that is being modified
C-      from a combined view.
C-
C-   Created  12-MAR-1991   LUPE HOWELL
C-   Updated  14-OCT-1991   Lupe Howell   
C-   Updated   7-JAN-1992   Lupe Howell  Modify call to PX_DISPLAY_ITEMS
C-   Updated   2-NOV-1992   Lupe Howell  Add  DISPLAY_ITEMS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
C
      LOGICAL COMBINED
      INTEGER OPTNUM(MAXSCREN),OUTNUM,NSCREEN,IER
      CHARACTER*(*) SCREENAME
     
      INTEGER SCRETY(MAXSCREN),ITYPE(MAXSCREN),ACTNUM
      INTEGER NUM,IVAL(MAXSCREN),I,NACTIONS
C
      CHARACTER*32 SCRENAM(MAXSCREN),REM(MAXSCREN),SCREREM(MAXSCREN)
      CHARACTER*32 OPTION(MAXSCREN),ACTIONS(MAXSCREN)
      CHARACTER*32 CVAL(MAXSCREN)
      CHARACTER*40 REMARK,COMB_REM(MAXSCREN)
C
      LOGICAL ACTIVE
      SAVE ACTNUM
C----------------------------------------------------------------------
      INTEGER MODIDX
C----------------------------------------------------------------------
      ACTNUM = 0
      OUTNUM = 0
      IER = 0
      SCREENAME = ' '
C
C ****  If modify combined view  get the list of action views in
C ****  the combined array and their remarks
C
      IF( COMBINED ) THEN
        CALL PXGET_COMBINED_ACTION(NACTIONS,ACTIONS)
        I = 0
        DO WHILE ( I .LT. NACTIONS ) 
          I = I + 1
          CALL EZ_GET_ARRAY ! Getting remarks of combined views
     &    (ACTIONS(I),'%TITLE',1,NUM,CVAL,ITYPE,REMARK,IER)
          COMB_REM(I) = REMARK
        ENDDO
      ENDIF
C
C ****  Getting the number of screens in the package array
C
      NUM = 1
      CALL EZ_GET_ARRAY('PXSCREEN','NSCREEN',NUM,NSCREEN,CVAL,
     &              ITYPE,REM,IER)       ! Getting number of screens
      IF ( IER .NE. 0 ) THEN
        CALL INTMSG(' Problem accessing PXSCREEN')
        GOTO 999
      ENDIF
C
C ****  Getting the screen's names in PXSCREEN
C
      CALL EZ_GET_ARRAY('PXSCREEN','NAME',NSCREEN,IVAL,SCRENAM,
     &              SCRETY,SCREREM,IER) ! Getting screen names
      IF ( IER .NE. 0 ) THEN
        CALL INTMSG(' Problem accessing PXSCREEN')
        GOTO 999
      ENDIF
C
C ****  If combined check if there was more that one action
C ****  and if it was let the user select from them
C
      IF ( COMBINED ) THEN
        IF ( NACTIONS .GT. 1 ) THEN
          CALL DISPLAY_ITEMS
     &      (NACTIONS,ACTIONS,COMB_REM,'MODIFY SCREENS',ACTNUM)
        ELSE
          ACTNUM = 1
        ENDIF
        IF( ACTNUM .EQ. 0 ) GOTO 999
C
C ****  Search for the position in the RCP file of the
C ****  action requested
C
        I = 0
        ACTIVE = .TRUE.
        DO WHILE( ACTIVE )
          I = I + 1
          IF( ACTIONS(ACTNUM) .EQ. SCRENAM(I) ) THEN
            OUTNUM = I
            ACTIVE = .FALSE.
            SCREENAME = ACTIONS(ACTNUM)
          ENDIF
          ACTIVE = ACTIVE  .AND. ( I .LT. NSCREEN )
        ENDDO
C
C ****  If not in a combined view display all
C ****  available screens to get option from the user
C
      ELSE
        CALL DISPLAY_ITEMS
     &    (NSCREEN,SCRENAM,SCREREM,'MODIFY SCREENS',OUTNUM)
        SCREENAME = SCRENAM(OUTNUM)
      ENDIF

  999 RETURN
C#######################################################################
      ENTRY PXGET_IDX_SCREEN(MODIDX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the index corresponding to the screen that is
C-   being modified.
C-
C-   Inputs  : None
C-   Outputs : MODIDX [I]: Index of the modified screen
C-
C-   Created  21-OCT-1991   Lupe Howell
C-
C----------------------------------------------------------------------
      MODIDX = ACTNUM
      RETURN
      END

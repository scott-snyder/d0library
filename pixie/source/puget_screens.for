      SUBROUTINE PUGET_SCREENS(SCREEN_NAMES,TOTAL_SCREENS,OUTNUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays all the screens of the current RCP
C-   and letting the user to pick one
C-
C-   Inputs  : None
C-
C-   Outputs : SCREEN_NAMES [C*(*)]: Names of the screens
C-             TOTAL_SCREENS   [I ]: Total number of screens in RCP file
C-             OUTNUM          [I ]: Number of the screen selected
C-
C-   Created  10-MAY-1991   Lupe Howell
C-   Updated  19-JUN-1991   Lupe Howell  Tidy up
C-   Updated   7-JAN-1992   Lupe Howell  Modify call to PX_DISPLAY_ITEMS
C-   Updated   2-NOV-1992   Lupe Howell  Add call to DISPLAY_ITEMS 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) SCREEN_NAMES(*)
      INTEGER TOTAL_SCREENS,OUTNUM
C
      INTEGER I,J,K,LEN,NEXT,IER,TYPES(40),TYPE,NSCREEN,IVAL(40),TEMP
C
      CHARACTER*32 FOUND_ARRAYS,SCRENAM(40)
      CHARACTER*1  CVAL
      CHARACTER*32 REM(40),REMARK,TEMP_SCREEN(40)
C----------------------------------------------------------------------
      CALL OUTMSG('1')
      TOTAL_SCREENS = 0
      NEXT = 0
      J = 1
C
C ****  GET Screens in PXSCREEN array
C
      CALL EZ_GET_ARRAY('PXSCREEN','NSCREEN',J,NSCREEN,CVAL,
     &              TYPE,REMARK,IER)     ! Getting number of screens
      IF ( IER .EQ. 0 ) THEN
        CALL EZ_GET_ARRAY('PXSCREEN','NAME',NSCREEN,IVAL,SCRENAM,
     &              TYPES,REM,IER)       ! Getting screen names
        IF ( IER .EQ. 0 ) THEN
          DO K = 1, NSCREEN
            CALL SWORDS(SCRENAM(K),I,J,LEN)
            TOTAL_SCREENS = TOTAL_SCREENS + 1
            TEMP_SCREEN(TOTAL_SCREENS)  = SCRENAM(K)(1:LEN)
            SCREEN_NAMES(TOTAL_SCREENS) = SCRENAM(K)(1:LEN)
          ENDDO
        ENDIF
      ENDIF
C
C ****  Get Combined Screens
C
      TEMP = TOTAL_SCREENS
      IER = 0
      DO WHILE ( NEXT .NE. -1 )
        CALL EZGET_NEXT_NAME(FOUND_ARRAYS,NEXT)
        CALL WORD(FOUND_ARRAYS,I,J,LEN)
        IF ( FOUND_ARRAYS(J:J) .EQ. '%' ) THEN
          TOTAL_SCREENS = TOTAL_SCREENS + 1
          SCREEN_NAMES(TOTAL_SCREENS) = FOUND_ARRAYS(I:J)
          TEMP_SCREEN(TOTAL_SCREENS)  = FOUND_ARRAYS(I:J-1)
        ENDIF
      ENDDO
C
C ****  Set the remarks for the combined arrays if any
C
      DO WHILE ( TEMP .LT. TOTAL_SCREENS )
        TEMP = TEMP + 1
        CALL EZ_GET_ARRAY ! Getting remarks of combined views
     &    (SCREEN_NAMES(TEMP),'%TITLE',1,IVAL,CVAL,TYPES,REMARK,IER)
        REM(TEMP) = REMARK
      ENDDO
C
C ****  Get option from screens
C
      CALL DISPLAY_ITEMS
     &  (TOTAL_SCREENS,TEMP_SCREEN,REM,'SCREENS',OUTNUM)
  999 RETURN
      END

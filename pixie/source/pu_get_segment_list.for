      SUBROUTINE PU_GET_SEGMENT_LIST(PACKAGE,COMMAND,SEGMENTS,NSEGS,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the retained segments associated with
C-   the given (PACKAGE,COMMAND). The segment list is updated in PUOPEN
C-   via the call to PU_UPDATE_SEGMENT_LIST.
C-
C-   Inputs  : PACKAGE          [C*]    Package Name
C-             COMMAND          [C*]    Command name (screen)
C-   Outputs : SEGMENTS(*)      [I]     Segment list
C-             NSEGS            [I]     Number of segments
C-             IER              [I]     0  --- OK
C-                                      -1 --- Given (Package,Command)
C-                                      is NOT active.
C-   Controls: None
C-
C-   Created  18-DEC-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PACKAGE
      CHARACTER*(*) COMMAND
      INTEGER SEGMENTS(*)
      INTEGER NSEGS
      INTEGER IER
      INTEGER CURRENT_SEGMENT
C----------------------------------------------------------------------
      INTEGER MAX_SEGS,MAX_VIEWS
      PARAMETER( MAX_SEGS  = 10 )
      PARAMETER( MAX_VIEWS = 16 )
      CHARACTER*40 ACTIVE_PACKAGE,ACTIVE_COMMAND
      CHARACTER*80 VIEW_LIST(MAX_VIEWS),LABEL
      INTEGER SEGMENT_LIST(MAX_SEGS,MAX_VIEWS)
      INTEGER SEGMENT_COUNT(MAX_VIEWS)
      INTEGER VIEW_COUNT,IVIEW
      INTEGER I,J,K
      SAVE VIEW_LIST,SEGMENT_LIST,SEGMENT_COUNT
      SAVE VIEW_COUNT
C----------------------------------------------------------------------
      IER   = 0
      NSEGS = 0
C
C ****  If number of active views is zero then just quit
C
      IF ( VIEW_COUNT .LE. 0 ) THEN
        IER = -1
        GOTO 999
      ENDIF
C
C ****  Create unique view label
C
      CALL WORD(PACKAGE(1:LEN(PACKAGE)),I,J,K)
      LABEL = PACKAGE(I:J)//COMMAND(1:LEN(COMMAND))
C
C ****  Find segments for view. Do sequential search for now
C
      IVIEW = 0
      DO I =  1, VIEW_COUNT
        IF ( LABEL .EQ. VIEW_LIST(I) ) THEN
          IVIEW = I
          GOTO 100
        ENDIF
      ENDDO
      IF ( IVIEW .LE. 0 ) THEN
        IER = -2
        GOTO 999
      ENDIF

  100 CONTINUE
      NSEGS = SEGMENT_COUNT(IVIEW)
      DO I =  1, NSEGS
        SEGMENTS(I) = SEGMENT_LIST(I,IVIEW)
      ENDDO
      RETURN
C
      ENTRY PU_UPDATE_SEGMENT_LIST(CURRENT_SEGMENT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Add the current segment number to the
C-   currently active screen. Find the package and command which
C-   corresponds to the active screen.
C-
C-   Inputs  : CURRENT_SEGMENT  [I]     Current retained segment number
C-   Outputs : None
C-   Controls:
C-
C-   Created  18-DEC-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
C
C ****  If the current segment is 1 then set counts to zero
C
      IF ( CURRENT_SEGMENT .LE. 1 ) THEN
        VIEW_COUNT = 0
        DO I =  1,MAX_VIEWS
          SEGMENT_COUNT(I) = 0
        ENDDO
      ENDIF
C
C ****  Get active package and active command
C
      CALL PU_ACTIVE_PACKAGE(ACTIVE_PACKAGE)
      CALL PU_GET_ACTIVE_COMMAND(ACTIVE_COMMAND)
C
C ****  Create unique view label
C
      CALL WORD(ACTIVE_PACKAGE,I,J,K)
      LABEL = ACTIVE_PACKAGE(I:J)//ACTIVE_COMMAND
C
C ****  Find pointer to package/command. Do sequential search for now
C
      IVIEW = 0
      IF ( VIEW_COUNT .GT. 0 ) THEN
        DO I =  1, VIEW_COUNT
          IF ( LABEL .EQ. VIEW_LIST(I) ) THEN
            IVIEW = I
            GOTO 200
          ENDIF
        ENDDO
      ENDIF
C
C ****  This is a new package/command so add to view_list
C
      IF ( VIEW_COUNT .LT. MAX_VIEWS ) THEN
        VIEW_COUNT = VIEW_COUNT + 1
        VIEW_LIST(VIEW_COUNT) = LABEL
        IVIEW = VIEW_COUNT
      ELSE
        CALL ERRMSG('PIXIE','PU_UPDATE_SEGMENT_LIST',
     &    'Too many viewports active','W')
        GOTO 999
      ENDIF
C
C ****  Add current segment to current package/command
C
  200 CONTINUE
C
      IF ( SEGMENT_COUNT(IVIEW) .LT. MAX_SEGS ) THEN
        SEGMENT_COUNT(IVIEW) = SEGMENT_COUNT(IVIEW) + 1
        SEGMENT_LIST(SEGMENT_COUNT(IVIEW),IVIEW) = CURRENT_SEGMENT
      ELSE
        CALL ERRMSG('PIXIE','PU_UPDATE_SEGMENT_LIST',
     &    'Too many active segments for command '//ACTIVE_COMMAND,'W')
        GOTO 999
      ENDIF
C
  999 RETURN
      END

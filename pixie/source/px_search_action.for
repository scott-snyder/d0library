      SUBROUTINE PX_SEARCH_ACTION(NEW_NAME,ACTION_MENU,ACTION_SUBM,
     &  ACTION_NUM,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Search for a given action routine name and 
C-   returns the corresponding menu, submenu and action idexes
C-
C-   Inputs  : NEW_NAME   [C*]: Action routine name
C-   
C-   Outputs : ACTION_MENU [I]: Menu index
C-             ACTION_SUBM [I]: Submenu index
C-             ACTION_NUM  [I]: Action index
C-             IER         [I]: 0 If found
C-
C-   Created  18-JUN-1991   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NEW_NAME
      INTEGER ACTION_MENU,ACTION_SUBM,ACTION_NUM,IER
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXBUILDCOM.INC'
C----------------------------------------------------------------------
      INTEGER IMENU,ISUBM,IACTION,S,E,J
      LOGICAL ACTIVE,FOUND,ACTION
C----------------------------------------------------------------------
      ACTION = .TRUE.
      GOTO 10
C
      ENTRY PX_SEARCH_VIEW(NEW_NAME,ACTION_MENU,ACTION_SUBM,
     &  ACTION_NUM,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Search for a given command view returning 
C-   the indeces.
C-
C-   Created  11-JUL-1991   Lupe Howell
C-
C----------------------------------------------------------------------
      ACTION = .FALSE.
   10 CONTINUE
      IER = 0
      IMENU = 0
      FOUND = .FALSE.
      ACTIVE = .TRUE.
C
C ****  Searching the menus and submenu
C
      DO WHILE ( ACTIVE )
        IMENU = IMENU + 1
        ISUBM = 0
        DO WHILE ( (ISUBM .LE. SUBMENU_COUNT(IMENU)) .AND.
     &           (.NOT. FOUND) )
          ISUBM = ISUBM + 1
          IACTION = 1
          DO WHILE ( (IACTION .LE. ACTION_COUNT(IMENU,ISUBM))
     &      .AND.  ( .NOT. FOUND ))
            CALL SWORDS(NEW_NAME,S,E,J)
C
C ****  Check name with action name common block 
C
            IF ( ACTION ) THEN
              IF ( ACTION_NAME(IMENU,ISUBM,IACTION)(S:E) .EQ.
     &           NEW_NAME(S:E) ) THEN
                FOUND = .TRUE.
              ELSE
                IACTION = IACTION + 1
              ENDIF
            ELSE
C
C ****  Check name with action command common block
C
              IF ( ACTION_COMMAND(IMENU,ISUBM,IACTION)(S:E) .EQ.
     &           NEW_NAME(S:E) ) THEN
                FOUND = .TRUE.
              ELSE
                IACTION = IACTION + 1
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        ACTIVE = .NOT. ( FOUND .OR. (IMENU .GT. MENU_COUNT ) )
      ENDDO
C
C ****  If found return the index
C
      IF ( FOUND ) THEN
        ACTION_MENU = IMENU
        ACTION_SUBM = ISUBM
        ACTION_NUM  = IACTION
      ELSE
        IER = -1
      ENDIF
  999 RETURN
      END

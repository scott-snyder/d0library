      SUBROUTINE PXBUILD_INDEX(RCPFILE,CURRENT_ACTION,MENUIDX,SMENUIDX,
     &  ACTION_IDX,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a RCP file name and an Action Name returs
C-   the corresponding index of the menu, submenu and action.
C-
C-   Inputs  : RCPFIL        [C*]: Name of the RCP file
C-             CURRENT_ACTION[C*]: Name of the action requested
C-
C-   Outputs : MENUIDX       [I ]: Menu index corresponding to the request
C-             SMENUIDX      [I ]: Sub menu index
C-             ACTION_IDX    [I ]: Action index
C-
C-   Control : IER           [I ]: If the given RCP file name and action
C-                                 do not match IER = -1 otherwise =0
C-
C-   Created  28-MAY-1991   LUPE HOWELL
C-   Updated  19-JUN-1991   Lupe Howell  Tidy up 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCPFILE
      CHARACTER*(*) CURRENT_ACTION
      INTEGER MENUIDX,SMENUIDX,ACTION_IDX,IER
C
      INTEGER I,J,II,JJ,KK,RLEN,ALEN
      LOGICAL ACTION_FOUND,MENU_FOUND
      CHARACTER*80 OLD_RCPFILE, OLD_ACTION
C
      SAVE OLD_RCPFILE,OLD_ACTION,II,JJ,KK,MENU_FOUND,ACTION_FOUND
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXBUILDCOM.INC'
C----------------------------------------------------------------------
      IER = 0
      CALL SWORDS(RCPFILE,I,J,RLEN)
      CALL SWORDS(CURRENT_ACTION,I,J,ALEN)
C
C ****  Check if the indexes were not requested previously
C
      IF( ( OLD_RCPFILE(1:RLEN) .NE. RCPFILE(1:RLEN) ) .OR.
     &    ( OLD_ACTION(1:ALEN)  .NE. CURRENT_ACTION(1:ALEN) )) THEN
        MENU_FOUND = .FALSE.
        ACTION_FOUND = .FALSE.
        II = 0
        JJ = 0
        KK = 0
C
C ****  Get the menu index
C
        DO WHILE ( ( .NOT. MENU_FOUND )  .AND. (II .LE. MENU_COUNT) )
          II = II + 1
          IF ( RCPFILE .EQ. RCPFILE_NAME(II) ) THEN
            MENU_FOUND = .TRUE.
            MENUIDX = RCPFILE_INDX(II)
          ENDIF
        ENDDO
C
C ****  Get Submenu and Action index
C
        IF ( MENU_FOUND ) THEN
          DO WHILE ( (JJ .LE. SUBMENU_COUNT(MENUIDX))
     &         .AND. (.NOT. ACTION_FOUND) )
            JJ = JJ + 1
            KK = 0
            DO WHILE ( ( .NOT. ACTION_FOUND ).AND.
     &                 ( KK .LE. ACTION_COUNT(MENUIDX,JJ)) )
              KK = KK + 1
              IF ( CURRENT_ACTION(1:ALEN) .EQ. 
     &             ACTION_COMMAND(MENUIDX,JJ,KK)(1:ALEN) ) THEN
                SMENUIDX = JJ
                ACTION_IDX = KK
                ACTION_FOUND = .TRUE.
              ENDIF
            ENDDO
          ENDDO
          OLD_RCPFILE = RCPFILE
          OLD_ACTION = CURRENT_ACTION
        ENDIF
      ELSE
C
C ****  If the indexes were requested previouly return the saved indexes
C
        MENUIDX    = RCPFILE_INDX(II)
        SMENUIDX   = JJ
        ACTION_IDX = KK
      ENDIF
C
      IER = .NOT. ( MENU_FOUND .AND. ACTION_FOUND )
  999 RETURN
      END

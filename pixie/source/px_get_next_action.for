      SUBROUTINE PX_GET_NEXT_ACTION(COMBINED_VIEW_COMMAND,PACKAGE,
     &           ACTION_COMMAND,ACTION_NUMBER,PARAM,PARAM_VALUE,
     &           PARAM_TYPE,NUMBER_PARAMS,ACTION_REM,PTR,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the next action from a combined-view
C-   data-structure in the current RCP-bank.
C-
C-   Inputs  : COMBINED_VIEW_COMMAND[C*]: Combined view command
C-
C-   Outputs : PACKAGE         [C*]: Name of the package containing view
C-             ACTION_COMMAND  [C(*)*]: Action to be performed
C-             ACTION_NUMBER   [I ]: Counter to keep track of how many action
C-                                   commands there are
C-             GENDER          [C*]: Defines the type of parameters
C-                                   '%PARAMS' - Parameter element
C-                                   '%SCREEN' - Screen parameter element
C-             PARAM        [C*(*)]: Name of the parameters found
C-             PARAM_VALUE   [I(*)]: Values found in the action block.
C-             PARAM_TYPE   [C*(*)]: Types of the values found
C-             NUMBER_PARAMS   [I ]: Total number of parameters
C-             ACTION_REM      [C*]: Remark(s) of the action(s)
C-             PTR             [I ]: Pointer pointing to the next action line
C-             IER             [I ]: Error flag
C-
C-   The command array
C-   should have the following format:
C-
C-      \ARRAY combined-view-command
C-              '%TITLE'        'combined-view-title'   ' '
C-
C-              '%PACKAGE'      'package-name'          ' '
C-              '%ACTION'       'action-command'        ' '
C-                              :       :
C-              '%SCREEN'       ' '     ' '
C-                      'parameter-name'        new-value       ' '
C-                              :               :
C-              '%PARAMS'       ' '     ' '
C-                      'parameter-name'        new-value       ' '
C-                      :               :
C-      \END
C-
C-   Created  21-SEP-1990   Lupe Howell
C-   Updated  27-SEP-1990   Harrison B. Prosper
C-      Added title
C-   Updated  15-NOV-1990   Harrison B. Prosper
C-      Added error checking and read from local array
C-   Updated  26-FEB-1991   Lupe Howell  Alowing more than one ACTION_COMMAND
C-   Updated  21-MAR-1991   Harrison B. Prosper
C-      Add more error checking
C-   Updated  21-OCT-1992   Lupe Howell Added ACTION_REM parameter
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) COMBINED_VIEW_COMMAND
      CHARACTER*(*) PACKAGE
      CHARACTER*(*) ACTION_COMMAND(*)
      CHARACTER*(*) PARAM(*)
      CHARACTER*(*) ACTION_REM
      INTEGER PARAM_VALUE(*), PARAM_TYPE(*),ACTION_NUMBER,NUMBER_PARAMS
      INTEGER PTR,IER
C----------------------------------------------------------------------
      LOGICAL MORE_PARAMS, MORE_ACTIONS
      LOGICAL GET_PARAMETER, GET_VALUE
      INTEGER IVAL,ITYPE,LCVAL,LAST_PTR,IM,JM,KM,I
      CHARACTER*80 CVAL
      CHARACTER*80 MESSAGE
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
C
      IF ( PTR .EQ. 1 ) THEN
C
C ****  Skip %TITLE KEYWORD
C
        CALL EZGET_NEXT_VALUE_TYPE(COMBINED_VIEW_COMMAND,
     &                             IVAL,
     &                             CVAL,
     &                             ITYPE,
     &                             LCVAL,
     &                             IER,PTR)
        IF ( CVAL(1:2) .NE. '%T' ) THEN
          MESSAGE = ' No %TITLE keyword in command '//
     &          COMBINED_VIEW_COMMAND
          CALL SWORDS(MESSAGE,IM,JM,KM)
          CALL ERRMSG('NOTITLE_TOKEN','PX_GET_NEXT_ACTION',
     &         MESSAGE(IM:JM),'F')
        ENDIF
C
C ****  Skip title
C
        CALL EZGET_NEXT_VALUE_TYPE(COMBINED_VIEW_COMMAND,
     &                             IVAL,
     &                             CVAL,
     &                             ITYPE,
     &                             LCVAL,
     &                             IER,PTR)
        IF ( ITYPE .LT. VTCHR ) THEN
          MESSAGE = ' No TITLE in command '//COMBINED_VIEW_COMMAND
          CALL SWORDS(MESSAGE,IM,JM,KM)
          CALL ERRMSG('NOTITLE','PX_GET_NEXT_ACTION',
     &     MESSAGE(IM:JM),'F')
        ENDIF
C
C ****  Skip Remark
C
        CALL EZGET_NEXT_VALUE_TYPE(COMBINED_VIEW_COMMAND,
     &                             IVAL,
     &                             CVAL,
     &                             ITYPE,
     &                             LCVAL,
     &                             IER,PTR)
        MESSAGE = ' No TITLE remark in command '//COMBINED_VIEW_COMMAND
        CALL SWORDS(MESSAGE,IM,JM,KM)
        IF ( ITYPE .LT. VTCHR ) THEN
          CALL ERRMSG('NOREMARK','PX_GET_NEXT_ACTION',
     &     MESSAGE(IM:JM),'F')
        ENDIF
      ENDIF
C
C ****  Check for %PACKAGE key word
C
      CALL EZGET_NEXT_VALUE_TYPE(COMBINED_VIEW_COMMAND,
     &                           IVAL,
     &                           CVAL,
     &                           ITYPE,
     &                           LCVAL,
     &                           IER,PTR)
      MESSAGE = ' No %PACKAGE keyword in command '
     &         //COMBINED_VIEW_COMMAND
      CALL SWORDS(MESSAGE,IM,JM,KM)
      IF ( CVAL(1:4) .NE. '%PAC' ) THEN
        CALL ERRMSG('NOPACKAGE_TOKEN','PX_GET_NEXT_ACTION',
     &     MESSAGE(IM:JM),'F')
      ENDIF
C
C ****  Get package name
C
      CALL EZGET_NEXT_VALUE_TYPE(COMBINED_VIEW_COMMAND,
     &                           IVAL,
     &                           CVAL,
     &                           ITYPE,
     &                           LCVAL,
     &                           IER,PTR)
      IF ( CVAL(1:1) .EQ. ' ' ) THEN
        MESSAGE = ' Invalid package name in '//COMBINED_VIEW_COMMAND
        CALL SWORDS(MESSAGE,IM,JM,KM)
        CALL ERRMSG('NOPACKAGE','PX_GET_NEXT_ACTION',
     &     MESSAGE(IM:JM),'F')
      ENDIF
      PACKAGE = CVAL(1:LCVAL)
C
C ****  Skip the remark
C
      CALL EZGET_NEXT_VALUE_TYPE(COMBINED_VIEW_COMMAND,
     &                           IVAL,
     &                           CVAL,
     &                           ITYPE,
     &                           LCVAL,
     &                           IER,PTR)
      IF ( ITYPE .LT. VTCHR ) THEN
        MESSAGE = ' Invalid remark in command '//COMBINED_VIEW_COMMAND
        CALL SWORDS(MESSAGE,IM,JM,KM)
        CALL ERRMSG('NOREMARK','PX_GET_NEXT_ACTION',
     &     MESSAGE(IM:JM),'F')
      ENDIF
      ACTION_REM = CVAL(1:LCVAL)
C
C ****  Get action command(s)
C
      ACTION_NUMBER = 0
      MORE_ACTIONS = .TRUE.
      DO WHILE ( MORE_ACTIONS )
C
C ****  Check for %ACTION
C
        CALL EZGET_NEXT_VALUE_TYPE(COMBINED_VIEW_COMMAND,
     &                             IVAL,
     &                             CVAL,
     &                             ITYPE,
     &                             LCVAL,
     &                             IER,PTR)
        IF ( ITYPE .LT. VTCHR ) THEN
          MESSAGE = ' Invalid data TYPE in command '//
     &              COMBINED_VIEW_COMMAND
          CALL SWORDS(MESSAGE,IM,JM,KM)
          CALL ERRMSG('NOACTION_TOKEN','PX_GET_NEXT_ACTION',
     &     MESSAGE(IM:JM),'F')
        ENDIF
C
        IF ( CVAL(1:2) .EQ. '%A' ) THEN
C
C ****  Get action
C
          CALL EZGET_NEXT_VALUE_TYPE(COMBINED_VIEW_COMMAND,
     &                             IVAL,
     &                             CVAL,
     &                             ITYPE,
     &                             LCVAL,
     &                             IER,PTR)
          IF ( ITYPE .LT. VTCHR ) THEN
            MESSAGE =
     &        ' No ACTION line in command'//COMBINED_VIEW_COMMAND
            CALL SWORDS(MESSAGE,IM,JM,KM)
            CALL ERRMSG('NOACTION','PX_GET_NEXT_ACTION',
     &           MESSAGE(IM:JM),'F')
          ENDIF
          ACTION_NUMBER = ACTION_NUMBER + 1
          ACTION_COMMAND(ACTION_NUMBER) = CVAL(1:LCVAL)
C
C ****  Skip remark
C
          CALL EZGET_NEXT_VALUE_TYPE(COMBINED_VIEW_COMMAND,
     &                             IVAL,
     &                             CVAL,
     &                             ITYPE,
     &                             LCVAL,
     &                             IER,PTR)
          LAST_PTR = PTR
        ELSE
          MORE_ACTIONS = .FALSE.
        ENDIF
      ENDDO
      PTR = LAST_PTR
C
C ****  Loop over parameter names and values
C ****  until NEXT %PACKAGE token
C
      NUMBER_PARAMS = 0
      MORE_PARAMS   = .TRUE.
      GET_PARAMETER = .TRUE.
      GET_VALUE     = .FALSE.
      DO WHILE ( MORE_PARAMS .AND. (IER .EQ. 0) )
C
C ****  Get Parameter name
C
        CALL EZGET_NEXT_VALUE_TYPE(COMBINED_VIEW_COMMAND,
     &                                 IVAL,
     &                                 CVAL,
     &                                 ITYPE,
     &                                 LCVAL,
     &                                 IER,PTR)

        IF     ( CVAL(1:4) .EQ. '%PAC' ) THEN
          MORE_PARAMS = .FALSE.
        ELSE
          IF ( GET_PARAMETER ) THEN
            GET_PARAMETER = .FALSE.
            GET_VALUE     = .TRUE.
C
            IF ( ITYPE .LT. VTCHR ) THEN
              MESSAGE = ' No parameter where expected in command '//
     &                  COMBINED_VIEW_COMMAND
              CALL SWORDS(MESSAGE,IM,JM,KM)
              CALL ERRMSG('NOPARAMETER','PX_GET_NEXT_ACTION',
     &                  MESSAGE(IM:JM),'F')
            ENDIF
            IF ( CVAL(1:1) .EQ. ' ' ) THEN
              MESSAGE = ' Invalid parameter name in command '//
     &                  COMBINED_VIEW_COMMAND
              CALL SWORDS(MESSAGE,IM,JM,KM)
              CALL ERRMSG('BADPARAMETER','PX_GET_NEXT_ACTION',
     &              MESSAGE(IM:JM),'F')
            ENDIF
C
            NUMBER_PARAMS = NUMBER_PARAMS + 1
            PARAM(NUMBER_PARAMS) = CVAL(1:LCVAL)
C
          ELSEIF ( GET_VALUE ) THEN
            GET_VALUE     = .FALSE.
            PARAM_VALUE(NUMBER_PARAMS) = IVAL
            PARAM_TYPE(NUMBER_PARAMS) = ITYPE
C
          ELSE
            GET_PARAMETER = .TRUE.
C
            IF ( ITYPE .LT. VTCHR ) THEN
              MESSAGE = ' No REMARK where expected in command '//
     &                  COMBINED_VIEW_COMMAND
              CALL SWORDS(MESSAGE,IM,JM,KM)
              CALL ERRMSG('NOREMARK','PX_GET_NEXT_ACTION',
     &             MESSAGE(IM:JM),'F')
            ENDIF
C
            LAST_PTR = PTR              ! End of remark
          ENDIF
        ENDIF
      ENDDO
      PTR = LAST_PTR
  999 RETURN
      END

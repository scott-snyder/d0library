      SUBROUTINE ADD_ALARM ( SOURCE,MESSAGE,PRIORITY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Add ALARM message to alarm queue
C-
C-   Inputs  : SOURCE      [C*4] Package name
C-             MESSAGE     [C*]  Alarm message
C-             PRIORITY    [I]   Alarm Priority
C-   Outputs : None
C-   Controls: None
C-
C-   Created  29-OCT-1991   Boaz Klima, Harrison B Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) SOURCE,MESSAGE
      INTEGER PRIORITY
      INCLUDE 'D0$INC:EXAMINE2_ALARM.INC'
      LOGICAL FLGVAL
C----------------------------------------------------------------------
      IF (FLGVAL('ALARM_CONNECT')) THEN
        IF ( NALARMS.LT.MAX_ALARMS ) THEN
          NALARMS = NALARMS + 1
          PACKAGE_SOURCE(NALARMS) = SOURCE(1:LEN(SOURCE))
          PACKAGE_MESSAGE(NALARMS) = MESSAGE(1:LEN(MESSAGE))
          PACKAGE_PRIORITY(NALARMS) = PRIORITY
        ELSE
          CALL ERRMSG(' MAXALARM','ADD_ALARM',
     &      ' Too many ALARMs ','W')
        ENDIF
      ENDIF
  999 RETURN
      END

C----------------------------------------------------------------------
C-
C-   Created  29-OCT-1991   Boaz Klima
C-
C----------------------------------------------------------------------
      INTEGER MAX_ALARMS
      PARAMETER( MAX_ALARMS = 1000 )
      INTEGER NALARMS,PACKAGE_PRIORITY(MAX_ALARMS)
      CHARACTER*4 PACKAGE_SOURCE(MAX_ALARMS)
      CHARACTER*80 PACKAGE_MESSAGE(MAX_ALARMS)
      COMMON /EXAMINE2_ALARM/ NALARMS,PACKAGE_PRIORITY,PACKAGE_SOURCE,
     &                        PACKAGE_MESSAGE

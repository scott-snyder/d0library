      INTEGER FUNCTION GZSSTA ( STATION )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get link to bank SSTA with information about
C-                         the SAMUS station #STATION
C-
C-   Returned value  : Link to necessary bank SSTA
C-   Inputs  : STATION - the SAMUS station number
C-   Outputs : None
C-   Controls: 
C-
C-   Created  23-APR-1991   Andrei Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER STATION, LSSTH,GZSSTH, N_STATIONS
      PARAMETER( N_STATIONS = 6 )
      CHARACTER*80 MSGSTR               ! Error message
C----------------------------------------------------------------------
C
      GZSSTA = 0
      IF (STATION.GT.N_STATIONS) GOTO 999
C
      LSSTH = GZSSTH()
      IF ( LSSTH.GT.0 ) THEN
        GZSSTA=LC(LSSTH-STATION)
      ELSE
        MSGSTR = ' *** GZSSTA: bank SSTH is absent '
        CALL INTMSG (MSGSTR)
        GOTO 999
      ENDIF
C
  999 RETURN
      END

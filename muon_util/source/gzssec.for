C+
      INTEGER FUNCTION GZSSEC (STATION, SECTION)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get link to bank SSEC with information about
C-                         drift tubes in section #SECTION of the SAMUS
C-                         station #STATION
C-
C-   Returned value  : Link to necessary bank SSEC
C-   Inputs  : STATION - the SAMUS station number
C-             SECTION - the SAMUS section number
C-   Outputs : none.
C-   Controls: none.
C-
C-   Created  23-APR-1991   Andrei Kiryunin
C-   Updated  13-OCT-1992   Alexander Efimov   Check input parameters
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N_STATIONS, N_SECTIONS
      PARAMETER (N_STATIONS=6, N_SECTIONS=6)
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER STATION, SECTION, LSSTA, GZSSTA
      EXTERNAL GZSSTA
      CHARACTER*80 MSG
C
C ****  check input parameters
C
      GZSSEC = 0
      IF (STATION .LT. 1 .OR. STATION .GT. N_STATIONS) THEN
        WRITE (MSG, '('' GZSSEC error: wrong station number ='', I7)')
     &         STATION
        CALL INTMSG (MSG)
        GO TO 999
      END IF
      IF (SECTION .LT. 1 .OR. SECTION .GT. N_SECTIONS) THEN
        WRITE (MSG, '('' GZSSEC error: wrong section number ='',I7)')
     &         SECTION
        CALL INTMSG (MSG)
        GO TO 999
      END IF
C
C ****  get section address
C
      LSSTA = GZSSTA (STATION)
      IF (LSSTA .GT. 0) THEN
        GZSSEC = LC(LSSTA-SECTION)
      ELSE
        MSG = ' GZSSEC error: bank SSTA is absent '
        CALL INTMSG (MSG)
        GOTO 999
      ENDIF
C
  999 RETURN
      END

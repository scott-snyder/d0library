C+
      INTEGER FUNCTION BKSSTH ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create the Zebra bank SSTH and hanging under 
C-                         it banks with constants for the SAMUS drift 
C-                         tube stations description
C-
C-   Returned value  : bank SSTH address (or zero if something is bad)
C-   Inputs  : None
C-   Outputs : None
C-   Controls: 
C-
C-   Created  29-APR-1991   Andrei Kiryunin
C-   Updated  12-NOV-1992   Alexander Efimov   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSSTH.LINK'
      INTEGER LSSTH, GZSSTH, LSSAM, GZSSAM, NFORM, IERR
      INTEGER IVAL, IAR(100), NST, NSEC, I
      INTEGER STATIONS, SECTIONS
      INTEGER NDATA
      PARAMETER (NDATA=28)
      INTEGER N_STATIONS, N_SECTIONS
      PARAMETER (N_STATIONS=6, N_SECTIONS=6)
      CHARACTER*32 CHAR_STATION(N_STATIONS),
     &             CHAR_SECTION(N_SECTIONS)
      CHARACTER*32 NAME
      CHARACTER*80 MSGSTR
C
      BKSSTH = 0
C
C ****  Check existence of the SSAM bank
C
      LSSAM = GZSSAM()
      IF (LSSAM .EQ. 0) CALL BKSSAM ()
C
C ****  Create bank SSTH
C
      CALL MZFORM ('SSTH', '2I 1F 5I 2F 2I 7F 1I 2F 1I 5F', NFORM)
      CALL MZBOOK (IDVSTP, LSSTH, LSSAM, -IZSSTH, 'SSTH', 6, 6,
     &             NDATA, NFORM, 0)
C
C ****  Read in RCP file with necessary static parameters
C
      CALL INRCP ('SAMUS_STATION_RCP', IERR)
      IF (IERR .NE. 0) THEN
        MSGSTR = ' *** BKSSTH: error openning of the RCP file '
        CALL INTMSG (MSGSTR)
        GOTO 999
      END IF
      CALL EZPICK ('SAMUS_STATION_RCP')
C
C ****  Get data for header bank
C
        CALL EZGET ('HEADER', IAR, IERR)
        IF (IERR .NE. 0) THEN
          MSGSTR = ' *** BKSSTH: error during EZGET of header '
          CALL INTMSG (MSGSTR)
          GOTO 999
        END IF
        DO I = 1, NDATA
          IC(LSSTH+I) = IAR(I)
        END DO
C
C ****  Get the list with stations names
C
      CALL EZ_GET_CHARS ('STATION_VOLUMES', STATIONS,
     &                    CHAR_STATION, IERR)
      IF (IERR .NE. 0) THEN
        MSGSTR = ' *** BKSSTH: error during EZ_GET_CHAR of stations '
        CALL INTMSG (MSGSTR)
        GOTO 999
      END IF
      IF (STATIONS .GT. N_STATIONS) THEN
        MSGSTR = ' *** BKSSTH: too many SAMUS stations in RCP file '
        CALL INTMSG (MSGSTR)
        GOTO 999
      END IF
C
C ****  Get values for station description
C
      DO NST = 1, STATIONS
        CALL EZGETA (CHAR_STATION(NST), 0, 0, 0, IVAL, IERR)
        IF (IERR .NE. 0) THEN
          MSGSTR = ' *** BKSSTH: error during EZGETA of stations '
          CALL INTMSG (MSGSTR)
          GOTO 999
        END IF
        CALL EZGET (CHAR_STATION(NST), IAR, IERR)
        IF (IERR .NE. 0) THEN
          MSGSTR = ' *** BKSSTH: error during EZGET of stations '
          CALL INTMSG (MSGSTR)
          GOTO 999
        END IF
C
C ****  Create bank SSTA with data from IAR
C
        CALL BKSSTA (NST, IVAL, IAR)
C
C ****  Get the list with sections names
C
        WRITE (NAME, '(''STATION_'',I1,''_VOLUMES'')') NST
        CALL EZ_GET_CHARS (NAME, SECTIONS, CHAR_SECTION, IERR)
        IF (IERR .NE. 0) THEN
          MSGSTR = ' *** BKSSTH: error during EZ_GET_CHAR of sections '
          CALL INTMSG (MSGSTR)
          GOTO 999
        END IF
        IF (SECTIONS .GT. N_SECTIONS) THEN
          MSGSTR = ' *** BKSSTH: too many SAMUS sections in RCP file '
          CALL INTMSG (MSGSTR)
          GOTO 999
        END IF
        DO NSEC = 1, SECTIONS
C
C ****  Get values for station description
C
          CALL EZGETA (CHAR_SECTION(NSEC), 0, 0, 0, IVAL, IERR)
          IF (IERR .NE. 0) THEN
            MSGSTR = ' *** BKSSTH: error during EZGETA of sections '
            CALL INTMSG (MSGSTR)
            GOTO 999
          END IF
          CALL EZGET (CHAR_SECTION(NSEC), IAR, IERR)
          IF (IERR .NE. 0) THEN
            MSGSTR = ' *** BKSSTH: error during EZGET of sections '
            CALL INTMSG (MSGSTR)
            GOTO 999
          END IF
C
C ****  Create bank SSEC with data from IAR
C
          CALL BKSSEC (NST, NSEC, IVAL, IAR)
        END DO
      END DO
C
C ****  define addresses
C
      CALL EZRSET
      LSSTH = GZSSTH()
      BKSSTH = LSSTH
C
  999 RETURN
      END

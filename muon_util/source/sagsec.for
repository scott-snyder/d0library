C+
      INTEGER FUNCTION SAGSEC (STATION, SECTION, NTUBES, TYPE, SHIFT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get SAMUS section geometry parameters.
C-
C-   Inputs  : STATION number
C-             SECTION number
C-   Outputs : NTUBES - number of tubes in this section
C-             TYPE - section type
C-             SHIFT - special shift for tubes in the C stations
C-   Controls: none.
C-
C-   Created  27-SEP-1990   Alexander Efimov
C-   Updated  30-APR-1991   Andrei Kiryunin: geometry from banks SSTH.
C-   Updated  19-NOV-1992   Alexander Efimov: add Ailer angles for
C-                          stations orientation.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER N_STATIONS, N_SECTIONS
      PARAMETER (N_STATIONS=6, N_SECTIONS=6)
      INTEGER STATION, SECTION, TYPE, NTUBES, SHIFT
      INTEGER GZSSTA, LSSEC, GZSSEC
C
      SAGSEC = -1
C
C ****  Get information from SAMUS stpfile
C
      IF (STATION .LT. 1 .OR. STATION .GT. N_STATIONS) GO TO 999
      IF (SECTION .LT. 1 .OR. SECTION .GT. N_SECTIONS) GO TO 999
      LSSEC = GZSSEC(STATION,SECTION)
      IF (LSSEC .EQ. 0) GOTO 999
      NTUBES = IC(LSSEC+4)
      TYPE = IC(LSSEC+2)
      SHIFT = IC(LSSEC+5)
      SAGSEC = +1
C
  999 RETURN
      END

      SUBROUTINE SACHAN(NST,NSEC,NTU,NCHAN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert SAMUS tube number into electronics
C-                         channel number
C-
C-   Inputs  : NST - station number
C-             NSEC - section number
C-             NTU - tube number
C-   Outputs : NCHAN - electronic channel number
C-   Controls: none
C-
C-   Created   8-APR-1991   V. GLEBOV
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NST, NSEC, NTU, NCHAN
C----------------------------------------------------------------------
      NCHAN = NTU
C
C ****  Special shift for connected tubes in C stations (NST=3,6)
C
      IF (NST .EQ. 3 .OR. NST .EQ. 6) THEN
C
C ****  For X and Y planes (NSEC=1,3,4,6)
C
        IF (NSEC .NE. 2 .AND. NSEC .NE. 5) THEN
          IF (NTU .GT. 128) NCHAN = NTU - 51
C
C ****  For U plane (NSEC=2,5)
C
        ELSEIF (NTU .GT. 176) THEN
          NCHAN = NTU - 65
        ENDIF
      ENDIF
C
C ****  Count channel from 0 (for X, Y planes) and from 16 (for U plane)
C
      IF (NSEC .NE. 2 .AND. NSEC .NE. 5) THEN
        NCHAN = NCHAN - 1                   
      ELSE
        NCHAN = NCHAN + 15
      ENDIF
C
  999 RETURN
      END

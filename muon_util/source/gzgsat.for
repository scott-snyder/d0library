      INTEGER FUNCTION GZGSAT ( ITRACK )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get link to bank GSAT with information about 
C-                         track # ITRACK
C-
C-   Returned value  : Link to bank GSAT
C-   Inputs  : ITRACK - # track (if equals 0 - link to the first bank in 
C-             linear structure)
C-   Outputs : None
C-   Controls: 
C-
C-   Created   4-APR-1991   Andrei Kiryunin
C-   Modified  15-JUL-1994  Chip Stewart GZGMUH(0)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZGSAT.LINK'
      INTEGER I,ITRACK,LGMUH,GZGMUH
C----------------------------------------------------------------------
C
      GZGSAT = 0
C
      LGMUH = GZGMUH(0)
      IF ( LGMUH.GT.0 ) THEN
        GZGSAT=LQ(LGMUH-IZGSAT)
        IF (ITRACK.EQ.0) GOTO 999
  10    IF (GZGSAT.EQ.0) GOTO 999
        IF (IQ(GZGSAT+1).EQ.ITRACK) GOTO 999
        GZGSAT=LQ(GZGSAT)
        GOTO 10
      ENDIF
C
  999 RETURN
      END

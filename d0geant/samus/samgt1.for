      SUBROUTINE SAMGT1 ( ITRACK, HITS )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill bank GSAT (under GMUH - GHIT - GEAN - HEAD)
C-                         with information about GEANT track # ITRACK
C-
C-   Inputs  : ITRACK - track number in GEANT
C-             HITS - 5 hits in SAMUS tube for this track
C-   Outputs : None
C-   Controls: 
C-
C-   Created   5-APR-1991   Andrei Kiryunin
C-   Updated  24-APR-1991   Andrei Kiryunin: added fifth word in hit 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:D0LOG.INC'
      INTEGER ITRACK,LGSAT,GZGSAT,NHITS
      REAL    HITS(5)
      CHARACTER*80 MSGSTR               ! Error message
C----------------------------------------------------------------------
C
C ****  Find bank GSAT for track #ITRACK or create it
C
      LGSAT = GZGSAT(ITRACK)
      IF ( LGSAT.EQ.0 ) THEN
        CALL BKGSAT (ITRACK,LGSAT)
      ENDIF
C
C ****  Pack hits
C
      NHITS=IQ(LGSAT+10)+1
      IF (NHITS.LE.72) THEN
        IQ(LGSAT+10)=NHITS
        CALL UCOPY (HITS(1),Q(LGSAT+NHITS*5+6),5)
      ELSE
        IF (PSAM.GE.2) THEN
          MSGSTR = ' *** SAMGT1: too many hits per track in SAMUS '
          CALL INTMSG (MSGSTR)
        ENDIF
      ENDIF
C
  999 RETURN
      END

      INTEGER FUNCTION GZPROC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Get pointer to PROC bank
C-      PATHGZ returns pointer for RECO, GEAN or FAKE
C-      depending on path chosen with PATHST
C-
C-   Created   4-NOV-1988   Serban D. Protopopescu
C-   Updated   6-JAN-1996   Bob Hirosky  add bank name checking
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZPROC.LINK'
      INTEGER LSUP,PATHGZ
      CHARACTER*4 BNAME
C----------------------------------------------------------------------
C
      GZPROC=0
      LSUP=PATHGZ()

      IF ( LSUP .GT. 0 ) THEN
C
C: Is supporting link RECO, GEAN or FAKE?  - check
C
        CALL UHTOC( IQ(LSUP-4), 4, BNAME, 4 )
        IF ( ( BNAME .EQ. 'RECO' ).OR.( BNAME .EQ. 'GEAN' )
     &      .OR.( BNAME .EQ. 'FAKE' ) ) GZPROC=LQ(LSUP-IZPROC)
      ENDIF
C
  999 RETURN
      END

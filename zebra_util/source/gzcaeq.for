      INTEGER FUNCTION GZCAEQ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns link to CAEQ bank
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  25-MAR-1992   Andrew J. Milder
C-   Updated  26-OCT-1995   Krzysztof L. Genser  make sure proc bank has
C-                          the caeq link 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZPROC,LPROC, GZANLS, GZMDST, LANLS
      EXTERNAL GZPROC, GZANLS, GZMDST
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAEQ.LINK'
      CHARACTER*4 BNAME
C----------------------------------------------------------------------
      GZCAEQ = 0
C
C: This bank should normally (Run 1B) be under PROC
C
      LPROC = GZPROC()
C
C      IF (LPROC .GT. 0 ) GZCAEQ = LQ( LPROC - IZCAEQ )
C      IF ( GZCAEQ .GT. 0 ) RETURN
      IF (LPROC .GT. 0 ) THEN
C
C     make sure the bank has "enough" structural links
C
        IF (IQ(LPROC-2).GE.IZCAEQ) THEN
          GZCAEQ = LQ( LPROC - IZCAEQ )
          IF ( GZCAEQ .GT. 0 ) RETURN
        ENDIF
C
      ENDIF
C
C: But for a MDST, it may be under ANLS as well
C
      IF ( GZMDST() .GT. 0 ) THEN           ! QCD MDST, look under ANLS
        LANLS = GZANLS()
        IF ( LANLS .GT. 0 ) GZCAEQ = LQ( LANLS - 2 )
        IF ( GZCAEQ .GT. 0 ) THEN
C
C: Sometimes other banks have this link - check
C
          CALL UHTOC( IQ(GZCAEQ-4), 4, BNAME, 4 )
          IF ( BNAME .NE. 'CAEQ' ) GZCAEQ = 0
        ENDIF
      ENDIF
  999 RETURN
      END

      SUBROUTINE VTNEAR(TIME1, LIST1, NDATA1, OFFSET,
     &                  TIME2, LIST2, NDATA2, NEAR1, NEAR2 )  
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Associate nearest hits in two lists.
C-                         NEAR1(IHIT1) gives the hit number IHIT2
C-                         of the hit in list 2 which is closest to
C-                         hit number IHIT1 in list 1. Vice versa
C-                         for NEAR2(IHIT2).
C-
C-   Inputs  : TIME1: array of hit times for one end of wire
C-             TIME2: array of times for other end of wire
C-             LIST1,LIST2:give time orderings for TIME arrays
C-             NDATA1,NDATA2:number of times in respective array
C-             OFFSET: average time difference t(+z) - t(-z) for
C-                     matched hit
C-
C-   Outputs : NEAR1,NEAR2: arrays of closest hits (in time) described above
C- 
C-   Controls: 
C-
C-   Created   1-FEB-1989   Peter Grudberg (from Chris K)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NDATA1, NDATA2
      INTEGER LIST1(NDATA1), LIST2(NDATA2)
      REAL TIME1(NDATA1), TIME2(NDATA2), OFFSET
      INTEGER NEAR1(NDATA1), NEAR2(NDATA2)
C
      INTEGER INDEX1, INDEX2, IHIT1, IHIT2
      REAL DELTA
C----------------------------------------------------------------------
      CALL VZERO( NEAR1, NDATA1 )
      CALL VZERO( NEAR2, NDATA2 )
C  Loop over hits in list 1, finding nearest hit in list 2 for each
      IF ( NDATA2 .GT. 0 ) THEN
        DO 100 INDEX1 = 1 , NDATA1
          INDEX2 = 1
          IHIT1  = LIST1(INDEX1)
          IHIT2  = LIST2(INDEX2)
          DELTA  = ABS( TIME2(IHIT2) - TIME1(IHIT1) + OFFSET )
C
          DO 110 INDEX2 = 2 , NDATA2
            IHIT2 = LIST2(INDEX2)
            IF ( ABS( TIME2(IHIT2) - TIME1(IHIT1) + OFFSET )
     &           .GT. DELTA ) GO TO 10
            DELTA = ABS( TIME2(IHIT2) - TIME1(IHIT1) + OFFSET )
  110     CONTINUE
   10     CONTINUE                        ! Jump out of loop when NEAR found
          INDEX2 = INDEX2 - 1
          NEAR1(IHIT1) = LIST2(INDEX2)
  100   CONTINUE
      ENDIF
C Now repeat with the two lists reversed to build NEAR2
      IF ( NDATA1 .GT. 0 ) THEN
        DO 200 INDEX2 = 1 , NDATA2
          INDEX1 = 1
          IHIT2  = LIST2(INDEX2)
          IHIT1  = LIST1(INDEX1)
          DELTA  = ABS( TIME2(IHIT2) - TIME1(IHIT1) + OFFSET )
C
          DO 210 INDEX1 = 2 , NDATA1
            IHIT1 = LIST1(INDEX1)
            IF ( ABS( TIME2(IHIT2) - TIME1(IHIT1) + OFFSET )
     &           .GT. DELTA ) GO TO 20
            DELTA = ABS( TIME2(IHIT2) - TIME1(IHIT1) + OFFSET )
  210     CONTINUE
   20     CONTINUE                        ! Jump out of loop when NEAR found
          INDEX1 = INDEX1 - 1
          NEAR2(IHIT2) = LIST1(INDEX1)
  200   CONTINUE
      ENDIF
  999 RETURN
      END

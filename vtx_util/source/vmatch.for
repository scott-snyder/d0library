      SUBROUTINE VMATCH(NDATA1,VTDAT1,NDATA2,VTDAT2,NPAIR,PAIR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Subroutine VMATCH finds matches between hits
C-   from opposite ends of a wire.  The matching algorithm used is:
C-          (1): For all (-z) hits, find nearest (in time) (+z) hit
C-          (2): For all (+z) hits, find nearest (-z) hit
C-          (3): If two hits are mutually nearest to one another,
C-               and the time difference between the hits is within a
C-               time window, the match is made.
C-          (4): The unmatched (-z) and (+z) hits are added to the list
C-               of pairs
C-
C-   Inputs  : NDATA1: number of hits on (-z) end of wire
C-             VTDAT1(NWVWDA,MXHTOT):array of hit data for (-z) end
C-             NDATA2: number of hits 0n (+z) end of wire
C-             VTDAT2(NWVWDA,MXHTOT):array of hit data for (+z) end
C-
C-   Outputs : NPAIR(4): number of hit pairs ( total, matched,
C-                       unmatched (+z), unmatched (-z) )
C-             PAIR(2*MXHTOT,2):id of hits included in pair
C-               PAIR(IPAIR,1) = (-z) hit included in pair (=0 if
C-                               no (-z) hit)
C-               PAIR(IPAIR,2) = (+z) hit included in pair (0=no hit)
C-
C-   Controls: 
C-
C-   Created   1-FEB-1989   Peter Grudberg (from Chris Klopfenstein)
C-   Modified 30-JUN-1989   P. Grudberg - Use RCP for parameters
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER MXHTOT, NWVWDA
      PARAMETER ( NWVWDA = 8 )
      PARAMETER ( MXHTOT = 50 )         ! Max hits/wire-end
      INTEGER NDATA1, NDATA2, NPAIR(4), PAIR( 2*MXHTOT, 2 )
      REAL VTDAT1(NWVWDA, MXHTOT), VTDAT2(NWVWDA, MXHTOT)
C
      INTEGER IHIT, IHIT1, IHIT2, IPAIR
      INTEGER LIST1(MXHTOT), LIST2(MXHTOT)
      REAL TIME1(MXHTOT), TIME2(MXHTOT)
C
      INTEGER LAST, IFIRST, IER
      DATA IFIRST / 0 /
      REAL DELTA, OFFSET, WINDOW
      INTEGER NEAR1( MXHTOT ), NEAR2( MXHTOT )
C----------------------------------------------------------------------
C
C **** First time: get parameters from RCP
C
      IF ( IFIRST .EQ. 0 ) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET( 'OFSETW', OFFSET, IER )
        CALL EZGET( 'WINDOW', WINDOW, IER )
        CALL EZRSET
        IFIRST = 1
      ENDIF
C  Get lists of times from VTDAT1, VTDAT2
      DO 100 IHIT = 1 , MAX( NDATA1, NDATA2 )
        TIME1( IHIT ) = VTDAT1( 2, IHIT )
        TIME2( IHIT ) = VTDAT2( 2, IHIT )
  100 CONTINUE
C
C **** Sort the list of times
C (last 3 arguments: sort first NDATA real #'s in ascending order)
      IF ( NDATA1 .GT. 0 ) CALL SORTZV( TIME1, LIST1, NDATA1, 1, 0, 0 )
      IF ( NDATA2 .GT. 0 ) CALL SORTZV( TIME2, LIST2, NDATA2, 1, 0, 0 )
C  Find nearest (+z) hit to each (-z) hit and vice versa
      CALL VZERO(NEAR1,MXHTOT)
      CALL VZERO(NEAR2,MXHTOT)
      IF ( NDATA1 .GT. 0 .AND. NDATA2 .GT. 0 ) THEN
        CALL VTNEAR( TIME1, LIST1, NDATA1, OFFSET,
     &               TIME2, LIST2, NDATA2, NEAR1, NEAR2 )
      ENDIF
C  If NEAR1 and NEAR2 agree, match the hits (if within the time window)
C  If they don't agree, keep the hits separate.
C  First find all pairs and fill array pair.  Only combine the hits
C  if the time difference is less than the cut.
      CALL VZERO ( NPAIR, 4 )
      DO 110 IHIT1 = 1 , NDATA1
        IF ( NEAR1(IHIT1) .GT. 0 ) THEN
          IF ( NEAR2(NEAR1(IHIT1)) .EQ. IHIT1 ) THEN
            DELTA = ABS( TIME1(IHIT1) - TIME2(NEAR1(IHIT1))
     &                  + OFFSET )
            IF ( DELTA .LE. WINDOW ) THEN
              NPAIR(1) = NPAIR(1) + 1
              NPAIR(2) = NPAIR(2) + 1
              PAIR( NPAIR(1), 1 ) = IHIT1
              PAIR( NPAIR(1), 2 ) = NEAR1(IHIT1)
            ELSE                        ! Record the hits separately
              NPAIR(1) = NPAIR(1) + 1
              NPAIR(4) = NPAIR(4) + 1
              PAIR( NPAIR(1), 1 ) = IHIT1
              PAIR( NPAIR(1), 2 ) = 0
              NPAIR(1) = NPAIR(1) + 1
              NPAIR(3) = NPAIR(3) + 1
              PAIR( NPAIR(1), 1 ) = 0
              PAIR( NPAIR(1), 2 ) = NEAR1(IHIT1)
            ENDIF
          ELSE                          ! add unmatched (-z) hit to list
            NPAIR(1) = NPAIR(1) + 1
            NPAIR(4) = NPAIR(4) + 1
            PAIR( NPAIR(1), 1 ) = IHIT1
            PAIR( NPAIR(1), 2 ) = 0
          ENDIF
        ELSE                            ! if NEAR1(IHIT)=0, no hits on
C                                       ! (+z) end, add unmatched hit to list
          NPAIR(1) = NPAIR(1) + 1
          NPAIR(4) = NPAIR(4) + 1
          PAIR( NPAIR(1), 1 ) = IHIT1
          PAIR( NPAIR(1), 2 ) = 0
        ENDIF
  110 CONTINUE
C  Add unmatched (+z) hits to the list
      DO 130 IHIT2 = 1 , NDATA2
        IF ( NEAR2(IHIT2) .GT. 0 ) THEN
          IF ( NEAR1(NEAR2(IHIT2)) .NE. IHIT2 ) THEN
            NPAIR(1) = NPAIR(1) + 1
            NPAIR(3) = NPAIR(3) + 1
            PAIR( NPAIR(1), 1 ) = 0
            PAIR( NPAIR(1), 2 ) = IHIT2
          ENDIF
        ELSE                            ! If near2(ihit2) = 0
          NPAIR(1) = NPAIR(1) + 1
          NPAIR(3) = NPAIR(3) + 1
          PAIR( NPAIR(1), 1 ) = 0
          PAIR( NPAIR(1), 2 ) = IHIT2
        ENDIF
  130 CONTINUE
C
  999 RETURN
      END

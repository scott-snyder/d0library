      LOGICAL FUNCTION CELL_IS_HOT (IETA, IPHI, ILYR, MIN, FRAC, R)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given the address of a cell, and MIN (the minimum ET
C-   for a hot cell candidate) and FRAC (the maximum value of E(neigh)/E(hot)
C-   for a hot cell, return .TRUE. if the cell is HOT and .FALSE. otherwise.
C-
C-
C-   Returned value:  .TRUE. if the cell is hot, .FALSE. otherwise
C-   Inputs  : IETA, IPHI, ILYR [I] the indices specifying the hot cell
C-                                  canditate
C-             MIN  [R] the minimum ET for a cell to be considered hot
C-             FRAC [R] the maximum value of E(neigh)/E(hot) for a cell to
C-                      be considered hot
C-   Outputs : R    [R] the hot cell ratio for cell IETA, IPHI, ILYR
C-   Controls: none
C-
C-   Created  19-SEP-1992   Marc Paterno
C-   Updated   5-OCT-1992   Marc Paterno  Altered to handle arrays for MIN and
C-                                        FRAC
C-   Updated  18-MAR-1993   Marc Paterno  Removed handling of arrays for MIN
C-                                        and FRAC
C-   Updated  23-MAR-1993   Marc Paterno  Removed call to ERRMSG for each hot
C-                                        cell found
C-   Updated   7-APR-1993   Marc Paterno  Added output for hot cell ratio
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  IETA, IPHI, ILYR
      REAL     MIN, FRAC, R
C----------------------------------------------------------------------
      INTEGER  N
      REAL     E, ET, EX, EY, EZ, ETUP, ETDN, AVE
      LOGICAL  OK
C----------------------------------------------------------------------
      CELL_IS_HOT = .FALSE.
      R = 0.

      CALL CELL_HIT (IETA, IPHI, ILYR, E, ET, EX, EY, EZ)

      IF ( ET .LE. MIN ) RETURN         ! not enough ET to be hot

      CALL NEIGHBOR_HITS (IETA, IPHI, ILYR, ETUP, ETDN, N, OK)
      IF ( .NOT. OK ) THEN
        CALL ERRMSG ('No neighbor found', 'CELL_IS_HOT',
     &    'NEIGHBOR_HITS returned an error; will not kill cell', 'W')
        RETURN
      ENDIF

      IF ( N .EQ. 0 ) THEN              ! no neighbor was found
        AVE = 0.
        R = 0.
      ELSE
        AVE = (ETUP + ETDN)/N
        R = AVE / ET
      ENDIF                             ! if n .eq. 0

      IF ( R .LE. FRAC ) CELL_IS_HOT = .TRUE.

      RETURN
      END

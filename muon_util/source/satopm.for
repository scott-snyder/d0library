      SUBROUTINE SATOPM (NST, NSEC, PM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert SAMUS Station and Section Numbers
C-                         into Module # (Phil Martin's number)
C-
C-   Inputs  : NST - station number,
C-             NSEC - section number,
C-   Outputs : PM - Module # (Phil Martin's number)
C-   Controls: none
C-
C-   Created  25-APR-1991   V. GLEBOV
C-   Updated  25-SEP-1991   A. Efimov
C-   Updated  15-FEB-1995   I. Mandrichenko   Verify input parameters
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NST, NSEC, PM
      INTEGER N_STATION, N_SECTION
      PARAMETER (N_STATION=6, N_SECTION=6)
      INTEGER PMDAT(N_STATION,N_SECTION)
C-                NA   NB   NC   SA   SB   SC
      DATA PMDAT/400, 414, 420, 434, 440, 454,  ! X2
     *           401, 413, 421, 433, 441, 453,  ! U2
     *           402, 412, 422, 432, 442, 452,  ! Y2
     *           404, 410, 424, 430, 444, 450,  ! X1
     *           405, 417, 425, 437, 445, 457,  ! U1
     *           406, 416, 426, 436, 446, 456/  ! Y1
C-
      PM = 0
      IF( NST.GE.1 .AND. NST.LE.N_STATION .AND.
     +        NSEC.GE.1 .AND. NSEC.LE.N_SECTION )
     +        PM = PMDAT (NST, NSEC)
  999 RETURN
      END

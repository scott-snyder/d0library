      SUBROUTINE SAFRPM (PM, NST, NSEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert Phill Martin's number into
C-                         SAMUS Station and Section Numbers
C-
C-   Inputs  : PM - Phill Martin's number
C-   Outputs : NST - station number,
C-             NSEC - section number,
C-   Controls: none
C-
C-   Created  25-APR-1991   V. GLEBOV
C-   Updated  25-SEP-1991   A. Efimov
C-   Optimized 04-FEB-1995    I.Mandrichenko
C-
C----------------------------------------------------------------------
      IMPLICIT	  NONE
      INTEGER ISEC(8,6),PM,NST,NSEC,IST,ISC
C-
      DATA    ISEC/
     +    1,  2,  3, -1,  4,  5,  6, -1,
     +    4, -1,  3,  2,  1, -1,  6,  5,
     +    1,  2,  3, -1,  4,  5,  6, -1,
     +    4, -1,  3,  2,  1, -1,  6,  5,
     +    1,  2,  3, -1,  4,  5,  6, -1,
     +    4, -1,  3,  2,  1, -1,  6,  5/
C-                 NA   NB   NC   SA   SB   SC
C      DATA PMDAT/400, 414, 420, 434, 440, 454,  ! X2  1
C     *           401, 413, 421, 433, 441, 453,  ! U2  2
C     *           402, 412, 422, 432, 442, 452,  ! Y2  3
C     *           404, 410, 424, 430, 444, 450,  ! X1  4
C     *           405, 417, 425, 437, 445, 457,  ! U1  5
C     *           406, 416, 426, 436, 446, 456/  ! Y1  6
      NST = -1
      NSEC = -1
      IF( PM/100 .NE. 4 ) GOTO 999
      IST = MOD(PM/10,10) + 1
      IF( IST.GT.6 ) GOTO 999
      ISC = MOD(PM,10) + 1
      IF( ISC.GT.8 ) GOTO 999
      ISC = ISEC(ISC,IST)
      IF( ISC.EQ.-1 ) GOTO 999
      NSEC = ISC
      NST = IST
  999 CONTINUE
      RETURN
      END

C----------------------------------------------------------------------
C-
C-   Created   4-FEB-1989   Stephen Kahn
C-
C----------------------------------------------------------------------
      INTEGER NARR
      PARAMETER (NARR = 650)
      INTEGER IVAL(NARR)
      COMMON /SCRARR/ RVAL(NARR)
      REAL RVAL
      EQUIVALENCE (IVAL, RVAL)

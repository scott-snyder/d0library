      SUBROUTINE HMATRIX(NQUAN,QUAN,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : NQUAN  Number of entries in QUAN bank. These are the
C-                      number of variables to be analyzed plus the
C-                      number of variables to be predicted.
C-                      (ie Visible + Invisible quantities)
C-                    ***Set NQUAN to zero to skip event***
C-
C-   Inputs  : QUAN   Variables to be analyzed and/or predicted.  In
C-   Outputs :          HMatrix accumulation phase, user must supply all
C-                      variables.  In HMatrix analyze mode, the
C-                      predicted values are returned in their place in
C-                      the QUAN array.
C-             STATUS zero to use event
C-                    non-zero to not use this data
C-
C-   Controls:
C-
C-   Created  23-SEP-1991   joey thompson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER  NQUAN
      REAL     QUAN(*)
      INTEGER  STATUS
      LOGICAL  OK
      LOGICAL HMATRIX_EVENT
C----------------------------------------------------------------------
C
C ****  Call HMatrix_Fill to fill buffer to pass info to HMatrix Pkg
C
      STATUS = -1
      IF (NQUAN .GT. VIS_MAX + INVIS_MAX) GOTO 999  !Too many variables
      IF (NQUAN .GT. 0 ) THEN
        CALL HMATRIX_FILL(NQUAN,QUAN)
        OK = HMATRIX_EVENT()
        IF (OK) THEN
          CALL UCOPY(C(LQUAN+1),QUAN,NQUAN)
          STATUS = 0
        ENDIF
      ENDIF
  999 RETURN
      END

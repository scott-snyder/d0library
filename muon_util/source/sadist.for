C+
      REAL FUNCTION SADIST (NST, NSEC, NTUBE, TIME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate distance from drift time
C-
C-   Inputs  : NST   - Station number
C-             NSEC  - Section number
C-             NTUBE - Tube number
C-             TIME - Drift time
C-   Outputs : SADIST - distance from hit to tube axis.
C-   Controls: none.
C-
C-   Rewritten 10-DEC-1992   Alexander Efimov
C-   Updated  20-FEB-1994   Alexander Efimov
C-   Updated   7-MAR-1994   Alexander Efimov
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INTEGER NST, NSEC, NTUBE, TIME
      INTEGER GZSELC, LSELC, GZSMNT, LSMNT
      EXTERNAL GZSELC, GZSMNT
      INTEGER NADLT, NWRD, NPOL, KEY, J, L, IERR
      REAL    AA, BB, TM, TMIN, TMAX
      DOUBLE PRECISION  X, XX, F
      INTEGER PEDCUT
      SAVE    PEDCUT
      CHARACTER*32 MESSID, CALLER
      CHARACTER*80 MESSAG
      LOGICAL FIRST
      SAVE    FIRST
      DATA FIRST /.TRUE./
C
      IF (FIRST) THEN
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET ('PEDCUT', PEDCUT, IERR)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
      SADIST = - 1.0
      LSELC = GZSELC (NST, NSEC)            ! address of the SELC bank
      IF (LSELC .LE. 0) THEN
        IF (TIME .LE. PEDCUT) THEN
          SADIST = -1.0
        ELSE
          SADIST = +1.0
        END IF
        RETURN
      END IF
      L = LSELC + 10 + 6 * (NTUBE - 1)    ! address of the information
      AA =  C(L+3)                              ! A-parameter
      BB =  C(L+4)                              ! B-parameter
      IF (ABS(AA) .LT. 1.0E-13) THEN
        MESSID = 'SADIST: bad calibration parameters.'
        CALLER = 'SADIST'
        MESSAG = ' '
        CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
        RETURN
      END IF
      TM = (REAL(TIME) - BB) / AA         ! time value in ns
      LSMNT = GZSMNT (NST, NSEC)          ! address of the SMNT bank
      IF (LSMNT .LE. 0) THEN
        MESSID = 'SADIST: bank SMNT does not exist.'
        CALLER = 'SADIST'
        MESSAG = ' '
        CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
        RETURN
      END IF
      NWRD = IC(LSMNT+11)              ! number of words per 32ADLT
      NPOL = IC(LSMNT+12)              ! number of polynomal parameters
      NADLT = (NTUBE - 1) / 32 + 1               ! 32ADLT number
      L = LSMNT + (NADLT - 1) * NWRD + 12        ! address 32ADLT data
      KEY = IC(L+1)                          ! key 32ADLT data
      IF (KEY .LE. 0) RETURN                 ! bad 32ADLT data
      TMIN = C(L+2)                          ! minimum time (t0) value
      TMAX = C(L+3)                               ! maximum time value
      IF (TM .LT. TMIN .OR. TM .GT. TMAX) THEN        ! bad time
        SADIST = 1.0E+13
        RETURN
      END IF
      X = (TM - TMIN) / (TMAX - TMIN)
      F = 0.0
      XX = 1.0
      DO J = 1, NPOL
        F = F + XX * C(L+J+3)
        XX = XX * X
      END DO
      SADIST = F                             ! drift distance value
C
      RETURN
      END

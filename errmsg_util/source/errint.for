      SUBROUTINE ERRINT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization routine executed as first user
C-                         routine is called.
C-
C-   Inputs  : NONE
C-
C-   Outputs : WARN, LULOG, default max counts for logging, warning devices 
C-              NENTRY = 0
C-
C-   Created  18-DEC-1988   Jun-Jang Jeng (MSU)
C-   Updated   3-JAN-1989   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I
      LOGICAL USED
      INCLUDE 'D0$ERRMSG_UTIL$PARAMS:ERRMSG.PARAMS'
      INCLUDE 'D0$INC:ERRCTL.INC'
      INCLUDE 'D0$INC:ERRCNT.INC'
      DATA USED/.FALSE./
C
      IF (.NOT.USED) THEN
        USED = .TRUE.
        LULOG = 6
        WARN = .TRUE.
        LOGDFL = LARGE
        WRNDFL = LARGE
        NENTRY = 0
      ENDIF
C
  999 RETURN
      END

      SUBROUTINE CPUTIM (N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display CPU time using the formula
C-
C-                         CPU = (CPU1-CPU0)/N in seconds.
C-
C-                         where CPU0 is the value when N = 0 and CPU1
C-                         is the value when N > 0. N could be for example
C-                         the number of loops. This routine calls TIMEX.
C-
C-   Inputs  : N           N = 0 ==> Reset; N > 1 Display.
C-   Outputs : None
C-
C-   Created  22-JUN-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N
      REAL    CPU0,CPU1,CPU
      SAVE    CPU0
      DATA    CPU0/0.0/
C----------------------------------------------------------------------
      IF ( N .LE. 0 ) THEN
        CALL TIMEX (CPU0)
      ELSE
        CALL TIMEX (CPU1)
        CPU = (CPU1-CPU0)/N
        WRITE(6,100) CPU
  100   FORMAT(1X,/,' CPUtime: ',1PE10.3,' seconds'/)
      ENDIF
  999 RETURN
      END

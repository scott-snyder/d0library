      FUNCTION UDST_FIN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finish up analysis
C-
C-   Returned value  : True if OK
C-
C-   Created   18-JUL-1993   Ulrich Heintz, Meenakshi Narain, V. Balamurali
C-   Updated  10-JUL-1994   Ulrich Heintz  remove call to UDST_OUT_CLOSE, now
C-                                                only dummy routine 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL UDST_FIN
C----------------------------------------------------------------------
      UDST_FIN = .TRUE.
  999 RETURN
      END

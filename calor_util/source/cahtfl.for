
      SUBROUTINE CAHTFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  CALL BKCAHT to do the work
C-      Book and fill CAHT (cal. hits header) bank
C-      NOTE: a new bank is created everytime it is called.
C-            It is up to the caller to decide whether he wants a
C-            new bank or not.
C-
C-   Created  17-JAN-1989   Serban D. Protopopescu
C-   Updated   6-MAY-1989   A.M.Jonckheere  Keep For backward compatibility
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LCAHT
C--------------------------------------------------------------
      CALL BKCAHT(LCAHT)
  999 RETURN
      END

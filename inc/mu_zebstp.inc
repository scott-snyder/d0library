C----------------------------------------------------------------------
C-
C-      This include file needs to be used with ZEBSTP.INC.  It sets
C-      specific names for the first seven spare links LMU(1) - LMU(7)
C-      This gives us named links for each bank under SMUO
C-
C-
C-   Created   4-JUN-1991   J.Green
C-   Modified    Apr-92     J.Green   Added LMDFH
C-   Modified    Oct-93     J.Green   Added LMSGH, LMSTH
C-
C----------------------------------------------------------------------
      INTEGER LMSRH, LMMAH, LMDTH, LMBHD, LMDFH, LMSGH, LMSTH
      EQUIVALENCE (LMSRH, LMU(1))
      EQUIVALENCE (LMMAH, LMU(2))
      EQUIVALENCE (LMDTH, LMU(3))
      EQUIVALENCE (LMBHD, LMU(4))
      EQUIVALENCE (LMDFH, LMU(5))
      EQUIVALENCE (LMSGH, LMU(6))
      EQUIVALENCE (LMSTH, LMU(7))

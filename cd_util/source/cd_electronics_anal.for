      LOGICAL FUNCTION CD_ELECTRONICS_ANAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Control analysis of data for CD Electronics 
C-                         Examine
C-
C-   Returned value  : .TRUE.
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  11-OCT-1990   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
C Book histograms
        CALL ZEBKHST
        FIRST = .FALSE.
      END IF
C
C Do analysis and fill histograms
      CALL ZEANAL
C
      CD_ELECTRONICS_ANAL = .TRUE.
C
  999 RETURN
      END

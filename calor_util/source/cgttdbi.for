      SUBROUTINE CGTTDBI
C----------------------------------------------------------------------
C-
C-   CGTTDBI = (Calorimeter)Generate Trigger Tower to 
C-             Data Block Index table.
C-
C-   Purpose and Methods : The above line says most all. The table,
C-                         common block /TTDB/TTBI, is a 2-D table
C-                         which accepts (L1ETAC,l1PHIC) and equivalent
C-                         L-1 DBI. The mapping is one-to-one
C-
C-   Inputs  : None.
C-
C-   Outputs : Fills in common block /TTDB/TTBI.
C-
C-   Controls: None.
C-
C-   Created  12-SEP-1989   Dale A. Ross, MSU
C-
C----------------------------------------------------------------------
*
      IMPLICIT NONE!
*
      INCLUDE 'D0$PARAMS:L1PHP.params/list'
*
C     "Passed" Variable(s):
*
      INCLUDE 'D0$INC:Ttdbi.inc/list'
*
C     Local Variables:
*
      INTEGER L1ETAC,L1DBI,L1PHIC
*
C     =========================================================
*
      DO L1ETAC = -NETAL1,NETAL1
        DO L1PHIC = 1,NPHIL1
          CALL CTTDBI(L1ETAC,L1PHIC,L1DBI)
          TTDBI(L1ETAC,L1PHIC) = L1DBI
C-                        (range of L1DBI is [0,2*NPHIL1*NETAL1-1] )
        END DO
      END DO
*
  999 RETURN
      END

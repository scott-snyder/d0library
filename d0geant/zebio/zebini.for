      SUBROUTINE ZEBINI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize the D0 Zebra Structure /ZEBCOM/
C-
C-   Inputs  : NONE
C-   Outputs : Zebra Header banks
C-
C-   Created   8-JUL-1987   A.M.Jonckkhere
C-   Updated   2-MAR-1988   Ghita Rahal-Callot  : Add run # in LHEAD
C-   Updated   1-JUN-1988   Ghita Rahal-Callot  : Open IRDUNI in READONLY
C-                              in order to be shared
C-   Updated  20-SEP-1989   Alan M. Jonckheere  : Take out Begin Run Record
C-                              read, put it into RDZEB 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
C  Initializing store in /ZEBCOM/ common...
C
      CALL INZCOM(0)
C
  999 CONTINUE
      END

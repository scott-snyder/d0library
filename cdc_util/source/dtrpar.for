      FUNCTION DTRPAR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : initialize parameters for this run
C-
C-   Inputs  :
C-   Outputs :  .FALSE. to skip this run
C-
C-   Created  18-JAN-1988   Olivier Callot
C-   Updated  27-JUL-1988   Qizhong Li     to book user own histograms
C-   Updated  22-MAR-1989   Qizhong Li-Demarteau  use SRCP
C-   Updated  12-JUL-1989   Qizhong Li-Demarteau  from CDCPAR to DTRPAR 
C-   Updated  15-AUG-1989   Qizhong Li-Demarteau  separate hist by sectors 
C-   Updated  10-APR-1990   Qizhong Li-Demarteau  added hist for trigger 
C-   Updated   5-JUN-1990   Qizhong Li-Demarteau  put histogram booking into
C-                                              DHBOOK and call it once only 
C-   Updated  13-SEP-1990   Qizhong Li-Demarteau  rename DHBOOK to DTRHBK to
C-                                        avoid duplicate names in D0library
C-   Updated   1-NOV-1991   Qizhong Li-Demarteau  moved LOGICAL from routine
C-                                         name to Type Declaration Statement
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL DTRPAR
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C   read STP file
C
      CALL CDINIT
C
C   book histograms
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL DTRHBK
      ENDIF
C
      DTRPAR = .TRUE.
  999 RETURN
      END

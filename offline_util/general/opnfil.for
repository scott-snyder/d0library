      SUBROUTINE OPNFIL ( LUNIN, FILNAM, IERR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Open an UNFORMATTED, READONLY file
C-
C-   Inputs  :
C-   Outputs :
C-
C-   Created  20-MAY-1988   Ghita Rahal-Callot
C-   Updated  28-OCT-1988   Ghita Rahal-Callot  : added the flavor to handle
C-                                                machine dependency 
C-   Updated  11-Mar-1992   Herbert B. Greenlee
C-      Change OPEN to D0OPEN.  Got rid of machine block.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IERR, LUNIN
      CHARACTER*(*) FILNAM
      LOGICAL OK
C----------------------------------------------------------------------
      IERR = 0
C
C ****  Open the file FILNAM
C
      CALL D0OPEN (LUNIN, FILNAM, 'IU', OK)
      IF(OK)GO TO 999
      IERR=1
  999 RETURN
      END

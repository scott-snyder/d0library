C DEC/CMS REPLACEMENT HISTORY, Element NEXTID.FOR
C *1    26-SEP-1987 16:35:57 RAJA "From JL"
C DEC/CMS REPLACEMENT HISTORY, Element NEXTID.FOR
      INTEGER FUNCTION NEXTID()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      get next free HBOOK ID
C-
C-   Inputs  :
C-      none
C-   Outputs :
C-      NEXTID      an unused HBOOK ID
C-
C-   Created  21-AUG-1987   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      LOGICAL HEXIST

      INTEGER IDOLD
      DATA IDOLD /0/

C----------------------------------------------------------------------
      NEXTID = IDOLD + 1

      DO WHILE (HEXIST(NEXTID))
        NEXTID = NEXTID + 1
      ENDDO

      IDOLD = NEXTID        !Start there next time

  999 RETURN
      END

C DEC/CMS REPLACEMENT HISTORY, Element INIHBK.FOR
C *1    11-MAY-1988 10:55:30 HARRY "Initialize HBOOK (calls HLIMIT with 100000 fullwords)"
C DEC/CMS REPLACEMENT HISTORY, Element INIHBK.FOR
      SUBROUTINE INIHBK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize HBOOK common
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  17-FEB-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER HMEMOR,NHBOOK
      PARAMETER( NHBOOK = 100000 )
      COMMON // HMEMOR(NHBOOK)
C----------------------------------------------------------------------
      CALL HLIMIT(NHBOOK)       ! Allocate storage for HBOOK
      RETURN
      END

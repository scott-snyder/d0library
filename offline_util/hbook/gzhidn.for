      FUNCTION GZHIDN(IDN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return address of histogram with id IDN. 
C-
C-   Returned value  : Address
C-   Inputs  : IDN  [I] Histogram Id.
C-
C-   Created  11-JAN-1992   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      INTEGER IDN
      INTEGER GZHIDN
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:HCBOOK.INC'
C----------------------------------------------------------------------
      CALL HGNPAR(IDN,'GZHIDN')
      GZHIDN = LCIDN
  999 RETURN
      END

      SUBROUTINE PARHFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Book and fill PARH (particles header) bank
C-      will only make one if it doesn't exist
C-      
C-   Created  17-JAN-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LPARH,GZPARH
C----------------------------------------------------------------------
C
      IF(GZPARH().EQ.0) CALL BKPARH(LPARH)
  999 RETURN
      END

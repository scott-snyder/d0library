      SUBROUTINE CNUMCH(NCH1,NCH2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Find number of calorimeter channels with data 
C-  
C-   Outputs : 
C-     NCH1= number of channels from CAD1
C-     NCH2= number of channels from CAD2
C-
C-   Created  22-JAN-1989   Serban D. Protopopescu
C-   Updated   8-MAR-1990   Chip Stewart  - use GTCAD_TOTAL
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NCH1,NCH2
      INTEGER IER 
C----------------------------------------------------------------------
C
      CALL GTCAD_TOTAL(1,NCH1,IER)
      IF(IER.NE.0) CALL ERRMSG('CAHITS','CAEPFL','CAD1 TOTAL BAD','W')
      CALL GTCAD_TOTAL(2,NCH2,IER)
      IF(IER.NE.0) CALL ERRMSG('CAHITS','CAEPFL','CAD2 TOTAL BAD','W')
  999 RETURN
      END

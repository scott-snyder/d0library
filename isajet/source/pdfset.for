      SUBROUTINE PDFSET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       dummy to avoid loading PDFLIB
C-
C-   Created  20-SEP-1994   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      ENTRY PFTOPDG
      CALL ERRMSG('PDFLIB not available','PDFSET',
     &  'Use package ISAGEN_PDF to get PDFLIB','F')
  999 RETURN
      END

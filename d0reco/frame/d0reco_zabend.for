      SUBROUTINE ZABEND
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      crash program if a ZEBRA error exit occurs
C-
C-   Created  17-JUN-1992   Serban Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL ERRMSG('Zebra crash in D0RECO','ZABEND',' ','F')
  999 RETURN
      END

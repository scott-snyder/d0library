      SUBROUTINE MERGE_PELC_PPHO_CHAINS(LCLUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Merge linear chains of PELCs and PPHOs and 
C-                         sort by Et
C-              
C-                         Never actually use this routine for anything,
C-                         it's dangerous ZEBRA tomfoolery
C-
C-   Inputs  : NONE
C-   Outputs : LCLUS - link to first bank in the PELC/PPHO chain 
C-   Controls: NONE
C-
C-   Created   1-JUN-1995   Ian Adam
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LPPHO,GZPPHO,LPARH,GZPARH,LPELC,LCLUS
C----------------------------------------------------------------------
      LPPHO = GZPPHO()
      LPARH = GZPARH()
      IF (LPPHO.GT.0) CALL ZSHUNT(0,LPPHO,LPARH,-2,1)
      CALL SORT_PELC(LPELC)
      LCLUS = LPELC

  999 RETURN
      END

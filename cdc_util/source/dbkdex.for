      SUBROUTINE DBKDEX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Book histogram for DE/DX
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: called by DTRDIA
C-             ENTRY point: DTSDEX(HSTDEX)
C-
C-   Created  11-SEP-1990   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL YES, HSTDEX
C----------------------------------------------------------------------
C
      YES=.FALSE.
      CALL GETPAR(1,' Book histogram for DE/DX? Y/N>','L',YES)
      IF(.NOT.YES) GOTO 999
C
      CALL HBOOK1(1110,' DE/DX $',100,0.0,5.0,0.)
C
 999  RETURN
C---------------------------------------------------------------------------
      ENTRY DTSDEX(HSTDEX)
      HSTDEX = YES
      RETURN
      END

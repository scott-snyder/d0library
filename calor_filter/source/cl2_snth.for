      FUNCTION CL2_SNTH(IETA,IPHI,LYR,ZVTX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  calculate sin theta of a cell wrt vtx
C-
C-   Returned value  : the sine
C-   Inputs  : IETA,IPHI,LYR Physics cal cell indices
C-             ZVTX          Z of the vertex
C-   Controls: none
C-
C-   Created  23-JUL-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IETA,IPHI,LYR,IER
      REAL    CL2_SNTH,ZVTX,XCELL,YCELL,ZCELL
      LOGICAL CEXIST
C----------------------------------------------------------------------
      IF (CEXIST(IETA,IPHI,LYR) ) THEN
        CALL CELXYZ(IETA,IPHI,LYR,XCELL,YCELL,ZCELL,IER)
        CL2_SNTH = 1.0/SQRT(1.0+(ZVTX-ZCELL)**2/(XCELL**2+YCELL**2))
      ELSE
        CL2_SNTH = 0.0  !nonexistent cell
      ENDIF
  999 RETURN
      END

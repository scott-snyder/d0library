      SUBROUTINE CELL_ENERGY(IETA,IPHI,ILYR,ENERGY,ET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : given physics indices of cell
C-                         returns energy and ET of cell
C-
C-   Inputs  : IETA,IPHY,ILYR - Physics indices
C-   Outputs : ENERGY,ET cell energies and ET
C-   Controls:
C-
C-   Created  12-OCT-1989   Rajendran Raja
C-   Updated  18-MAY-1995   Chip Stewart  - use GTCAEH_ADDR  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IETA,IPHI,ILYR,STAT,IER
      REAL    ENERGY,ET,Ex,Ey,Ez,SGX,SGY,CW
      CHARACTER*80 MSG
C----------------------------------------------------------------------
      ENERGY=0.
      ET=0.
      CALL GTCAEH_ADDR(IETA,IPHI,ILYR,Ex,Ey,Ez,ENERGY,ET,SGX,SGY,
     &  CW,STAT,IER)
      IF (IER .NE. 0) THEN
        WRITE(MSG,10)IETA,IPHI,ILYR,IER
   10   FORMAT('Cannot get Energy and Et: ETA PHI LYR IER ',4I5)
        CALL ERRMSG(' GTCAEH_ADDR ERROR','CELL_ENERGY',MSG,'W')
      ENDIF
  999 RETURN
      END

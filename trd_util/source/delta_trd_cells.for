      INTEGER FUNCTION DELTA_TRD_CELLS (CELL1,CELL2,LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : computes the difference between 2 TRD cells
C-              examples : delta_trd_cells (1,3)=2
C-                         delta_trd_cells (3,1)=2
C-                         delta_trd_cells (1,254)=3   in layer 1 or 2
C-                         delta_trd_cells (1,254)=253 in layer 3
C-   Inputs  : cell1,cell2 in [1..256]
C-             layer       in [1..3] 
C-   Controls: none
C-
C-   Created  10-JUN-1993   Alain PLUQUET
C-   Updated  25-NOV-1993   Alain PLUQUET  
C----------------------------------------------------------------------
      INTEGER CELL1,CELL2,D,LAYER,NUMBER_OF_WIRES(6)
      CALL GET_TRD_NBW(NUMBER_OF_WIRES)        
      IF   (CELL1.GT.NUMBER_OF_WIRES(LAYER).OR.CELL1.LT.0
     &  .OR.CELL2.GT.NUMBER_OF_WIRES(LAYER).OR.CELL2.LT.0.) THEN
        CALL ERRMSG
     &      (' DELTA_TRD_CELLS','DELTA_TRD_CELLS',
     &      'WRONG CELLS NUMBERS','W')
        DELTA_TRD_CELLS=999        
      ELSE
        D=ABS(CELL1-CELL2)
        IF (D.GT.NUMBER_OF_WIRES(LAYER)/2) D=NUMBER_OF_WIRES(LAYER)-D
        DELTA_TRD_CELLS=D
      ENDIF
      END

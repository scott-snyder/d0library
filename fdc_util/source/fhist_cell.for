      FUNCTION FHIST_CELL(HALF,UNIT,QUAD,SECT)
C----------------------------------------------------------------------
C
C-
C-   Purpose and Methods : Determine whether requested HALF,UNIT,QUAD,
C-                         SECT should be histogrammed using FDEXST and
C-                         HIST_CELLS in RCP.
C-                         If no cells listed in RCP, return true if read out.
C-                         Otherwise, return true only if selected in RCP file.
C-
C-                         Format of RCP array (e.g.):
C-!       half    Unit    Quad    Sect
C- \ARRAY HIST_CELLS      16
C-        0       0       3       3
C-        0       0       3       4
C-        0       0       4       4
C-        0       1       0       34
C- \END
C-
C-   Returned value  : True if read out
C-   Inputs  : HALF = Forward/Backward Chamber
C-             UNIT = Phi/Theta
C-             QUAD = Theta Quadrant, irrelevant for Phi
C-             SECT = Theta/Phi sector Number
C-   Outputs : none
C-   Controls: none
C-
C-   Created  25-JUL-1991   Susan K. Blessing  Based on old FDEXST.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER HALF,UNIT,QUAD,SECT
      INTEGER IER
      INTEGER CELL,N_CELLS,N_VAL
C
      INTEGER MAX_CELL
      PARAMETER( MAX_CELL =  20)
      INTEGER HIST_HALF(MAX_CELL)
      INTEGER HIST_UNIT(MAX_CELL)
      INTEGER HIST_QUAD(MAX_CELL)
      INTEGER HIST_SECT(MAX_CELL)
C
      LOGICAL FHIST_CELL
      LOGICAL FDEXST
      LOGICAL FIRST
      LOGICAL OK
C
      SAVE FIRST,N_VAL,HIST_HALF,HIST_UNIT,HIST_QUAD,HIST_SECT,N_CELLS
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('FDC_RCP')
        CALL EZGETA ('HIST_CELLS',0,0,0,N_VAL,IER)
        CALL EZGETA ('HIST_CELLS',1,N_VAL,4,HIST_HALF(1),IER)
        CALL EZGETA ('HIST_CELLS',2,N_VAL,4,HIST_UNIT(1),IER)
        CALL EZGETA ('HIST_CELLS',3,N_VAL,4,HIST_QUAD(1),IER)
        CALL EZGETA ('HIST_CELLS',4,N_VAL,4,HIST_SECT(1),IER)
        CALL EZRSET
        N_CELLS = N_VAL/4
      ENDIF
C
      OK = FDEXST(HALF,UNIT,QUAD,SECT)
C
      IF (OK.AND.N_CELLS.GT.0) THEN
C
        OK = .FALSE.
C
        DO CELL =  1, N_CELLS
          IF ( (HALF .EQ. HIST_HALF(CELL)) .AND.
     &         (UNIT .EQ. HIST_UNIT(CELL)) .AND.
     &         (SECT .EQ. HIST_SECT(CELL)) .AND.
     &         (QUAD .EQ. HIST_QUAD(CELL)) ) THEN
            OK = .TRUE.
          ENDIF
        ENDDO
      ENDIF
C
      FHIST_CELL = OK
C
C----------------------------------------------------------------------
  999 RETURN
      END

      SUBROUTINE FDC_SHIFT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Adjust positions of wires along z according
C-              to RCP parameters.
C-
C-    Example (move thetas away from phi by 0.5 cm):
C-
C- \ARRAY SHIFT_Z
C-!    half    layer   z_shift
C-      0       0        0.5
C-      0       1       -0.5
C-      1       0       -0.5
C-      1       1        0.5
C- \END
C-
C-   Created  19-JUL-1991   Robert E. Avery
C-   Updated  20-AUG-1992   Robert E. Avery  Allow shift to depend on
C-   wire number. New RCP array, SHIFT_Z_WIRE, is multiplier for each wire.
C-   Updated  17-FEB-1993   Robert E. Avery   Make sure shift is only 
C-      applied once (for this set of banks).
C-   Updated  14-APR-1993   Robert E. Avery  Set UNIT number (bug fix). 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$LINKS:IZFALH.LINK'
C
      INTEGER CELL,N_CELLS,N_VAL
      INTEGER HALF,LAYER,UNIT,QUAD,SECT,WIRE
      INTEGER IER,STATUS
      INTEGER MAXQUAD, MINQUAD, MAXSEC
      INTEGER NMWIRE, NMPARW ,IPTR
      INTEGER GZFASE, LKFASE
      INTEGER LKFALH
      INTEGER N_VAL_WIRE
      INTEGER MAX_CELL
      PARAMETER( MAX_CELL =  6)
      INTEGER SHIFT_HALF(MAX_CELL)
      INTEGER SHIFT_LAYER(MAX_CELL)
      REAL    Z_SHIFT(MAX_CELL)
      REAL    Z_SHIFT_WIRE(0:15)
      LOGICAL SHIFT_FDC
C----------------------------------------------------------------------
      CALL EZPICK('FTRAKS_RCP')
      CALL EZGET_l('SHIFT_FDC',SHIFT_FDC,IER)
      CALL EZGETA_i ('SHIFT_Z',0,0,0,N_VAL,IER)
      CALL EZGETA_iarr ('SHIFT_Z',1,N_VAL,3,SHIFT_HALF,IER)
      CALL EZGETA_iarr ('SHIFT_Z',2,N_VAL,3,SHIFT_LAYER(1),IER)
      CALL EZGETA ('SHIFT_Z',3,N_VAL,3,Z_SHIFT(1),IER)
      CALL EZGETA_i ('SHIFT_Z_WIRE',0,0,0,N_VAL_WIRE,IER)
      CALL EZGET  ('SHIFT_Z_WIRE',Z_SHIFT_WIRE,IER)
      CALL EZRSET
C
      IF (.NOT.SHIFT_FDC)  GOTO 999
      IF ( N_VAL.LT.3 ) GOTO 999
C
      LKFALH=LC(LSFDC-IZFALH)
      IF(LKFALH.LE. 0) GOTO 999
C
C Make sure shift is only applied once (for this set of banks).
C
      STATUS=IC(LKFALH)
      IF ( BTEST(STATUS,0) ) GOTO 999
      IC(LKFALH)=IBSET(STATUS,0)
C
C Do shift:
C
      N_CELLS = N_VAL/3
      DO CELL =  1, N_CELLS
        HALF = SHIFT_HALF(CELL)
        LAYER = SHIFT_LAYER(CELL)
        UNIT = LAYER/2
        IF ( LAYER .EQ. 0 ) THEN
          MAXSEC = MXSECT
          MINQUAD = 0
          MAXQUAD = 3
        ELSEIF( LAYER .EQ. 1 ) THEN
          MAXSEC = MXSECT
          MINQUAD = 4
          MAXQUAD = 7
        ELSE
          MAXSEC = MXSECP
          MINQUAD = 0
          MAXQUAD = 0
        ENDIF
        DO  QUAD=  MINQUAD, MAXQUAD
          DO  SECT=  0, MAXSEC
            LKFASE=GZFASE(HALF,UNIT,QUAD,SECT)
            NMWIRE = IC(LKFASE+3)
            NMPARW = IC(LKFASE+4)
            DO WIRE =  0, NMWIRE-1
              IPTR = LKFASE + WIRE*NMPARW   + 6
              IF ( N_VAL_WIRE.GT.0 ) THEN
                C(IPTR+3) = C(IPTR+3)
     &            + Z_SHIFT(CELL)*Z_SHIFT_WIRE(WIRE)
              ELSE
                C(IPTR+3) = C(IPTR+3)  + Z_SHIFT(CELL)
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
  999 RETURN
      END

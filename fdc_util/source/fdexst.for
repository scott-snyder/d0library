      FUNCTION FDEXST(HALF,UNIT,QUAD,SECT)
C----------------------------------------------------------------------

C-
C-   Purpose and Methods : Determine whether requested HALF,UNIT,QUAD,
C-                         SECT is read out.  (LOGICAL address)
C-  
C-   Returned value  : True if read out
C-   Inputs  : HALF = Forward/Backward Chamber
C-             UNIT = Phi/Theta
C-             QUAD = Theta Quadrant, irrelevant for Phi
C-             SECT = Theta/Phi sect Number
C-   Outputs : none
C-   Controls: none
C-
C-   Created  18-JAN-1990   Srini Rajagopalan
C-   Updated  15-JUN-1990   Robert E. Avery  (add RCP selection) 
C-   Updated  26-APR-1991   Jeffrey Bantly  fetch hist_cells from FDC.RCP
C-   Updated  25-JUL-1991   Susan K. Blessing  Remove HIST_CELLS and 
C-    associated stuff.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER HALF,UNIT,QUAD,SECT
      INTEGER LKFPSE
      INTEGER GZFPSE
      INTEGER NRUN,RUNNO
C
      LOGICAL FDEXST
C
C----------------------------------------------------------------------
C
      FDEXST = .FALSE.
C
C  *** Find if Channel is out of range.
C
      IF (HALF.LT.0.OR.HALF.GT.1) GO TO 999
      IF (UNIT.LT.0.OR.UNIT.GT.1) GO TO 999
      IF (UNIT.EQ.0) THEN
        IF (QUAD.LT.0.OR.QUAD.GT.7) GO TO 999
        IF (SECT.GT.5) GO TO 999
      ELSE
        IF (SECT.GT.35) GO TO 999
      ENDIF
C
      LKFPSE = GZFPSE(HALF,UNIT,QUAD,SECT)
C
      NRUN = RUNNO()
      IF ( IC(LKFPSE+1).LT.NRUN ) THEN
         FDEXST = .TRUE.
      ENDIF
C
      IF (IC(LKFPSE+1).EQ.99999998) FDEXST = .FALSE.
C----------------------------------------------------------------------
  999 RETURN
      END

      SUBROUTINE FILL_VDTM(LAYER,CATEG,LUN_DTM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill VDTM bank with distance-time map look
C-               up tables
C-
C-   Inputs  : LAYER   -- usual, starting with 0
C-             CATEG   -- category of time-distance map
C-             LUN_DTM -- Logical unit number of DTM input file
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-JUN-1990   ED OLTMAN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVTMW.LINK'
      INCLUDE 'D0$LINKS:IZVDTM.LINK'
c I/O:
      INTEGER LAYER,CATEG,LUN_DTM
c Locals:
      INTEGER LVTMW,LVDTM,START,NUMB,I
      REAL SCALE
      DATA SCALE / 1.0 /
c Externals:
      INTEGER GZVTMW
C----------------------------------------------------------------------
      LVTMW = GZVTMW(LAYER)
      IF (LVTMW .EQ. 0) THEN
        CALL ERRMSG('No VTMW bank', 'FILL_VDTM',
     &    'Supporting bank not present', 'W')
        GO TO 999
      ENDIF
      LVDTM = LC(LVTMW - (IZVDTM+CATEG) )
      IF (LVDTM .EQ. 0) THEN
        CALL ERRMSG('VDTM not booked', 'FILL_VDTM',
     &    'Attempt to fill unbooked bank', 'W')
        GO TO 999
      ENDIF
      C(LVDTM+6) = SCALE                ! v_drift-(E/P) scale factor
c..read in wire positions
      START = LVDTM + 7
      NUMB  = 2*IC(LVDTM+4)
      READ(LUN_DTM) (C(I),I=START,START+NUMB-1)
c..read in pointers and word counts
      START= START + NUMB
      NUMB = 4*IC(LVDTM+3)*IC(LVDTM+4)
      READ(LUN_DTM) (IC(I),I=START,START+NUMB-1)
c..read in parameters 
      START = START + NUMB
      NUMB  = 2*IC(LVDTM+5)
      READ(LUN_DTM) (C(I),I=START,START+NUMB-1)
  999 RETURN
      END

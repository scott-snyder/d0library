      SUBROUTINE JT2ALL(NAM, PX, PY, SX, SY, ROT, TX, TY)
C
C    Purpose:
CD   The purpose of this module is to apply a complete 2D image
CD   transformation.  The parameters passed are the NAME of the retained
CD   segment, Point of origin (PX, PY) for the scaling, rotation, and
CD   translation; The scaling factors (SX, SY); The rotation angle (ROT)
CD   in radian counterclockwise about the positive Z-axis; And the
CD   translation components (TX, TY).  All parameters are real with the
CD   exception of segment name.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 23-Jan-1989
C
CH   History:
CH      15-FEB-89  SA   Correct translation scales.
CH      15-FEB-89  SA   Routine KTRUPD was added to do translations.
CH      25-JAN-89  SA   Call to PDI preceded by PPURGE.
CH      24-JAN-89  SA   Limits on PX,PY,TX,TY correctly scaled.
CH      23-JAN-89  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-R
C
C    Calls:
CC      ERROR.
C
      EXTERNAL ERRHND
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER NAM
      REAL PX, PY, SX, SY, ROT, TX, TY
C
C    Then local declarations of variables (non-common variables).
C
      INTEGER I, LOCAT, TTYPE, ITRT, IRST
      REAL CNV, EPS, TTRV(3), TSCV(3)
      REAL SPX, SPY, STX, STY
      CHARACTER*4 SSTR
      CHARACTER*7 NODEN
      CHARACTER*2 TRNAM1, TRNAM2, TRNAM3
      DATA CNV, EPS/57.2957795, 1.0E-9/
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
C
      IF(CTTYPE .LT. 1) GO TO 999
C
C    Then executable code follows
      IF (SEGOPN) THEN
        CALL ERROR('JT2ALL: A SEGMENT IS OPEN')
      ENDIF
      LOCAT = 0
      DO 10 I=1,NSEGS
        IF (SEGINF(1,I) .EQ. NAM) LOCAT = I
   10 CONTINUE
      IF (LOCAT .EQ. 0) THEN
        CALL ERROR('JT2ALL: NAME DOES NOT EXIST')
      ENDIF
      TTYPE = SEGINF(2,LOCAT) .AND. 3
      IF (TTYPE .NE. 2) THEN
        CALL ERROR('JT2ALL: 2D IMAGE TRANS. NOT ALLOWED ON NAME')
      ENDIF
C
C       Scaled translations:
C
      SPX = PX * SCV(1)
      SPY = PY * SCV(2)
      STX = TX * SCV(1)
      STY = TY * SCV(2)
C
      IF (ABS(SPX) .GT. 1.0 .OR. ABS(SPY) .GT. 1.0) THEN
        CALL ERROR('JT2ALL: TRANSF ORIGIN (PX, PY) OUT OF RANGE')
      ENDIF
      IF (ABS(STX) .GT. 1.0 .OR. ABS(STY) .GT. 1.0) THEN
        CALL ERROR('JT2ALL: TRANSLATION (TX, TY) OUT OF RANGE')
      ENDIF
      IF (SX .LE. 0.0 .OR. SX .GT. 100.0 .OR.
     +    SY .LE. 0.0 .OR. SY .GT. 100.0) THEN
        CALL ERROR('JT2ALL: SCALE FACTORS (SX, SY) OUT OF RANGE')
      ENDIF
C
      CALL KBLDN(SEGINF(6,LOCAT), SSTR)
      SSTR = 'R' // SSTR(1:3)
      ITRT = 0
C
C  TRANSLATE
      TTRV(1) = TX
      TTRV(2) = TY
      TTRV(3) = 0.0
      NODEN = SSTR//'.IT'
      TRNAM3 = 'TT'
      IRST = 0
      CALL KTRUPD(TTRV, NODEN, TRNAM3, IRST)
C
      IF(CTTYPE .EQ.2) THEN
C  CHANGE OF ORIGIN
        TTRV(1) = -PX
        TTRV(2) = -PY
        TTRV(3) = 0.0
        NODEN = SSTR//'.IN'
        TRNAM1 = 'TN'
        IRST = 1
        CALL KTRUPD(TTRV, NODEN, TRNAM1, IRST)
C  SCALE
        TSCV(1) = SX
        TSCV(2) = SY
        TSCV(3) = 1.0
        CALL PFN('SC', 'SCALE', ERRHND)
        CALL PCONN('SC', 1, 1, SSTR//'.IS"', ERRHND)
        CALL PSNV3D(TSCV, 1, 'SC', ERRHND)
C  ROTATE
        CALL PFN('ZR', 'ZROTATE', ERRHND)
        CALL PCONN('ZR', 1, 1, SSTR//'.IRZ"', ERRHND)
        CALL PSNREA(-ROT * CNV, 1, 'ZR', ERRHND)
C  CHANGE ORIGIN BACK
        TTRV(1) = PX
        TTRV(2) = PY
        TTRV(3) = 0.0
        NODEN = SSTR//'.IP'
        TRNAM2 = 'TP'
        IRST = 1
        CALL KTRUPD(TTRV, NODEN, TRNAM2, IRST)
      ENDIF
C
C      CALL KFRAM
      CALL PPURGE(ERRHND)
C
      IF (ABS(PX) .LT. EPS .AND. ABS(PY) .LT. EPS .AND.
     +    ABS(ROT) .LT. EPS .AND. ABS(SX-1.0) .LT. EPS .AND.
     +    ABS(SY-1.0) .LT. EPS .AND. ABS(TX) .LT. EPS .AND.
     +    ABS(TY) .LT. EPS) THEN
        CALL PDI(TRNAM1//'XV', 1, 1, TRNAM1//'AC', ERRHND)
        CALL PDI(TRNAM1//'YV', 1, 1, TRNAM1//'AC', ERRHND)
        CALL PDI(TRNAM1//'ZV', 1, 1, TRNAM1//'AC', ERRHND)
        CALL PDI(TRNAM1//'AC', 1, 1, SSTR//'.IN"', ERRHND)
        CALL PDI(TRNAM2//'XV', 1, 1, TRNAM2//'AC', ERRHND)
        CALL PDI(TRNAM2//'YV', 1, 1, TRNAM2//'AC', ERRHND)
        CALL PDI(TRNAM2//'ZV', 1, 1, TRNAM2//'AC', ERRHND)
        CALL PDI(TRNAM2//'AC', 1, 1, SSTR//'.IP"', ERRHND)
C
        CALL PDI('SC', 1, 1, SSTR//'.IS"', ERRHND)
C
        CALL PDI('ZR', 1, 1, SSTR//'.IRZ"', ERRHND)
C
        CALL PDI(TRNAM3//'XV', 1, 1, TRNAM3//'AC', ERRHND)
        CALL PDI(TRNAM3//'YV', 1, 1, TRNAM3//'AC', ERRHND)
        CALL PDI(TRNAM3//'ZV', 1, 1, TRNAM3//'AC', ERRHND)
        CALL PDI(TRNAM3//'AC', 1, 1, SSTR//'.IT"', ERRHND)
        CALL PPURGE(ERRHND)
      ENDIF
C
  999 CONTINUE
      RETURN
      END

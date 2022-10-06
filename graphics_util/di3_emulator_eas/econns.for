      SUBROUTINE ECONNS (NINTEG, ILIST, NREAL, RLIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To connect segments to the transformation
C-                         network.
C-
C-   Inputs  :NINTEG, ILIST, NREAL, RLIST
C-   Outputs :NONE
C-   Controls:NONE
C-
C-   Created   08-FEB-1989   SHAHRIAR ABACHI
C-   UPDATED   01-FEB-1990   SHAHRIAR ABACHI   new emulator stuff added
C-   UPDATED   15-JUN-1990   SHAHRIAR ABACHI   Traslation bug fixed
C-   UPDATED   11-AUG-1992   SHAHRIAR ABACHI   fine scale added to knob 8
C-   UPDATED   02-DEC-1992   NOBUAKI  OSHIMA   
C-      Use RLIST(10) to adjust the coarse scale knob.
C    Updated  24-MAR-2004   sss - compile with g77. (fix bug)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  NINTEG, ILIST(*), I, ISEG, NREAL
      REAL RLIST(*)
      CHARACTER*4 TRNSF(50)
      COMMON /ESCAPC/ TRNSF
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:NEWDI3.INC/LIST'
      EXTERNAL ERRHND
      REAL  EYEPN(3), UPV(3), VUPN(3), TVEC(3), DIST
      REAL DN, ANG, DW, PI, EPS, SFAC
      CHARACTER*4 SEGN, SSTR
      LOGICAL COUNT
      DATA PI, EPS /3.141593, 1.0E-9/
C------------------
C
      IF(NUDI3) THEN
C
        CALL KFUN
        CALL KFUNC
        CALL PCONN(TRNSF(1), 1, 1, DISPL//'.IRX"', ERRHND)
        CALL PCONN(TRNSF(2), 1, 1, DISPL//'.IRY"', ERRHND)
        CALL PCONN(TRNSF(3), 1, 1, DISPL//'.IRZ"', ERRHND)
        CALL PCONN(TRNSF(4), 1, 1, DISPL//'.IS"', ERRHND)
        CALL PCONN(TRNSF(5), 1, 1, DISPL//'.IF"', ERRHND) !NEW
        CALL PCONN('ACCM5', 1, 1, DISPL//'.IT"', ERRHND)
C
      ELSE
C
        CALL PFN(TRNSF(6), 'LOOKAT', ERRHND)
        ISEG = 0
        DO 2 I = 1,NINTEG
          COUNT = .TRUE.
          DO 3 WHILE(COUNT)
          ISEG = ISEG + 1
          IF ( ISEG .GE. NSEGS) COUNT = .FALSE.
          IF ( SEGINF(1,ISEG) .EQ. ILIST(I) ) COUNT = .FALSE.
    3   CONTINUE
        CALL KBLDN(SEGINF(6,ISEG), SSTR)
        SEGN = 'R' // SSTR(1:3)
        CALL PCONN(TRNSF(1), 1, 1, SEGN//'.IRX', ERRHND)
        CALL PCONN(TRNSF(2), 1, 1, SEGN//'.IRY', ERRHND)
        CALL PCONN(TRNSF(3), 1, 1, SEGN//'.IRZ', ERRHND)
        CALL PCONN(TRNSF(4), 1, 1, SEGN//'.IS', ERRHND)
        CALL PCONN(TRNSF(5), 1, 1, SEGN//'.IF', ERRHND)
CC        CALL PCONN(TRNSF(5), 1, 1, SEGN//'.IS', ERRHND) !NEW
        CALL PCONN(TRNSF(6), 1, 1, SEGN//'.IL', ERRHND)
        CALL PCONN('ACCM5', 1, 1, SEGN//'.IT', ERRHND)
    2 CONTINUE
C
      ENDIF
C------------------
      CALL PSNREA(0.0, 2, TRNSF(1), ERRHND)
      CALL PSNREA(200.0, 3, TRNSF(1), ERRHND)
      CALL PSNREA(0.0, 2, TRNSF(2), ERRHND)
      CALL PSNREA(200.0, 3, TRNSF(2), ERRHND)
      CALL PSNREA(0.0, 2, TRNSF(3), ERRHND)
      CALL PSNREA(200.0, 3, TRNSF(3), ERRHND)
C
      SFAC = RLIST(10)
      CALL PSNREA(SFAC, 2, 'MULP3', ERRHND)
      CALL PSNREA(1.0, 2, TRNSF(4), ERRHND)
      CALL PSNREA(0.2, 3, TRNSF(4), ERRHND)
      CALL PSNREA(1000.0, 4, TRNSF(4), ERRHND)
      CALL PSNREA(0.01, 5, TRNSF(4), ERRHND)
C
cc      CALL PSNREA(1.0, 2, TRNSF(5), ERRHND) !NEW
cc      CALL PSNREA(0.2, 3, TRNSF(5), ERRHND) !NEW
cc      CALL PSNREA(1000.0, 4, TRNSF(5), ERRHND) !NEW
cc      CALL PSNREA(0.01, 5, TRNSF(5), ERRHND) !NEW
C
      IF(RLIST(1) .LE. EPS) THEN
        DIST = 1.0E5
      ELSE
        DIST = RLIST(1)
      ENDIF
      DN = SQRT(NORML(1)**2 + NORML(2)**2 + NORML(3)**2)
      DO 4 I=1,3
        EYEPN(I) = VUPNT(I) - DIST * NORML(I) / DN
        UPV(I)   = UPVEC(I)
        VUPN(I)  = VUPNT(I)
        TVEC(I)  = 0.0
    4 CONTINUE
      EYEPN(3) = EYEPN(3) * RIGHT
      UPV(3)   = UPV(3) * RIGHT
      VUPN(3)  = VUPN(3) * RIGHT
C
      CALL PSNV3D(EYEPN, 2, TRNSF(6), ERRHND)
      CALL PSNV3D(UPV, 3, TRNSF(6), ERRHND)
      CALL PSNV3D(VUPN, 1, TRNSF(6), ERRHND)
C
      CALL PSNREA(200.0, 2, 'MULP1', ERRHND)
      CALL PSNREA(200.0, 2, 'MULP2', ERRHND)
CC      CALL PSNREA(5.0, 2, 'MULP3', ERRHND)
      CALL PSNREA(200.0, 2, 'MULP4', ERRHND)
CC      CALL PSNREA(0.0, 2, 'ACCM1', ERRHND)
      CALL PSNREA(0.0, 2, 'ACCM2', ERRHND)
      CALL PSNREA(0.0, 2, 'ACCM3', ERRHND)
      CALL PSNREA(0.0, 2, 'ACCM4', ERRHND)
      CALL PSNV3D(TVEC, 2, 'ACCM5', ERRHND)
      CALL PSNREA(100.0, 4, 'ACCM5', ERRHND)
      CALL PSNREA(100000.0, 5, 'ACCM5', ERRHND)
      CALL PSNREA(-100000.0, 6, 'ACCM5', ERRHND)
C
      IF(ABS(RLIST(2)) .GT. EPS .OR. ABS(RLIST(3)) .GT. EPS) THEN
        DW = ( RLIST(3) - RLIST(2) ) / 2.0
      ELSE
        DW = ( UWIND(2) - UWIND(1) ) / 2.
      ENDIF
      ANG = 2.0 * ATAN2(DW, DIST)
      IF(ANG .LT. 0.0) ANG = ANG + 2.0 * PI
      ANG = ANG * 180.0 / PI
      CALL PSNREA(ANG, 2, TRNSF(5), ERRHND)
      CALL PSNREA(DIST-180.0, 3, TRNSF(5), ERRHND)
      CALL PSNREA(DIST+180.0, 4, TRNSF(5), ERRHND)
      CALL PSNBOO(.TRUE., 1, 'FOVU"', ERRHND)
C
      CALL PSNREA(TVEC(1), 1, TRNSF(7), ERRHND)
      CALL PSNREA(TVEC(1), 1, TRNSF(8), ERRHND)
      CALL PSNREA(TVEC(1), 1, TRNSF(9), ERRHND)
C
      CALL PPURGE(ERRHND)
C
  999 RETURN
      END

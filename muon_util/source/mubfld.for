      SUBROUTINE MUBFLD(QQUAD,X,Y,Z,BX,BY,BZ)
C   ==================================
C
C    ROUTINE TO PRODUCE MAGNETIC FIELD PARAMETERS
C    INPUT IS X,Y,Z POSITION IN GLOBAL COORDINATE(cm)
C    OUTPUT IS MAGNETIC FIELD IN KGAUSS
C     HEDIN 3/6/89   initially somewhat of a kluge
C      D0=hard wired; basement uses field map
C         SET UP FOR D0 GEOMETRY Or BASEMENT (QUAD=0)
C      DH 9/89 fixup basment
C      DH 3/90 fix D0 geometry
C      DH 10/91 QUAD CHANGE, KILL BASEMENT
C      DH 12/91 change geom slightly in central
C         doesn't do corner of EF well
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      REAL RAD,XCORN,YCORN,BCF,BEF,
     3 X,Y,Z,BX,BY,BZ,ZCF,ZEF1,ZEF2,X1,X3
      INTEGER QUAD,QQUAD
      INTRINSIC ABS
C
      DATA ZCF/378.46/         ! Z EDGE OF CF
      DATA ZEF1,ZEF2/439.62,591.62/  ! Z LIMITS OF EF
      DATA XCORN,YCORN/309.88,309.88/  ! X,Y INSIDE CORNERS OF CF
      DATA BCF,BEF/20.,20./         ! MAX B IN CF AND EF
C
      QUAD=MOD(QQUAD,100)
      BX=0.
      BY=0.
      BZ=0.
C
      IF(QUAD.NE.0) THEN
        IF(ABS(Z).LT.ZCF) THEN        ! CF REGION
          IF(ABS(X).GT.XCORN.AND.ABS(X).LT.XCORN+110.) THEN
            IF(ABS(Y).LT.YCORN) BY=BCF*ABS(X)/X   ! VERTICAL SLAB
            IF(ABS(Y).GE.YCORN.AND.ABS(Y).LT.YCORN+110.) THEN
              RAD=SQRT((ABS(X)-XCORN)**2+(ABS(Y)-YCORN)**2)
              BX=-(ABS(Y)-YCORN)/RAD*ABS(Y)/Y*BCF
              BY= (ABS(X)-XCORN)/RAD*ABS(X)/X*BCF
            ENDIF
          ENDIF
          IF(ABS(Y).GT.YCORN.AND.ABS(Y).LT.YCORN+110.) THEN
            IF(ABS(X).LT.XCORN) BX=-BCF*ABS(Y)/Y   ! HORIZ SLAB
            IF(ABS(X).GE.XCORN.AND.ABS(X).LT.XCORN+110.) THEN
              RAD=SQRT((ABS(X)-XCORN)**2+(ABS(Y)-YCORN)**2)
              BX=-(ABS(Y)-YCORN)/RAD*ABS(Y)/Y*BCF
              BY= (ABS(X)-XCORN)/RAD*ABS(X)/X*BCF
            ENDIF
          ENDIF
        ELSE IF(ABS(Z).GT.ZEF1.AND.ABS(Z).LT.ZEF2) THEN
          RAD=SQRT(X**2+Y**2)
          BX=-BEF*Y/RAD
          BY= BEF*X/RAD
        ENDIF
      ENDIF
C
      RETURN
      END

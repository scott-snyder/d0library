C===================================================================
      SUBROUTINE ISZXYZ(NPART,ID,PX,PY,PZ,X,Y,Z,P,THETA,I3D)
C===================================================================
C
C  Description:  Plots ISAJET events in XYZ space (2D)
C  ============
C
C  Author:
C  ========
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - December 28, 1987
C
C====================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ===================
C
      INTEGER SI,I
      PARAMETER(SI=1000)
      INTEGER NPART
      INTEGER ID(SI),I3D
      INTEGER ICHRG,IDCHRG
      REAL PX(SI),PY(SI),PZ(SI)
      REAL X(SI),Y(SI),Z(SI)
      REAL P(SI),THETA(SI)
      REAL XF,YF,ZF,PTOT,LEN,PT
      REAL THMDE,THMIN,PI,PMIN,PTMIN
      REAL WIN2D,WIN3D,DUM,RMAT(4,4)
      CHARACTER*19 MESS1
      LOGICAL NEUTLS,NEUSON,CHAMB
      DATA LEN/80./
      DATA PI/3.1416/
      DATA WIN2D/90./
      DATA WIN3D/160./
      DATA DUM/0./
C
C  Executable Code:
C  ================
C
      IF (I3D .EQ. 1) THEN
         CALL JTRANS(RMAT,8,-65.,DUM,DUM,DUM,DUM,DUM,DUM)
         CALL JBUILD(RMAT,7,10.,DUM,DUM,DUM,DUM,DUM,DUM)
         CALL JMODEL(RMAT)
         CALL JMODON(.TRUE.)
      ENDIF
      CALL PUOPEN
      CALL PUGETV('VTX & CDC SHOWN',CHAMB)
      IF (CHAMB) THEN
         CALL ISZCYL(0.,0.,0.,3.5052,16.4592,57.5)
         CALL ISZCYL(0.,0.,0.,52.034,72.2,89.7)
      ENDIF
      CALL PUGETV('NEUTRINOS PLOTTED',NEUSON)
      CALL PUGETV('NEUTRALS PLOTTED',NEUTLS)
      CALL PUGETV('P MIN CUT',PMIN)
      CALL PUGETV('PT MIN CUT',PTMIN)
      CALL PUGETV('THETA MIN CUT',THMDE)
      THMIN = (PI/180.)*THMDE
      DO 20 I = 1, NPART
      PT=SQRT(PX(I)**2+PY(I)**2)
      IF(PT.LT.PTMIN)GO TO 20
C
C  Set up default color and linestyle.
C  =====================================
C
          CALL PXCOLR('GRE')
          CALL JLSTYL(0)
C
C  Decide on a different color and linestyle, depending on the ID of the
C  track...
C  ======================================================================
C
          CALL ISZOPT(ID(I))
          ICHRG = IDCHRG(ID(I))
          IF (NEUSON) THEN
             IF ((ABS(ID(I)).EQ.11).OR.(ABS(ID(I)).EQ.13)
     X       .OR.(ABS(ID(I)).EQ.15)) ICHRG=1
          ENDIF
          IF ((ICHRG .NE. 0).OR.(NEUTLS))THEN
             IF ((THETA(I).GT.THMIN).AND.
     X           (THETA(I).LT.(PI-THMIN))) THEN
                IF(P(I).GT.PMIN)THEN
                   CALL J3MOVE(X(I),Y(I),Z(I))
                   PTOT = SQRT(PX(I)**2+PY(I)**2+PZ(I)**2)
                   XF = X(I) + (PX(I)*LEN/PTOT)
                   YF = Y(I) + (PY(I)*LEN/PTOT)
                   ZF = Z(I) + (PZ(I)*LEN/PTOT)
                   CALL J3DRAW(XF,YF,ZF)
                ENDIF
             ENDIF
          ENDIF
   20 CONTINUE
      CALL JRCLOS
      IF (I3D .EQ. 1) THEN
        CALL JMODON(.FALSE.)
      ENDIF
      IF(PMIN.GT.0)THEN
        WRITE(MESS1,200) PMIN
  200 FORMAT('P MIN= ',F6.2)
        CALL PUMESS(MESS1)
      ENDIF
      IF(PTMIN.GT.0)THEN
        WRITE(MESS1,201) PTMIN
  201 FORMAT('PT MIN= ',F6.2)
        CALL PUMESS(MESS1)
      ENDIF
      IF(THMDE.GT.0)THEN
        WRITE(MESS1,202) THMDE
  202 FORMAT('THETA MIN= ',F6.2)
        CALL PUMESS(MESS1)
      ENDIF
      RETURN
      END

      INTEGER FUNCTION SAGTCT(NPL,LINE,W1,W2,DIF,DIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get number of nearest triplet to the given line
C-                         in 3D,XZ or YZ plane
C-   Returned value  : triplet number
C-   Inputs  :
C-             NPL   -   plane number
C-             LINE  -   point on line and direction cosine
C-             DIF   -   maximum distance between hit and line
C-              W1   -   weight for XZ plane
C-              W2   -   weight for YZ plane
C-   Outputs :
C-            DIF    -   weigted distance for closest triplet
C-            DIST   -   distances in XZ and  YZ planes
C-
C-   Controls:
C-
C-   Created   8-AUG-1995   Andrei Mayorov
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LSAH3,GZSAH3,LSAHS,GZSAHS
      EXTERNAL GZSAH3,GZSAHS
      INTEGER NPL,I,ND,NTRIP
      PARAMETER(ND=4)
      REAL    LINE(6),DIF,DIST(2),distw,dxz,dyz,sadipl
      REAL    w1,w2
      external sadipl
C----------------------------------------------------------------------
      SAGTCT=-1
      LSAHS=GZSAHS()
      IF (LSAHS.LE.0) RETURN
      NTRIP=IQ(LSAHS+18+NPL)
      IF (NTRIP.LE.0) RETURN
      LSAH3=GZSAH3(NPL)
      IF(LSAH3.LE.0) RETURN
      DO I=1,NTRIP
        IF(IQ(LSAH3+ND*(I-1)+1).GE.4) THEN ! triplet contains nonisolated tubes
          DXZ=SADIPL(Q(LSAH3+ND*(I-1)+2),LINE,2)
          DYZ=SADIPL(Q(LSAH3+ND*(I-1)+2),LINE,3)
          DISTW=DXZ*W1+DYZ*W2
          DISTW=SQRT(DISTW)
          DXZ=SQRT(DXZ)
          DYZ=SQRT(DYZ)
          IF(DISTW.LT.DIF) THEN
            DIF=DISTW
            DIST(1)=DXZ
            DIST(2)=DYZ
            SAGTCT=I
          END IF
        END IF
      END DO

  999 RETURN
      END

      SUBROUTINE VEE3D(I1,I2,PHI1,PHI2,THE1,THE2,XYZ,EXYZ,PRIM,EPRIM)
C------------------------------------------------------------------
C 
C  Check if the pair of central tracks makes a vee in 3D. 
C  If good vee in 3D book and fill VERT and PVES
C
C  Input:    I1,I2      - ID's of the two tracks
C            PHI1,PHI2,THE1,THE2  - spherical angles of the two tracks
C            XYZ(1:3)   - vertex coordinates
C            EXYZ(1:3)  - error of vertex coordinates
C            PRIM(1:3)  - coordinates of the primary vertex
C            EPRIM(1:3) - error of coordinates of the primary vertex
C                             
C  Daria Zieminska 9-JUL-1990
C  Modified: 17-JUL-1991, Tom Trippe. Add phi, theta to VEEFIL call 
C  Modified: 12-AUG-1991, Tom Trippe. Add EZRSET calls 
C-   Updated   7-NOV-1991   Daria Zieminska  VEEFIL -> VEEFIT 
C
C------------------------------------------------------------------
      IMPLICIT NONE 
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAE.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISV2.LINK/LIST'
      INTEGER LISAE,LISV2
      INTEGER ICALL,IER,I1,I2
      INTEGER NV,IORV,IORP
      CHARACTER*8 NAME,LABEL
      REAL PRIM(3),EPRIM(3),MOM(2),PLANMX 
      REAL X,Y,Z,R,XYZ(3),EXYZ(3),XV,YV,ZV,PLAN,CROSS,P1,P2,RATIO
      REAL X1,Y1,Z1,X2,Y2,Z2,THE1,THE2,PHI1,PHI2
      REAL DXYZ(3),EDXYZ(3)
      REAL PX,PY,PZ,P,COSALFA,COSMIN,R3DMIN,PVEE_MIN
      SAVE ICALL 
      DATA ICALL/0/
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VEES_RCP')
        CALL EZGET('PLANMX',PLANMX,IER)
        CALL EZGET('COSMIN',COSMIN,IER)
        CALL EZGET('R3DMIN',R3DMIN,IER)
        CALL EZGET('PVEE_MIN',PVEE_MIN,IER)
        CALL EZRSET
        ICALL=1
      END IF
      DXYZ(1)=XYZ(1)-PRIM(1)
      DXYZ(2)=XYZ(2)-PRIM(2)
      DXYZ(3)=XYZ(3)-PRIM(3) 
      R=SQRT(DXYZ(1)**2+DXYZ(2)**2+DXYZ(3)**2)
      IF (R.LT.R3DMIN) GO TO 1000
      X=DXYZ(1)/R
      Y=DXYZ(2)/R
      Z=DXYZ(3)/R
      X1=SIN(THE1)*COS(PHI1)
      Y1=SIN(THE1)*SIN(PHI1)
      Z1=COS(THE1)          
      X2=SIN(THE2)*COS(PHI2)
      Y2=SIN(THE2)*SIN(PHI2)
      Z2=COS(THE2)          
      PLAN=X*Y1*Z2+X1*Y2*Z+X2*Y*Z1-
     &     X2*Y1*Z-X*Y2*Z1-X1*Y*Z2
C      IF (ABS(PLAN).GT.PLANMX) GO TO 1000
      CROSS=X2*Y1-X1*Y2
      P1=Y*X2-X*Y2
      P1=P1/CROSS
      P2=X*Y1-X1*Y
      P2=P2/CROSS
      IF (P1.LT.0..OR.P2.LT.0.) GO TO 1000
      X1=X1*P1
      Y1=Y1*P1
      Z1=Z1*P1
      X2=X2*P2
      Y2=Y2*P2
      Z2=Z2*P2
      PX=X1+X2
      PY=Y1+Y2
      PZ=Z1+Z2
      P=SQRT(PX**2+PY**2+PZ**2)
      COSALFA=X*PX+Y*PY+Z*PZ
      COSALFA=COSALFA/P
      IF (COSALFA.LT.COSMIN) GO TO 1000
C
C  Fit the vee and fill vertex bank VERT and particle bank PVES
C
      CALL VEEFIT(I1,I2,XYZ,EXYZ,DXYZ,PHI1,PHI2,THE1,THE2)
 1000 RETURN
      END       

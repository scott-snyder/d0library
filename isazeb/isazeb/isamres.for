      SUBROUTINE ISAMRES(X,Y,Z,PX,PY,PZ,E)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC   ADDS RESOLUTION TO A GIVEN MUON --- ONLY MOMENTUM
CC    INPUT: X,Y,Z   3 COMPONENTS OF MOMENTUM
CC    OUTPUT: PX,PY,PZ 3 COMPONENTS OF MOMENTUM
CC            E    TOTAL ENERGY
CC    set up for D0 muons
CC       DH 12/87
CC       dh 11/90 add angular dependent resolution
CC          DH 7/91 fix inadverdant error setting resolution=0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      DOUBLE PRECISION THET,PHI,PT,PTP
      REAL T,A,B,TR,D,R1,X,Y,Z,PX,PY,PZ,P,DELT,PIN,EMMU,RES,PP,E
      DATA DELT/.0005/         ! ANGULAR RESOLUTION--NOT RIGHT
      DATA EMMU/.106/
CCC
      PIN=SQRT(X**2+Y**2+Z**2)
      PT=SQRT(X**2+Y**2)
C
C   DO ENERGY
C
       D=Z/PIN
      T=ABS(ACOS(D))*180./3.14159
      IF(T.GT.90.) T=180.-T
      TR=T*3.14159/180.
      IF(T.GT.45.) THEN
        A=.18*SQRT(SIN(TR))
        B=.002*SIN(TR)
      ELSE IF(T.GT.35..AND.T.LE.45.) THEN
        A=.18*SQRT(SIN(TR))/SQRT(.5)
        B=.004*SIN(TR)
      ELSE IF(T.GT.20..AND.T.LE.35.) THEN
        A=.18*SQRT(COS(TR))/SQRT(1.5)
        B=.0015*COS(TR)
      ELSE
        A=.18*SQRT(COS(TR))/SQRT(1.5)
        B=.001*COS(TR)
      ENDIF
        RES=SQRT(A**2 + (PIN*B)**2)
      CALL NORRAN(R1)
      PP=1./PIN +1./PIN*R1*RES 
      P=ABS(1./PP)
      E=SQRT(P**2+EMMU**2)
C
C      CALL NORRAN(R1)
C      THET=R1*DELT
C      CALL NORRAN(R1)
C      PHI=R1*DELT
C      PTP=PT+THET*Z
C      PZ=Z-THET*PT
C      PX=PTP*(X/PT-PHI*Y/PT)
C      PY=PTP*(Y/PT+PHI*X/PT)
      PX=X*P/PIN
      PY=Y*P/PIN
      PZ=Z*P/PIN
C
      RETURN
      END     
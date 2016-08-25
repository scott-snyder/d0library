      SUBROUTINE MSVCHK(ISTAT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This checks overlap of volume definition 
C-   for Muon PDT modules.    
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   4-APR-1989   Shuichi Kunori
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$LINKS:IZSTPO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSTPC.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSMUO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMGEH.LINK/LIST'
C  varables in i/o argument...
      INTEGER ISTAT
C  external variables...
      INTEGER GZMGEO
C  local variables...
      INTEGER I,J
      INTEGER I1,I2,J1,J2,J3,I1I,I2I,K1,K2,K3,N
      INTEGER NOVER,IOVER(2,100)
      CHARACTER*4 HSHAP1,HSHAP2
      REAL     SPAR1(3),XPAR1(3),ROTM1(9),  SPAR2(3),XPAR2(3),ROTM2(9)
      INTEGER  NSPAR1, NSPAR2, NBUF,IBUF(1)
      REAL FSIGN(2),XYZMIN(3),XYZMAX(3),CORNR2(3,8),V(3),R,X,Y
      REAL XLOC2(3),X2R2(3),X2R2T1(3),X2IN1(3)
      DATA FSIGN/1.0,-1.0/
C
      ISTAT=0
C
      NOVER=0
      CALL VZERO(IOVER,200)
C
      WRITE(6,600)
C
C     -- loop over PDT modules...
C
      DO 100 I1=1,307
        I1I=I1
C     -- obtain geometry parameters for I1 module...
        NBUF=0
        CALL MUMODU(I1,HSHAP1,NSPAR1,SPAR1,XPAR1,ROTM1,NBUF,IBUF)
        IF(HSHAP1.EQ.'    ') GO TO 101
C     -- reduce volume size by 0.1cm to avoid precision problem...
        DO 104 I=1,3
           SPAR1(I)=SPAR1(I)-0.1
104     CONTINUE
C
C
C        -- loop over PDT modules for second module.
C
         DO 200 I2=1,307
            I2I=I2
            IF(I1.EQ.I2) GO TO 201
C           -- check if this pair has been tested and found to be 
C              overlapping...
            IF(NOVER.GT.0) THEN
              DO 202 I=1,NOVER
                IF(IOVER(1,I).EQ.I2I.AND.IOVER(2,I).ne.0.AND.
     &             I1I.ne.0) GO TO 201
202           CONTINUE
            ENDIF
C           -- obtain geometry parameters for I2 module...
            NBUF=0
            CALL MUMODU(I2,HSHAP2,NSPAR2,SPAR2,XPAR2,ROTM2,NBUF,IBUF)
            IF(HSHAP2.EQ.'    ') GO TO 201
C           -- reduce volume size by 0.1cm to avoid precision probram...
            DO 204 I=1,3
               SPAR2(I)=SPAR2(I)-0.1
204         CONTINUE
C           -- calculate coordinates for eight corners...
            N=0
            CALL VFILL(XYZMIN,3, 99999.)
            CALL VFILL(XYZMAX,3,-99999.)
            DO 210 K1=1,2
            DO 212 K2=1,2
            DO 214 K3=1,2
C               -- calculate 8 coner points of I2-module and transform
C                  them into I1 local coordinate system.   Transformation
C                  between lab-system and PDT local system is defined as    
C
C                       X(lab) = T + INV(R) * x(local)
C                  
C                  Transformation here is,
C
C                       x(local1) = R1 * ( X(lab) - T1)
C                  with
C                       X(lab) = T2 + INV(R2) * x(local2)
C                  
C               -- corner in local coordinate system of second module...
                XLOC2(1)=FSIGN(K1)*SPAR2(1)
                XLOC2(2)=FSIGN(K2)*SPAR2(2)
                XLOC2(3)=FSIGN(K3)*SPAR2(3)
C               -- rotate to global system...
                CALL VMATR(XLOC2,ROTM2,X2R2,3,3)
C               -- corner of second module in local system 1
C                  without rotation...
                X2R2T1(1)=XPAR2(1)+X2R2(1)-XPAR1(1)
                X2R2T1(2)=XPAR2(2)+X2R2(2)-XPAR1(2)
                X2R2T1(3)=XPAR2(3)+X2R2(3)-XPAR1(3)
C               -- now rotate to local system 1...
                CALL VMATL(ROTM1,X2R2T1,X2IN1,3,3)
C               -- store a coordinate of the corner of second module
C                  in the system of the first module...
                N=N+1
                CORNR2(1,N)=X2IN1(1)
                CORNR2(2,N)=X2IN1(2)
                CORNR2(3,N)=X2IN1(3)
C               -- test if this point is in the first moule or not.
C                  If so,  jump out from loop, and go to next module...
                IF(ABS(X2IN1(1)).LT.SPAR1(1).AND
     +            .ABS(X2IN1(2)).LT.SPAR1(2).AND  
     +            .ABS(X2IN1(3)).LT.SPAR1(3)) THEN
                      GO TO 270
                ENDIF
C               -- looking for minimum and maximum coordinates...
                DO 216 I=1,3
                  IF(X2IN1(I).GT.XYZMAX(I)) XYZMAX(I)=X2IN1(I)
                  IF(X2IN1(I).LT.XYZMAX(I)) XYZMIN(I)=X2IN1(I)
216             CONTINUE
C
214         CONTINUE
212         CONTINUE
210         CONTINUE
C           -- test if the second module is completely outside of
C              the first module...
            DO 220 I=1,3
               IF(XYZMIN(I).GT.SPAR1(I)) GO TO 201
               IF(XYZMAX(I).LT.-SPAR1(I)) GO TO 201
220         CONTINUE
C
C           -- now testing if lines between corners of second modules
C              intercept the first box...
            DO 230 K1=1,7
            K3=K1+1
            DO 232 K2=K3,8
C              -- calculate vector from one corner to another...
               DO 234 I=1,3
                 V(I)=CORNR2(I,K1)-CORNR2(I,K2)
234            CONTINUE
C 
               DO 240 J=1,3
C                -- set up cyclic indecies...
                 J1=MOD(J-1,3)+1
                 J2=MOD(J  ,3)+1
                 J3=MOD(J+1,3)+1
                 DO 242 I=1,2
                    IF(ABS(V(J3)).GT.1.0E-10) THEN
                       R=(FSIGN(I)*SPAR1(J3)-CORNR2(J3,K2))/V(J3)
                       IF(R.GT.0.0.AND.R.LT.1.0) THEN
                          X=R*V(J1)+CORNR2(J1,K2)
                          Y=R*V(J2)+CORNR2(J2,K2)
                          IF(ABS(X).LT.SPAR1(J1).AND
     +                             .ABS(Y).LT.SPAR1(J2)) THEN
                              GO TO 270
                          ENDIF
                       ENDIF
                    ENDIF
242              CONTINUE
C
240            CONTINUE
C
232         CONTINUE
230         CONTINUE
C
            GO TO 201
C           -- set flag for overlap...
270         CONTINUE
            ISTAT=1
            NOVER=NOVER+1
            IOVER(1,NOVER)=I1I
            IOVER(2,NOVER)=I2I
201      CONTINUE
200      CONTINUE
C
101   CONTINUE
100   CONTINUE
C
C     -- print out modules overlaping...
      IF(NOVER.GT.0) THEN
         DO 300 I=1,NOVER
            WRITE(6,610) I,IOVER(1,I),IOVER(2,I)
300      CONTINUE
      ELSE
         WRITE(6,620)
      ENDIF
C
  999 RETURN
C
600   FORMAT('  ****** LIST OF PDT MODULE OVERLAP ***** ')
610   FORMAT(10X,I5,5X,'MODULES = ',I3,' AND ',I3)
620   FORMAT(15X,'(NO MODULE OVERLAP)')
C
      END

      SUBROUTINE VKIN_VAL(IMT,IEND,X,ETA,CY,NM,NX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To convert ordinary values to   
C-                         double precision format. Covariance 
C-                         error matrix CY for measured values 
C-                         defines here.
C-   Inputs  :  IMT   =    1 - fit for decay,
C-                         2 - fit for interaction at target in rest,
C-                         3 - fit for interaction at target in motion. 
C-   Outputs : 
C-              IEND  =  - 7  if IMT out off order 
C-              IEND  =  - 8  if number of degrees of freedom less then zero
C-              IEND  =  - 9  if number of tracks great then 10
C-              X     -    vector of unmeasured momenta for given fitting step
C-              ETA   -    vector of measured angels for given fitting step
C-              CY    -    covariance error matrix for measured values 
C-              NM    -    number of measured fitting values
C-              NX    -    number of unmeasured fitting values
C-   Controls: 
C-
C-   Created  24-jul-1991   V. Burtovoy
C-   Updated  22-OCT-1991   Daria Zieminska  D0 standards 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:VEEKIN.INC/LIST'
      REAL*8 X(4),ETA(50),CY(1500),BUF(1500)
      REAL*8 DZERO
      INTEGER NX,NM,IMT,IEND,I,II,J,NR,KA
      DATA DZERO /0.D00/
c
      IF(NTR.GT.10) GO TO 5
      NX=0
      NM=0
      CALL VZERO(ITR,40)
      DO 1 I=1,NTR
      II=IND(I)
      IF(II.LT.1.OR.II.GT.7) GO TO 2
      GO TO (10,20,30,40,50,60,70),II
   10 DO 11 J=1,3
      ITR(J,I)=-(NM+J)
      ETA(NM+J)=DBLE(STR(J,I))
   11 BUF(NM+J)=DBLE(ETR(J,I))
      NM=NM+3
      GO TO 100
   20 DO 21 J=1,3
      ITR(J,I)=-(NM+J)
      ETA(NM+J)=DBLE(STR(J,I))
   21 BUF(NM+J)=DBLE(ETR(J,I))
      NM=NM+3
      X(NX+1)=DBLE(STR(4,I))
      ITR(4,I)=NX+1
      NX=NX+1
      GO TO 100
   30 DO 31 J=2,3
      ITR(J,I)=-(NM+J-1)
      ETA(NM+J-1)=DBLE(STR(J,I))
   31 BUF(NM+J-1)=DBLE(ETR(J,I))
      NM=NM+2
      X(NX+1)=DBLE(STR(1,I))
      ITR(1,I)=NX+1
      NX=NX+1
      GO TO 100
   40 DO 41 J=2,3
      ITR(J,I)=-(NM+J-1)
      ETA(NM+J-1)=DBLE(STR(J,I))
   41 BUF(NM+J-1)=DBLE(ETR(J,I))
      NM=NM+2
      ITR(1,I)=NX+1
      ITR(4,I)=NX+2
      X(NX+1)=DBLE(STR(1,I))
      X(NX+2)=DBLE(STR(4,I))
      NX=NX+2
      GO TO 100
   50 DO 51 J=1,4
      ITR(J,I)=NX+J
   51 X(NX+J)=DBLE(STR(J,I))
      NX=NX+4
      GO TO 100
   60 DO 61 J=1,3
      ITR(J,I)=NX+J
   61 X(NX+J)=DBLE(STR(J,I))
      NX=NX+3
      GO TO 100
   70 ITR(1,I)=-(NM+1)
      ETA(NM+1)=DBLE(STR(1,I))
      BUF(NM+1)=DBLE(ETR(1,I))
      NM=NM+1
      ITR(2,I)=NX+1
      ITR(3,I)=NX+2
      X(NX+1)=DBLE(STR(2,I))
      X(NX+2)=DBLE(STR(3,I))
      NX=NX+2
      GO TO 100
  100 CONTINUE
    1 CONTINUE
      NR=4-NX
      IF(NR.LT.0) GO TO 4
      CALL DVSET(NM*NM,DZERO,CY(1),CY(2))
      DO 3 J=1,NM
      KA=(J-1)*NM+J
    3 CY(KA)=BUF(J)**2
      GO TO 999 
    4 IEND=-8
      GO TO 999 
    2 IEND=-7
      GO TO 999 
    5 IEND=-9
  999 RETURN
      END

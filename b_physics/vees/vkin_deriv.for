      SUBROUTINE VKIN_DERIV(IMT,X,ETA,F,A,B,NM,NX,NF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  to calculate the matrices A and B
C                           of derivatives for current values
C                           of X and ETA, correspondly.
C                           Here, vector F(4) is also calculated.
C-   Inputs  : IMT    = 1 - fit for decay,
C-                      2 - fit for interaction at fix. target,
C-                      3 - fit for interaction at moving target.
C-             X      - current meanings for unmeasured values
C-             ETA    - current meanings for measured values
C-             F(1)   =    P1x + P2x - P3x
C-             F(2)   =    P1y + P2y - P3y
C-             F(3)   =    P1z + P2z - P3z
C-             F(4)   =    E1  + E2  - E3,
C-     where:  PIx - momentum projection of I-th track
C-                   to the x- direction (for example)
C-             EI  - energy of I-th track
C-             NM     -    number of measured values
C-             NX     -    number of unmeasured values
C-             NF     =    4 - number of elemens dimension F
C-   Outputs :
C-             A      -    matrix of derivatives for current
C-                         values of X (momenta)
C-             B      -    matrix of derivatives for current
C-                         values of ETA (angles)
C-
C-   Created  24-jul-1991   V. Burtovoy
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:VEEKIN.INC/LIST'
      REAL*8 X(4),F(4),ETA(50),B(200),A(20),BUF(1500)
      REAL*8 DZERO,P,XM,EI,S,CF,CT,SF,ST,XF,XT
      REAL S1
      INTEGER NF,NX,NM,L,M,LST,IMT,I,I1,I2,I3,I4,II,IER
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
      DATA DZERO /0.D00/
C
      CALL DVSET(NF,DZERO,F(1),F(2))
      L      = 0
      M      = 0
      LST    = NTR
      IF(IMT .EQ. 3) LST = LST - 1
      DO 1 I = 1, NTR
        S1   = 1. - 2.*(I/LST)
        S    = DBLE(S1)
        I1   = ITR(1,I)
        IF(I1 .GT. 0) P = X(I1)
        IF(I1 .LT. 0) P = ETA(-I1)
        IF(P .LT. DZERO .AND. I1 .GT. 0) THEN
          X(I1) = DZERO
          P     = DZERO
        END IF
        IF(P .LT. DZERO .AND. I1 .LT. 0) THEN
          ETA(-I1) = DZERO
          P        = DZERO
        END IF
        I2 = ITR(2,I)
        IF(I2 .GT. 0) XT = X(I2)
        IF(I2 .LT. 0) XT = ETA(-I2)
        I3 = ITR(3,I)
        IF(I3 .GT. 0) XF =    X(I3)
        IF(I3 .LT. 0) XF = ETA(-I3)
        I4 = ITR(4,I)
        XM = DBLE(STR(4,I))
        IF(I4 .GT. 0) XM = X(I4)
        IF(I4 .GT. 0 .AND. XM .LT. DZERO) X(I4) = DZERO
        EI = DSQRT(P*P + XM*XM)
        CF = DCOS(XF)
        SF = DSIN(XF)
        CT = DCOS(XT)
        ST = DSIN(XT)
        II = IND(I)
        GO TO (10,10,11,11,11,11,10),II
   10   B(M+1) =  ST*CF*S
        B(M+2) =  ST*SF*S
        B(M+3) =  CT*S
        B(M+4) =  P*S/EI
        M      =  M + 4
   11   GO TO (12,12,12,12,13,13,13),II
   12   B(M+1) =  P*CT*CF*S
        B(M+2) =  P*CT*SF*S
        B(M+3) = -P*ST*S
        B(M+4) =  DZERO
        M      =  M + 4
        B(M+1) = -P*ST*SF*S
        B(M+2) =  P*ST*CF*S
        B(M+3) =  DZERO
        B(M+4) =  DZERO
        M      =  M + 4
   13   GO TO (15,15,14,14,14,14,15),II
   14   A(L+1) =  ST*CF*S
        A(L+2) =  ST*SF*S
        A(L+3) =  CT*S
        A(L+4) =  P*S/EI
        L      =  L + 4
   15   GO TO (17,17,17,17,16,16,16),II
   16   A(L+1) =  P*CT*CF*S
        A(L+2) =  P*CT*SF*S
        A(L+3) = -P*ST*S
        A(L+4) =  DZERO
        L      =  L + 4
        A(L+1) = -P*ST*SF*S
        A(L+2) =  P*ST*CF*S
        A(L+3) =  DZERO
        A(L+4) =  DZERO
        L      =  L + 4
   17   GO TO (3,18,3,18,18,3,3),II
   18   A(L+1) =  DZERO
        A(L+2) =  DZERO
        A(L+3) =  DZERO
        A(L+4) =  XM*S/EI
        L      =  L + 4
    3   CONTINUE
        F(1)   =  F(1) + P*ST*CF*S
        F(2)   =  F(2) + P*ST*SF*S
        F(3)   =  F(3) + P*CT*S
        F(4)   =  F(4) + EI*S
    1 CONTINUE
      IF(IMT .EQ. 2) F(4) = F(4) - XMT
      IF( NX .LE. 0)                              GO TO 2
      CALL DVCPY(NX*NF,A(1),A(2),BUF(1),BUF(2))
      CALL DMCPY(NX,NF,BUF(1),BUF(2),BUF(NF+1),A(1),A(NX+1),A(2))
    2 CALL DVCPY(NM*NF,B(1),B(2),BUF(1),BUF(2))
      CALL DMCPY(NM,NF,BUF(1),BUF(2),BUF(NF+1),B(1),B(NM+1),B(2))
      RETURN
      END

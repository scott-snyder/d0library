      SUBROUTINE SAKLM3(ZSTART,ZEND,N,Z,U,SI,CO,S,T,ELOS,
     +          PARAM,INF,BDLINT,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Kalman fit of SAMUS track
C-
C Input parameters:
C       zstart      --  coordinate where the initial fit vector is given
C       zend        --  coordinate to calculate fit vector at
C       n           --  number of points to fit through
C       z(n)        --  coordinate along the track
C       s(n)        --  measurement errors (sigma)
C       t(n)        --  angular deviation at i-th point due to
C                       multiple scattering
C       si(n),co(n) --  measurement directions
C       u(n)        --  measurements:
C                       u = si*y + co*x
C       inf(5,5)    --  initial *information* matrix
C       param(5)    --  paramtegers of track at plane i=n
C           param(1)    =       x0
C           param(2)    =       y0
C           param(3)    =       a
C           param(4)    =       b
C           param(5)    =       d
C       bdlint(3)   --  Integrated B*dL vector [KGauss*cm]
C Output parameters:
C       param(5)    --  paramtegers of track at plane i=1
C       inf(5,5)    --  information matrix of output track parameters
C       bdlint(3)   --  Integrated B*dL vector [KGauss*cm]
C       ok          --  Completion status (1 means OK)
C
C Track model:
C       x = x0 + a*z + quadratic-terms(d)
C       y = y0 + b*z + quadratic-terms(d)
C
C See P.Billoir, NIM 225(1984) pp.352-366 for details.
C
C----------------------------------------------------------------------
C-
C-   Created   3-MAY-1994   Igor V. Mandrichenko
C-   Updated  23-JAN-1995   Igor V. Mandrichenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER N
      REAL    ZSTART,ZEND,
     +        Z(N),U(N),SI(N),CO(N),S(N),T(N),ELOS(N),
     +        PARAM(5),INF(5,5),BDLINT(3)
C----------------------------------------------------------------------
      INTEGER OK,I
      REAL    COV(5,5),
     *          VPC(5,5),
     *          PARS(7),PAR(7),
     *          X0,Y0,A,B,D,
     *          X0S,Y0S,AS,BS,DS
C-debug+
C      REAL    RPX,RPY,RPA,RPB
	EXTERNAL    SKFGFLD
C----------------------------------------------------------------------
      EQUIVALENCE (PAR(1),X0),    (PARS(1),X0S),
     *              (PAR(2),Y0),    (PARS(2),Y0S),
     *              (PAR(3),A),     (PARS(3),AS),
     *              (PAR(4),B),     (PARS(4),BS),
     *              (PAR(5),D),     (PARS(5),DS)
C
C Start the recursion.
C
C       Make initial estimation
C
      CALL UCOPY(PARAM,PAR,5)
C<<
      IF( INF(1,1).GT.0. ) THEN
            !
            ! We have information matrix estimation.
            ! Copy it
        CALL    KFPROP(0,ZSTART,PAR,INF,Z(N),PAR,VPC,SKFGFLD)
      ELSE
            !
            ! Make "I don't know" information matrix
            ! Assume that parameters vector is given at z(n)
            !
            !   V(i,i) = 1/sigma(i)**2
            !
        CALL VZERO(VPC,25)
        VPC(1,1) = 1./10000.
        VPC(2,2) = 1./10000.
        VPC(3,3) = 1.
        VPC(4,4) = 1.
        VPC(5,5) = 1./((PAR(5)*0.9)**2+1.E-9)
      END IF
C<<
      IF( S(N).GT.0. ) THEN
        CALL KFMEAS(PAR,VPC,U(N),S(N),SI(N),CO(N),PAR,VPC,OK)
        IF( OK.NE.1 ) THEN
          OK = -101
          GOTO 999
        END IF
      END IF
C<<
      CALL    UCOPY(VPC,COV,25)
      CALL    RSINV(5,COV,5,OK)
      IF( OK.NE.0 ) THEN
        OK = -1
        GOTO    999
      END IF
C<<
      DO I=N,1,-1
        IF( T(I).GT.0. ) THEN
C+debug+
C               rpx = COV(1,5)/sqrt(COV(5,5)*COV(1,1))
C               rpy = COV(2,5)/sqrt(COV(5,5)*COV(2,2))
C               rpa = COV(3,5)/sqrt(COV(5,5)*COV(3,3))
C               rpb = COV(4,5)/sqrt(COV(5,5)*COV(4,4))
C-debug-
          CALL    KFCSCAT(COV,PAR,T(I),COV,OK)
C+debug+
C               rpx = COV(1,5)/sqrt(COV(5,5)*COV(1,1))
C               rpy = COV(2,5)/sqrt(COV(5,5)*COV(2,2))
C               rpa = COV(3,5)/sqrt(COV(5,5)*COV(3,3))
C               rpb = COV(4,5)/sqrt(COV(5,5)*COV(4,4))
C               ok = 1
C-debug-
        END IF
C<<
        IF(ELOS(I).GT.0.) THEN
          CALL KFELOS(PAR,0.105658,ELOS(I),PAR,OK)
          IF( OK.NE.1 ) THEN
            OK = -103
            GOTO 999
          END IF
        END IF
C<<
        IF( I.GT.1 ) THEN
          CALL    KFCPROP(-1,Z(I),PAR,COV,Z(I-1),PAR,COV,BDLINT,
     +			      SKFGFLD)
C+debug+
C                   rpx = COV(1,5)/sqrt(COV(5,5)*COV(1,1))
C                   rpy = COV(2,5)/sqrt(COV(5,5)*COV(2,2))
C                   rpa = COV(3,5)/sqrt(COV(5,5)*COV(3,3))
C                   rpb = COV(4,5)/sqrt(COV(5,5)*COV(4,4))
C-debug-
          IF( S(I-1).GT.0. ) THEN
            CALL    UCOPY(COV,VPC,25)
            CALL    RSINV(5,VPC,5,OK)
            IF( OK.NE.0 ) THEN
              OK = -2
              GOTO 999
            END IF
            CALL KFMEAS(PAR,VPC,
     +                    U(I-1),S(I-1),SI(I-1),CO(I-1),
     +                    PAR,VPC,OK)
            IF( OK.NE.1 ) THEN
              OK = -104
              GOTO 999
            END IF
            CALL    UCOPY(VPC,COV,25)
            CALL    RSINV(5,COV,5,OK)
            IF( OK.NE.0 ) THEN
              OK = -3
              GOTO 999
            END IF
          END IF
        END IF
      END DO
C<<
      CALL    KFCPROP(0,Z(1),PAR,COV,ZEND,PARAM,INF,BDLINT,SKFGFLD)
      CALL    RSINV(5,INF,5,OK)
      IF( OK.NE.0 ) THEN
        OK = -4
        GOTO 999
      END IF
      OK = 1
C<<
  999 RETURN
      END

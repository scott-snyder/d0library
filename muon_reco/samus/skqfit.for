      SUBROUTINE SKQFIT(FIT6,TUBES,N,Z,FIT5,INF5,CHI2,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   1-JUN-1994   Igor V. Mandrichenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL    FIT6(6),TUBES(8,1),Z,FIT5(5),INF5(5,5),CHI2
      INTEGER OK
C----------------------------------------------------------------------
      REAL    FIT(4),FUNCTS(30),ERR(4),COV(4,4)
      INTEGER I,J,N,NN,NX,NY,NU
      EXTERNAL    SKDSTFCN
      COMMON  /SKMNSQ_DATA/ ZCOM,TUB(8,30)
      REAL    ZCOM,TUB
      REAL    SCC,    SSS,    SCS,
     +          SCCZ,   SSSZ,   SCSZ,
     +          SCCZZ,  SSSZZ,  SCSZZ,SI,CI,ZI
      REAL    WMTX(4,4),W2
      EQUIVALENCE
     +    (WMTX(1,1),SCC),  (WMTX(1,2),SCS),
     +    (WMTX(1,3),SCCZ), (WMTX(1,4),SCSZ),
     +    (WMTX(2,2),SSS),
     +    (WMTX(2,4),SSSZ),
     +
     +    (WMTX(3,3),SCCZZ),(WMTX(3,4),SCSZZ),
     +
     +    (WMTX(4,4),SSSZZ)

      Z = 0
      DO I=1,N
        Z = Z+TUBES(3,I)
      END DO
      Z = Z/N
      ZCOM = Z

      SCC=0
      SSS=0
      SCS=0
      SCCZ=0
      SSSZ=0
      SCSZ=0
      SCCZZ=0
      SSSZZ=0
      SCSZZ=0

      NN = 0
      NX = 0
      NY = 0
      NU = 0
      DO I=1,N
        IF( ABS(TUBES(4,I)) .LT. 0.2 ) THEN
          NY = NY + 1
        ELSE IF( ABS(TUBES(5,I)) .LT. 0.2 ) THEN
          NX = NX + 1
        ELSE
          NU = NU + 1
        END IF

        NN = NN+1

        SI = SQRT(TUBES(4,I)**2 + TUBES(5,I)**2)
        CI = TUBES(5,I)/SI
        SI = -TUBES(4,I)/SI
        ZI = TUBES(3,I) - ZCOM

        W2 = 1./TUBES(8,I)**2
        SCC     = SCC       +   CI*CI*W2
        SSS     = SSS       +   SI*SI*W2
        SCS     = SCS       +   SI*CI*W2
        SCCZ    = SCCZ      +   CI*CI*ZI*W2
        SSSZ    = SSSZ      +   SI*SI*ZI*W2
        SCSZ    = SCSZ      +   SI*CI*ZI*W2
        SCCZZ   = SCCZZ     +   CI*CI*ZI*ZI*W2
        SSSZZ   = SSSZZ     +   SI*SI*ZI*ZI*W2
        SCSZZ   = SCSZZ     +   SI*CI*ZI*ZI*W2

   10   CONTINUE
      END DO

      FIT(3) = FIT6(4)/FIT6(6)
      FIT(4) = FIT6(5)/FIT6(6)
      FIT(1) = FIT6(1) + FIT(3) * (Z-FIT6(3))
      FIT(2) = FIT6(2) + FIT(4) * (Z-FIT6(3))

      ERR(1) = 0.01
      ERR(2) = 0.01
      ERR(3) = 0.0001
      ERR(4) = 0.0001

      CALL    UCOPY(TUBES,TUB,N*8)
      CALL SKMNSQ (SKDSTFCN, N, 4, FUNCTS, FIT, ERR, COV)

      WMTX(2,3) = WMTX(1,4)
      DO I=1,4
        DO J=1,I
          INF5(I,J) = WMTX(J,I)
          INF5(J,I) = INF5(I,J)
        END DO
      END DO

      DO I=1,4
        INF5(I,5) = 0.
        INF5(5,I) = 0.
      END DO
      INF5(5,5) = 0.
      CALL    UCOPY(FIT,FIT5,4)

      CHI2 = 0
      DO I=1,N
        CHI2 = CHI2 + FUNCTS(I)**2
      END DO
      OK = 0
  999 RETURN
      END

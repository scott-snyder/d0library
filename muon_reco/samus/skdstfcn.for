      SUBROUTINE  SKDSTFCN(M,N,FUNC,FIT,IFLAG)
      REAL    FIT(4),FUNC(M)
      INTEGER M,N,IFLAG
      COMMON  /SKMNSQ_DATA/ ZCOM,TUB(8,30)
      REAL    FIT6(6),VZ
      EQUIVALENCE (VZ,FIT6(6))
      REAL    DIST2,W1,W2

      FIT6(1) = FIT(1)
      FIT6(2) = FIT(2)
      FIT6(3) = ZCOM
      VZ = 1./SQRT(FIT(3)**2 + FIT(4)**2 + 1.)
      FIT6(4) = FIT(3)*VZ
      FIT6(5) = FIT(4)*VZ

      IF( IFLAG.NE.3 ) THEN
        DO I=1,M
          CALL    SADS2L(FIT6, TUB(1,I), DIST2, W1, W2, IOK)
          FUNC(I) = (TUB(7,I)-SQRT(DIST2)) / TUB(8,I)
        END DO
      END IF
      RETURN
      END

      SUBROUTINE SPHER(NDIM,NMAX,PV,POUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Routine to calculate SPHERICITY for a group of
C-                         NMAX particles with 4-momenta  PV(1:4,1:NMAX)
C-
C-   Inputs  : NDIM - 2 for Transverse plane
C-                    3 for 3-dimentions
C-   Outputs : POUT(I) contains :
C         I = 1 , 3   Sphericity axis; pout(3) = fi(deg) if ndim=2
C         I = 4 , 6   Axis perpendicular to SPHERICITY axis
C         I = 7 , 9   Eigenvalues
c         i = 9/10    Sphericty (for ndim = 2/3)
C         I = 10      Planarity (ndim = 2)
C-   Controls: None
C-
C-   Created  14-DEC-1990  Andrzej Zieminski
C-   Updated  14-DEC-1990  Chip Stewart
C-   Updated  25-NOV-1991   Boaz Klima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NDIM,NMAX,IERR
      REAL PV(4,500) ,POUT(10), TTT(3,3),XL(3),EVEC(3,3),WORK(3)
      REAL YL(3),FVEC(3,3),SUM1,SUM2,PT,XX,FI
      INTEGER ITP(3),I,J,K,L,IT,JT
      REAL*8 TT(3,3)
      CHARACTER*50 MSG
C----------------------------------------------------------------------
      CALL UZERO(POUT,1,10)
      IERR = 0
      IF(NMAX.LT.2 .OR. NDIM.LT.2 .OR. NDIM.GT.3) IERR = 1
      IF(IERR.NE.0) GO TO 50
C
      DO 1 I = 1 ,3
        YL(I) = 0.
        ITP(I) = 0
        XL(I) = 0.
        WORK(I) = 0.
        DO 2 J = 1 , 3
          TT(I,J) = 0D0
          TTT(I,J) = 0.
          EVEC(I,J) = 0.
          FVEC(I,J) = 0.
    2   CONTINUE
    1 CONTINUE
C
      DO 11 J = 1 , NMAX
        DO 11 IT = 1 , NDIM
          DO 11 JT = 1 , NDIM
            IF(JT-IT) 13,12,11
   12       CONTINUE
            DO 22 I = 1 , NDIM
   22       TT(IT,JT) = TT(IT,JT) + PV(I,J)**2
   13       CONTINUE
            TT(IT,JT) = TT(IT,JT) - PV(IT,J)*PV(JT,J)
            TT(JT,IT) = TT(IT,JT)
   11 CONTINUE
      DO 31 IT = 1 , NDIM
        DO 31 JT = 1 , NDIM
   31 TTT(IT,JT) = TT(IT,JT)
C
C   CALCULATE EIGENVALUES AND EIGENVECTORS
C
      CALL EISRS1(3,NDIM,TTT,YL,FVEC,IERR,WORK)
      IF(IERR.NE.0) GO TO 50
C
C   ORDER EIGENVALUES IF NECCESSARY SO THAT XL(3) IS SMALLEST
C
      CALL ORDX(3,YL,ITP)
      DO 20 I = 1 , 3
        L = ITP(I)
        XL(I) = YL(L)
        DO 23 J = 1,3
   23   EVEC(J,I) = FVEC(J,L)
   20 CONTINUE
C
      DO 21 I = 1 , NDIM
        POUT(I) = EVEC(I,NDIM)
        POUT(I+3) = EVEC(I,1)
        POUT(I+6) = XL(I)
   21 CONTINUE
      POUT(9) = FLOAT(NDIM)*XL(NDIM)/(XL(1)+XL(2)+XL(3))
      pout(10) = (pout(7)-pout(8))/(pout(7)+pout(8))
C
      IF(NDIM.NE.2) GO TO 50
      IF(POUT(1).EQ.0. .AND. POUT(2).EQ.0) IERR = 1
      IF(ierr.EQ.1) go to 50
C
C     ORIENT AXIS FOR NDIM = 2
C
      SUM1 = 0.
      SUM2 = 0.
      DO 41 I = 1 , NMAX
        PT = SQRT(PV(1,I)**2+PV(2,I)**2)
        XX = POUT(1)*PV(1,I)+POUT(2)*PV(2,I)
        IF(XX.GE.0.) SUM1 = SUM1 + PT
        IF(XX.LT.0.) SUM2 = SUM2 + PT
   41 CONTINUE
      IF(SUM2.GT.SUM1) THEN
        DO 42 I = 1 , 6
   42   POUT(I) = -POUT(I)
      ENDIF
      IF(POUT(1).EQ.0.) GO TO 50
      FI = ATAN2(POUT(2),POUT(1))
      IF(FI.LT.0.) FI = FI + 6.2832
      FI = FI * 180./3.1416
      POUT(3) = FI
C
   50 IF(IERR.EQ.0 ) RETURN
c
      WRITE(MSG,100) NDIM,NMAX
      CALL ERRMSG(' ERROR IN SPHERICITY ','SPHERE',MSG,'W')
  100 FORMAT(' IN',I3,' DIMENSIONS FOR',I5,' PARTICLES ')
C
  999 RETURN
      END
C

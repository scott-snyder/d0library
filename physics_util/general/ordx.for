      SUBROUTINE ORDX(NMAX,XLIST,ITP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Orders NMAX real numbers of XLIST in
C-      descending order and stores results in ITP
C-
C-   Inputs  : NMAX, XLIST
C-   Outputs : ITP
C-   Controls: None
C-
C-   Created  25-NOV-1991   Boaz Klima ( copied from Andrzej Zieminski )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I, J, K, K1, NMAX, ITP(3), JPR
      REAL    PMAX, U, XLIST(3)
      DATA JPR/0/
C----------------------------------------------------------------------
      DO 30 K=1,NMAX
        PMAX=0.
        DO 30 I=1,NMAX
          IF(K.EQ.1) GO TO 25
          K1=K-1
          DO 20 J=1,K1
            IF(ITP(J).EQ.I) GO TO 30
   20     CONTINUE
   25     U = XLIST(I)
          IF(U.LT.PMAX) GO TO 30
          PMAX=U
          ITP(K)=I
   30 CONTINUE
C
      IF(JPR.EQ.0) RETURN
C
      PRINT 100,(XLIST(I),I=1,NMAX)
      PRINT 101,(ITP(I),I = 1,NMAX)
  100 FORMAT(1H0,1X,'LIST'/10(1X,10E10.3/))
  101 FORMAT(1X,'ORDER'/10(1X,10I10/))
C
      RETURN
      END

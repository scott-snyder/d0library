      SUBROUTINE MXWRT(SMB,A,N1,N2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to print on unit 2 contents of matrix A
C-                         of size N1 * N2
C-   Inputs  :             SMB - small text
C                          A   - matrix
C                          N1  - number of rows 
C                          N2  - number of columns 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   10-APR-1991   V. Burtovoy
C-
C----------------------------------------------------------------------
      REAL*8 A(1)
      CHARACTER*8 SMB
      WRITE(2,1000) SMB,N1,N2
 1000 FORMAT(/' -----"',A8,'"-----(',I2,'X',I2,') ----------')
      IF(N1*N2.EQ.0) RETURN
      DO 1 I=1,N1
      KB=(I-1)*N2+1
      KE=KB+N2-1
    1 WRITE(2,1001) (A(K),K=KB,KE)
 1001 FORMAT(12(1X,E10.4))
      RETURN
      END

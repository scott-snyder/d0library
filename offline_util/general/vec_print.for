      SUBROUTINE VEC_PRINT(PRUNIT,STRING,VEC,NSIZ,NVEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : PRINT PARTICLE VECTORS
C-
C-   Inputs  : PRUNIT=PRINT UNIT
C-             STRING =  character id string for print out
C-             VEC = vector (standard) format for particles
C-             NSIZ = Size of vector VEC
C-             NVEC number of occurences of VEC
C-   Outputs : 
C-   Controls: 
C-
C-   Created   9-AUG-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) STRING
      INTEGER NVEC,NSIZ,PRUNIT
      REAL    VEC(NSIZ,NVEC+1)
      INTEGER I,J
C----------------------------------------------------------------------
      DO I = 1 , NVEC
        IF(I.EQ.1)WRITE(PRUNIT,1)STRING
    1   FORMAT(1X,A10,8X,'EX',8X,'EY',8X,'EZ',8X,'E',8X,'ET',
     &    7X,'ETA',7X,'PHI')
        WRITE(PRUNIT,2)(VEC(J,I),J=1,7)
    2   FORMAT(11X,7F10.3)
      ENDDO
  999 RETURN
      END

      SUBROUTINE WRITE_RCP4(IRO,TIT,VEC,NVEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : WRITES A N- VECTOR ARAY AS AN RCP ARRAY TO UNIT
C-   IRO
C-
C-   Inputs  : IRO = UNIT
C-              TIT = RCP NAME OF ARRAY
C-              VEC = CONTENTS
C-              NVEC = NUMBER OF VECTORS
C-   Outputs :
C-   Controls:
C-
C-   Created  21-FEB-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IRO
      DOUBLE PRECISION    VEC(*)
      CHARACTER*(*) TIT
      INTEGER NVEC
      INTEGER K
C----------------------------------------------------------------------
      WRITE(IRO,1)TIT
    1 FORMAT(' \ARRAY ',A)
      WRITE(IRO,2)(VEC(K),K=1,NVEC)
    2 FORMAT(<NVEC>F)
      WRITE(IRO,3)
    3 FORMAT('\END')
  999 RETURN
      END

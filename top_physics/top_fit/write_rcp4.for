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
C-   Updated  23-MAR-2004   sss - compile with g77.
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
C&IF LINUX
C&      WRITE(IRO,*)(VEC(K),K=1,NVEC)
C&ELSE
      WRITE(IRO,2)(VEC(K),K=1,NVEC)
    2 FORMAT(<NVEC>F)
C&ENDIF
      WRITE(IRO,3)
    3 FORMAT('\END')
  999 RETURN
      END

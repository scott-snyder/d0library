C DEC/CMS REPLACEMENT HISTORY, Element EC_ROT_MATRICES.FOR
C *3    27-DEC-1988 16:57:16 RAJA "Correct bug"
C *2    27-DEC-1988 16:35:18 RAJA "EC ROT MATRICES"
C *1    27-DEC-1988 16:22:28 RAJA "PASSES ON ROTATION MATRICES"
C DEC/CMS REPLACEMENT HISTORY, Element EC_ROT_MATRICES.FOR
      SUBROUTINE EC_ROT_MATRICES
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Routine transmits the EC rotation matrices
C-                         from Prego input to Geant.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  27-DEC-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*16 ROTMAT
      DATA ROTMAT/'EC_ROT_MATRICES'/
      CHARACTER*32 NMSRC1
      INTEGER NRTMX
      PARAMETER( NRTMX  = 500 )   !maximum number of rotation matrices
C                                 ! per array
      INTEGER LSET(7*NRTMX+1)
      REAL    RSET(7*NRTMX+1)
      EQUIVALENCE (LSET,RSET)
C
      INTEGER IS,LEN3,NROT,IT,K
C----------------------------------------------------------------------
      CALL ADDSTR(ROTMAT,'(1)',NMSRC1,LEN3)   !Makes it into array format
      CALL GTSRCP(NMSRC1,LSET(1),1)
C
      NROT = LSET(1)
      IT = 1
      WRITE(20,2)ROTMAT
    2 FORMAT(' \ARRAY ',A)
      WRITE(20,3)NROT
    3 FORMAT(I8)
      DO 40 IS = 1 , NROT
        WRITE(20,1)LSET(1+IT),(RSET(K+IT),K=2,7)
    1   FORMAT(I8,6F10.3)
        IT = IT + 7
   40 CONTINUE
      WRITE(20,4)
    4 FORMAT(' \END')
  999 RETURN
      END

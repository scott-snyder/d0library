      SUBROUTINE EC_MIXTURES
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Passes on EC mixtures in CGS
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  27-DEC-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*11 MIXTURE
C
      DATA MIXTURE/'EC_MIXTURES'/
      CHARACTER*32 NMSRC1
      CHARACTER*80 FORM
      CHARACTER*1 FORM0
      CHARACTER*27 FORM1
      CHARACTER*6 FORM2
      CHARACTER*5 FORM3
      DATA FORM1/'(1X,3('''''''',A4,'''''''',1X),2I3,'/
      DATA FORM2/'I3,1X,'/
      DATA FORM3/'F7.4)'/
C
      INTEGER LMIX(500)
      REAL    RMIX(500)
      EQUIVALENCE (LMIX,RMIX)
C
      INTEGER IS,LEN3,NMIX,IT,K,NMAT
      REAL CONV
C----------------------------------------------------------------------
      CALL ADDSTR(MIXTURE,'(1)',NMSRC1,LEN3)   !Makes it into array format
      CALL GTSRCP(NMSRC1,LMIX(1),1)
      CALL GTSRCP('CONV_FACTOR',CONV,1)    !conversion factor
C
      NMIX = LMIX(1)
      WRITE(20,2)MIXTURE
    2 FORMAT(' \ARRAY ',A)
      WRITE(20,3)NMIX
    3 FORMAT(I8)
      IT = 1
      DO 40 IS = 1 , NMIX
        NMAT = LMIX(IT+5)
        WRITE(FORM0,1)NMAT
    1   FORMAT(I1)
        FORM = FORM1//FORM0//FORM2//FORM0//FORM3   !Dynamic formatting
C                                             ! in Fortran the way I know it!
        WRITE(20,FORM)(LMIX(K+IT),K=1,5),(LMIX(K+IT),K=6,5+NMAT),
     &    (CONV*RMIX(K+IT),K=6+NMAT,5+2*NMAT)
c  11    FORMAT(1X,3(''''A4'''',1X),2I3,(I3),1X,(F7.4))
        IT = IT + 5+2*NMAT
   40 CONTINUE
      WRITE(20,4)
    4 FORMAT(' \END')
  999 RETURN
      END

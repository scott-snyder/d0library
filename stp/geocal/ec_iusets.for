      SUBROUTINE EC_IUSETS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Writes out User sets for EC
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  30-DEC-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*25 USRSET(4,2)
C
      DATA USRSET/'IUSET_END_CALORIMETER+Z  ',
     &            'IUSET_END_MASSLESS_GAPS+Z',
     &            'IUSET_END_PLATES+Z       ',
     &            'IUSET_END_CRACKS+Z       ',
     &            'IUSET_END_CALORIMETER-Z',
     &            'IUSET_END_MASSLESS_GAPS-Z',
     &            'IUSET_END_CRACKS-Z',
     &            'IUSET_END_PLATES-Z       '/
C
      CHARACTER*32 NMSRC1
      CHARACTER*200 FORM
      CHARACTER*1 FNV,FNH,FND
      CHARACTER*21 FORM1
      CHARACTER*18 FORM2
      CHARACTER*3 FORM3
      CHARACTER*8 FORM4
      DATA FORM1/'('''''''',A4,'''''''',I6,I3),'/
      DATA FORM2/'('''''''',A4,'''''''',I3),'/
      DATA FORM3/'I3,'/
      DATA FORM4/'(2F8.2),'/
C
      INTEGER LDET(2500)
      REAL    RDET(2500)
      EQUIVALENCE (LDET,RDET)
C
      INTEGER IZ,INUM
      INTEGER IS,LEN3,IT,ITR1,ITR2,ITR3,ITR4,ITR5,ITR6
      INTEGER NV,ND,NH,K,NSET
C----------------------------------------------------------------------
      DO 100 IZ = 1,2
      DO 200 INUM = 1,4   !3 user sets for each Z division
      CALL ADDSTR(USRSET(INUM,IZ),'(1)',NMSRC1,LEN3)   !Makes it into array format
      CALL GTSRCP(NMSRC1,LDET(1),1)
C
      WRITE(24,2)USRSET(INUM,IZ)
    2 FORMAT(' \ARRAY ',A)
      WRITE(24,3)(LDET(K),K=1,6)
    3 FORMAT(1X,'''',A4,'''',4I8,/,I8)
      NSET = LDET(6)
      IT = 6
      DO 40 IS = 1 , NSET
        ITR1= IT+1
        IT = IT+3
        NV = LDET(IT)
        IT = IT+2*NV+1
        NH = LDET(IT)
        ITR2 = IT+2*NH
        ITR3 = ITR2+1
        ITR4 = ITR2+2*NH
        ITR5 = ITR4+1
        ND = LDET(ITR5)
        ITR6 = ITR5+2*ND
        IT = ITR6
C
        WRITE(FNV,1)NV
        WRITE(FNH,1)NH
        WRITE(FND,1)ND
    1   FORMAT(I1)
C
        FORM = '(1X,'''''''',A4,'''''''',I6,I3,1X,'''''''',A4,'''''''',
     &    I3,I3,1X,'''''''',A4,'''''''',I3,
     &    2F8.2,I3,1X,'''''''',A4,'''''''',I3)'
        IF(NV.GT.1)THEN
        FORM = '(1X,'''''''',A4,'''''''',I6,I3,'//
     &    FNV//'(1X,'''''''',A4,'''''''')'//FNV//'(I3),
     &    I3,1X,'''''''',A4,'''''''',I3,
     &    2F8.2,I3,1X,'''''''',A4,'''''''',I3)'
        ENDIF
        WRITE(24,FORM)(LDET(K),K=ITR1,ITR2),(RDET(K),K=ITR3,ITR4),
     &    (LDET(K),K=ITR5,ITR6)
    4   FORMAT(1X,'''',A4,'''',I6,I3,'''',A4,'''',I3,I3,'''',A4,'''',I3,
     &    2F8.2,I3,'''',A4,'''',I3)
C
   40   CONTINUE
        WRITE(24,5)
    5   FORMAT('\END')
  200   CONTINUE
  100   CONTINUE
  999   RETURN
        END

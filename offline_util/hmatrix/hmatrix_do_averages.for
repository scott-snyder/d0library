      SUBROUTINE HMATRIX_DO_AVERAGES
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DO AVERAGES, MAKE MATRIX, INVERSE ETC.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-DEC-1990   Rajendran Raja
C-   Updated   7-APR-1995   Alan M. Jonckheere  
C-      Change calls DGET -> DDGET and DSET -> DDSET 
C-        to avoid conflict with new intrinsic functions
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INTEGER NEVENT
      DOUBLE PRECISION DAVER,DEMAT,DAVERI,DAVERJ
      INTEGER I,J,I1,J1,IND,IND1,HMINDEX,IXIO,IER
      REAL    TEMP
      INTEGER NERROR
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('TEMP','-D',IXIO)        ! Describe Bank format
        CALL MZBOOK
     &  (IDVSTP,LTEMP,0,2,'TEMP',0,0,2*TOT_DIM*TOT_DIM,IXIO,0)
C
C ****  TEMP BANK IS FOR TEMPORARY USE HERE.
C
      ENDIF
C
      NEVENT = IC(LHMTR+2)              ! TOTAL NUMBER OF EVENTS
      IF ( NEVENT.NE.0 ) THEN
        DO 10 I = 1 ,TOT_DIM
          IND = 2*I-1                     ! DOUBLE PRECISION
          CALL DDGET(LAVER+IND,DAVER)
          DAVER = DAVER/NEVENT
          CALL DDSET(LAVER+IND,DAVER)
   10   CONTINUE
C
        DO 30 I = 1 ,TOT_DIM
          IND = 2*I-1
          CALL DDGET(LAVER+IND,DAVERI)
          DO 20 J = 1 , TOT_DIM
            IND = 2*J-1
            CALL DDGET(LAVER+IND,DAVERJ)
            IND = 2*HMINDEX(I,J,TOT_DIM,TOT_DIM)-1          ! DOUBLE PRECISION
            CALL DDGET(LEMAT+IND,DEMAT)
            DEMAT = DEMAT/NEVENT
            DEMAT = DEMAT - DAVERI*DAVERJ       ! NOW ERROR MATRIX
            CALL DDSET(LEMAT+IND,DEMAT)
   20     CONTINUE
   30   CONTINUE
C
      ELSE
        CALL ERRMSG('HMATRIX','HMATRIX_DO_AVERAGES',
     &    'ZERO NUMBER OF TOTAL EVENTS ','W')
      ENDIF
C
C ****  ERROR MATRIX NOW READY FOR INVERSION.
C
      CALL MATRIX_INVERT_2('EMAT',C(LEMAT+1),TOT_DIM,C(LWORK+1),
     &  C(LHMAT+1),NERROR)
C
      IF(NERROR.NE.0)THEN
        CALL ERRMSG('HMATRIX','HMATRIX_DO_AVERAGES',
     &    'ERROR INVERTING EMAT MATRIX','W')
      ENDIF
C
      CALL MATRIX_INVERT_TEST2('EMAT',C(LEMAT+1),C(LHMAT+1),
     &  1.E-10,TOT_DIM,C(LPROD+1))
C
      DO 40 I = 1 , VIS_DIM
        DO 50 J  = 1 , VIS_DIM
          IND = 2*HMINDEX(I,J,TOT_DIM,TOT_DIM)-1          ! DOUBLE PRECISION
          CALL DDGET(LEMAT+IND,DEMAT)
          IND = 2*HMINDEX(I,J,VIS_DIM,VIS_DIM)-1          ! DOUBLE PRECISION
          CALL DDSET(LTEMP+IND,DEMAT)  ! STORE FOR INVERSION
   50   CONTINUE
   40 CONTINUE
C
C ****  ERROR MATRIX NOW READY FOR INVERSION.
C
      CALL MATRIX_INVERT_2('HVIS',C(LTEMP+1),VIS_DIM,C(LWORK+1),
     &  C(LHVIS+1),NERROR)
C
      IF(NERROR.NE.0)THEN
        CALL ERRMSG('HMATRIX','HMATRIX_DO_AVERAGES',
     &    'ERROR INVERTING TO GET HVIS MATRIX','W')
      ENDIF
C
      CALL MATRIX_INVERT_TEST2('HVIS',C(LTEMP+1),C(LHVIS+1),
     &  1.E-10,VIS_DIM,C(LPROD+1))
C
C
C ****  NOW TO PRODUCE HINV , THE INVERSE OF THE INVISBLE PORTION OF THE
C ****  HMATRIX
C
      IF ( INVIS_DIM.GT.0 ) THEN

        DO I = VIS_DIM+1,TOT_DIM
          I1 = I -VIS_DIM
          DO J = VIS_DIM+1 , TOT_DIM
            J1 = J - VIS_DIM
            IND = 2*HMINDEX(I,J,TOT_DIM,TOT_DIM)-1          ! DOUBLE PRECISION
            CALL DDGET(LHMAT+IND,DEMAT)
            IND = 2*HMINDEX(I1,J1,INVIS_DIM,INVIS_DIM)-1      ! DOUBLE PRECISION
            CALL DDSET(LTEMP+IND,DEMAT)    ! STORE FOR INVERSION
          ENDDO
        ENDDO
C
        CALL MATRIX_INVERT_2('HINV',C(LTEMP+1),INVIS_DIM,C(LWORK+1),
     &  C(LHINV+1),NERROR)
C
        IF(NERROR.NE.0)THEN
          CALL ERRMSG('HMATRIX','HMATRIX_DO_AVERAGES',
     &    'ERROR INVERTING TO GET HINV MATRIX','W')
        ENDIF
C
        CALL MATRIX_INVERT_TEST2('EMAT_VIS',C(LTEMP+1),C(LHINV+1),
     &  1.E-10,INVIS_DIM,C(LPROD+1))
C
      ELSE
        CALL ERRMSG('HMATRIX','HMATRIX_DO_AVERAGES',
     &    'NO INVISIBLE QUANTITIES. HINV NOT BEING MADE ','W')
      ENDIF
C
C
C ****  NOW TO GET THE EIGENVALUES AND EIGEN VECTORS OF HVIS
C
      DO I = 1 , VIS_DIM
        DO J = 1 , VIS_DIM
          IND1 = HMINDEX(I,J,VIS_DIM,VIS_DIM)
          IND = 2*IND1-1
          CALL DDGET(LHVIS+IND,DEMAT)
          TEMP = DEMAT                  ! SINGLE PRECISION
          CALL DDSET(LTEMP+IND1,TEMP)    ! IN TEMP BANK
        ENDDO
      ENDDO
C
      CALL EISRS1(VIS_DIM,VIS_DIM,C(LTEMP+1),C(LEIGN+1),
     &        C(LUMAT+1),IER,C(LPROD+1))
C
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('HMATRIX','HMATRIX_DO_AVERAGES',
     &    'ERROR IN DIAGONALIZING HVIS','W')
      ENDIF
C
      CALL HMATRIX_DUMP_BANKS           ! DUMP ALL BANKS OF INTEREST
C
  999 RETURN
      END
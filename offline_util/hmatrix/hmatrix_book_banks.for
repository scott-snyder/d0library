      SUBROUTINE HMATRIX_BOOK_BANKS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOK ALL THE BANKS. ONLY IN ACCUMULATION MODE.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-DEC-1990   Rajendran Raja
C-   Updated  22-JAN-1991   Harrison B. Prosper  
C-      Add SUB_DIRECTORY argument to BKMHTR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
C----------------------------------------------------------------------
      CALL BKHMTR(SUB_DIRECTORY)
      CALL BKAVER(TOT_DIM)
      CALL BKEMAT(TOT_DIM*TOT_DIM)
      CALL BKHMAT(TOT_DIM*TOT_DIM)
      CALL BKHVIS(VIS_DIM*VIS_DIM)
      CALL BKHINV(INVIS_DIM*INVIS_DIM)
      CALL BKEIGN(VIS_DIM)
      CALL BKUMAT(VIS_DIM*VIS_DIM)
      CALL BKQUAN(TOT_DIM)              ! EVENT RELATED QUANTITIES.
      CALL BKDIAG(VIS_DIM)          ! USING MATRIX MAY WANT TO DIAGONALIZE.
C
C ****  NOW TO BOOK SOME STAND ALONE WORK AND PRODUCT BANKS
C
      IF ( ACCUMULATE ) THEN
        CALL BKWORK(TOT_DIM*TOT_DIM)
        CALL BKPROD(TOT_DIM*TOT_DIM)    ! WORK BANKS FOR INVERSION AND TESTING
      ENDIF
C
  999 RETURN
      END

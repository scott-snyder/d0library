      SUBROUTINE PRAVER ( PRUNIT, LAVER, NAVER, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'AVER'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LAVER  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NAVER  [I] : Dummy
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LAVER point to a bank, or if <0, NAVER is
C-                                  the bank number.
C-                          'LINEAR' : LAVER points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  20-DEC-1990 15:07:37.26  Rajendran Raja
C-   Updated  20-AUG-1992 sss - compile on ibm
C-   Updated   7-APR-1995   Alan M. Jonckheere  Change DGET -> DDGET
C-      to avoid conflict with new intrinsic function 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INCLUDE 'D0$LINKS:IZAVER.LINK'
C
      INTEGER PRUNIT, LAVER, NAVER, IFL
      CHARACTER*(*) CFL
      INTEGER LAVER1, GZAVER, LZLOC, J,IND
      DOUBLE PRECISION DNUM
C----------------------------------------------------------------------
C
      WRITE(PRUNIT,1101)
 1101 FORMAT(/' Print of HMATRIX bank AVER . Visible quantities ',/)
      DO 1110 J = 1 , VIS_DIM
        IND = LAVER + (2*J-1)
        CALL DDGET(IND,DNUM)
        WRITE( PRUNIT, 1100 )J, VISIBLE_QUANTITIES(J),DNUM
 1100   FORMAT(2X,I5,2X,A32,2X,D15.7)
 1110 CONTINUE
C
      WRITE(PRUNIT,1121)
 1121 FORMAT(/' Print of HMATRIX bank AVER . Invisible quantities ',/)
      DO 1120 J = 1 , INVIS_DIM
        IND = LAVER + 2*VIS_DIM+ (2*J-1)
        CALL DDGET(IND,DNUM)
        WRITE( PRUNIT, 1102 )J, INVISIBLE_QUANTITIES(J),DNUM
 1102   FORMAT(2X,I5,2X,A32,2X,D15.7)
 1120 CONTINUE
C
  999 RETURN
      END

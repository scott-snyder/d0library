      SUBROUTINE PRHVIS ( PRUNIT, LHVIS, NHVIS, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'HVIS'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LHVIS  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NHVIS  [I] : DUMMY
C-             CFL    [C*]: DUMMY
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  20-DEC-1990 15:07:37.26  Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZHVIS.LINK'
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
C
      INTEGER PRUNIT, LHVIS, NHVIS, IFL
      CHARACTER*(*) CFL
C----------------------------------------------------------------------
C
C  ***  Print the content of the bank pointed by LHVIS
C
      CALL MXPRND(PRUNIT,'DUMP OF HVIS',C(LHVIS+1),VIS_DIM,
     &  VIS_DIM,VIS_DIM,VIS_DIM,7,'(D10.3)')
  999 RETURN
      END

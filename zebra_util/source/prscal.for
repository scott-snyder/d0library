      SUBROUTINE PRSCAL(PRUNIT,LLSCAL,NSCAL,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print contents of Calorimeter Static
C-                         Parameters bank SCAL.
C-
C-   Inputs  :  PRUNIT          unit number for printout
C-              LLSCAL          bank address
C-              NSCAL           not used
C-              CFL             not used
C-              IFL             not used
C-   Outputs :  none
C-   Controls:  none
C-   Requires:  ZEBSTP          SCAL bank
C-   Fills   :  none
C-   Calls   :  none
C-
C-   Created   7-JUL-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER PRUNIT, LLSCAL, NSCAL, IFL
      INTEGER I
      CHARACTER CFL*(*)
C
C ****  Check for legitimate bank address
C
      IF ( LLSCAL .LE. 0 ) THEN
        WRITE(PRUNIT,1000) LLSCAL
        GO TO 999
      ENDIF
C
C ****  Print link info
C
      WRITE(PRUNIT,1001)
      WRITE(PRUNIT,1002) (LC(LLSCAL-I),I=4,1,-1)
      WRITE(PRUNIT,1003) (LC(LLSCAL+I),I=0,2)
C
C ****  Print standard bank info
C
      WRITE(PRUNIT,1004) (IC(LLSCAL-I),I=5,1,-1)
C
C ****  Print bank data
C
      WRITE(PRUNIT,1005) (IC(LLSCAL+I),I=1,8),C(LLSCAL+9),IC(LLSCAL+10)
C
  999 RETURN
C
 1000 FORMAT(/' Wrong address for SCAL:  LLSCAL = ',I8/)
 1001 FORMAT(/' Calorimeter Static Parameters bank: SCAL'/)
 1002 FORMAT(
     &  ' LC(LLSCAL-4)  Structural link to CGEH   : ',I8/
     &  ' LC(LLSCAL-3)  Structural link to CCPH   : ',I8/
     &  ' LC(LLSCAL-2)  Structural link to CGNH   : ',I8/
     &  ' LC(LLSCAL-1)  Structural link to CPDH   : ',I8)
 1003 FORMAT(
     &  ' LC(LLSCAL  )  Next       link           : ',I8/
     &  ' LC(LLSCAL+1)  Up         link to STP?   : ',I8/
     &  ' LC(LLSCAL+2)  Origin     link           : ',I8)
 1004 FORMAT(
     &  ' IC(LLSCAL-5)  Bank number               : ',I8/
     &  ' IC(LLSCAL-4)  Bank name                 : ',A4/
     &  ' IC(LLSCAL-3)  NL                        : ',I8/
     &  ' IC(LLSCAL-2)  NS                        : ',I8/
     &  ' IC(LLSCAL-1)  ND                        : ',I8)
 1005 FORMAT(
     &  ' IC(LLSCAL+1)  Bank type                 : ',I8/
     &  ' IC(LLSCAL+2)  Status                    : ',I8/
     &  ' IC(LLSCAL+3)  Quality factor            : ',I8/
     &  ' IC(LLSCAL+4)  Lowest valid run number   : ',I8/
     &  ' IC(LLSCAL+5)  Highest valid run number  : ',I8/
     &  ' IC(LLSCAL+6)  Run number for generation : ',I8/
     &  ' IC(LLSCAL+7)  Date generated            : ',I8/
     &  ' IC(LLSCAL+8)  Type of run generated for : ',I8/
     &  '  C(LLSCAL+9)  Version number            : ',G8.2/
     &  ' IC(LLSCAL+10)                           : ',I8)
      END

      SUBROUTINE PRZUSR(PRUNIT,LUSERI,NUSER,CFL,IFL)
C------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-  User event printout.  Called from ZTRDMP if 'ZUSR' is requested in 
C_  ZTRAKS.RCP.   By adding additonal write statements or calling extra PRxxxx
C   user can add print out into event dump files.
C-  
C-
C-  INPUT:
C-       ( All input parameters except PRUNIT are dummy)
C-  PRUNIT= unit number for printout
C-  LUSERI= bank address (dummy)
C-  NUSER = bank number  (dummy)
C-  CFL   = flag to control printout   (dummy)
C-  IFL   = 0  print everything   (dummy)
C-
C-   Created 24-FEB-1992   Daria Zieminska   (based on PRMUSR)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER PRUNIT,LUSERI,NUSER,IFL
      CHARACTER CFL*(*)
C----------------------------------------------------------------------
C      WRITE(PRUNIT,*)
C
C  print out ISAJET banks 
C
C      CALL PRTEVZ(PRUNIT) 
C
  999 RETURN
      END

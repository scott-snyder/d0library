      SUBROUTINE DDGET(LOC,DNUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GETS A DOULE PRECISION NUMBER BEGINNING 
C-                         AT LOCATION LOC
C-                         IN STORE /ZEBSTP 
C-
C-   Inputs  : LOC
C-   Outputs : DNUM 
C-   Controls: 
C-
C-   Created  22-DEC-1990   Rajendran Raja
C-   Updated   7-APR-1995   Alan M. Jonckheere  
C-      Change calls DGET -> DDGET and DSET -> DDSET 
C-        to avoid conflict with new intrinsic functions
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LOC
      DOUBLE PRECISION DNUM
      DOUBLE PRECISION DTEMP
      REAL TEMP(2)
      EQUIVALENCE (TEMP,DTEMP)
C----------------------------------------------------------------------
      TEMP(1) = C(LOC)
      TEMP(2) = C(LOC+ 1)
      DNUM = DTEMP
      RETURN
C
      ENTRY DDSET(LOC,DNUM)
C
C ****  SETS A DOUBLE PRECISION NUMBER AT LOCATION LOC IN STORE /ZEBSTP/
C
      DTEMP = DNUM
      C(LOC) = TEMP(1)
      C(LOC + 1) = TEMP(2)
  999 RETURN
      END

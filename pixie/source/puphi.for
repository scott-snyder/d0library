      SUBROUTINE PUPHI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pick a PHI segment
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-
C-   Modified 23-APR-1991   Nobu Oshima( remove JET DPHI )
C-   Created  13-DEC-1988   Michael W. Peters
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL    PI,HALFPI,TWOPI,RAD
      REAL    XOUT,YOUT,PHI
      INTEGER ICHAR,ICAPHI,IDPHI
      INTEGER NBITEM,DEFITM
      CHARACTER*40 TXTITM
      CHARACTER*4 REM,CVAL
      CHARACTER*1 ICHOIC
      DATA    NBITEM/2/,DEFITM/2/
C----------------------------------------------------------------------
      DATA TXTITM/'Enter S = Select Phi or C = Continue >'/
      INCLUDE 'D0$INC:PI.INC'
      INCLUDE 'D0$INC:PXCOMK.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C----------------------------------------------------------------------
C
C ****  Getting Input from the user to select a cell or not
C
      CALL OUTMSG('1')
      CALL GETPAR(1,TXTITM,'U',ICHOIC) 
      IF(ICHOIC.EQ.'C') GO TO 999
C
C ****  Getting Phi
C
      CALL PUMESS('Click to select phi')
      CALL PULOCA(0.,0.,ICHAR,XOUT,YOUT)
      PHI=ATAN2(YOUT,XOUT)
      IF(PHI.LT.0.) PHI=PHI+TWOPI
      ICAPHI=IFIX((PHI/TWOPI)*NPHIL)+1
      ICAPHI=MIN(ICAPHI,NPHIL)
C-
C--- Impose up and down region in the CAL SIDE view
      IF(ICAPHI.GT.NPHIL/2) ICAPHI=ICAPHI-NPHIL/2
C-
      CALL PUSETV('CAL PHI',ICAPHI)
      CALL PUGETV('CAL JET DPHI',IDPHI)
      CALL PUSETV('CAL DPHI',IDPHI)
  999 RETURN
      END
     

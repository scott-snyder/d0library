      SUBROUTINE PRSTTH(PRUNIT,LSTTH,NMUT,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print bank 'STTH': SAMUS hits on track
C-
C-   Inputs  : PRUNIT - Unit number for printout
C-             LMUT   - Bank address
C-             NMUT   - Bank number
C-             CFL    - Flag to control print out
C-             IFL    - How much to print, 0 is complete
C-   Outputs :
C-   Controls:
C-
C-   Created:  7-MAY-1991 O.Eroshin
C-   Modified: 04-FEB-1995 I.Mandrichenko Changed format of STTH
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER N_STTH
      PARAMETER (N_STTH=8)
      INTEGER I,J,JJ,N,PRUNIT,LSTTH,NMUT,IFL,IM,IT
      CHARACTER CFL*(*)
C
      N = IQ(LSTTH-1)/N_STTH
      WRITE(PRUNIT,101) NMUT,N
  101 FORMAT('0',10X,'STTH bank for track #',I2,'   ',I4,' hits:'//)
      WRITE(PRUNIT,104)
  104 FORMAT(11X,' #MODULE/#TUBE (X,Y,Z) (Cx,Cy,Dz) DriftDistance:')
C
      J = LSTTH+1
      DO I = 1,N
        IM = IQ(J)/256
        IT = MOD(IQ(J),256)
        WRITE(PRUNIT,102) IM,IT,(Q(J+JJ),JJ=1,7)
  102   FORMAT(1X,I3,'/',I3,' (',F8.2,',',F8.2,',',F8.2,')',
     +         ' (',F6.3,',',F6.3,',',F6.3,')',5X,F6.3)
        J = J+N_STTH
      END DO
C
      RETURN
      END

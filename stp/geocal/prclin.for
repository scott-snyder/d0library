      SUBROUTINE PRCLIN(PRUNIT, JJCLIN, NCLIN, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the contents of one
C-                         or more CLIN banks.
C-
C-   Inputs  : PRUNIT    [I]   Unit number for printout
C-             JJCLIN    [I]   Pointer to one bank (CFL='ONE') or
C-                             to first bank of linear chain if
C-                             CFL = 'LINEAR'.
C-             NCLIN     [I]   bank number.  (not used in current 
C-                             version.
C-             CFL       [C*]  Character flag:
C-                             'ONE' : LCTHE points to bank.
C-                             'LINEAR' : LCTHE points to first bank.
C-             IFL       [I]   Defines level of printout: 0: full
C-                             1: minimum, 2: in between
C-   Outputs :                 on unit PRUNIT
C-   Controls:                 none
C-
C-   Created  20-MAR-1991   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:CLIN.PARAMS'
      INTEGER PRUNIT, JCLIN, NCLIN, IFL, IP, JJCLIN
      CHARACTER*(*) CFL
      INTEGER I
C
      JCLIN = JJCLIN
      DO WHILE (JCLIN .NE. 0) 
        WRITE( PRUNIT, 1100) JCLIN
 1100   FORMAT('0Printout for CLIN bank pointed to by LCLIN= ',I10)
        WRITE( PRUNIT, 1105) (IC(JCLIN-I),I=1,3),(LC(JCLIN-I), I=0,1)
 1105   FORMAT(' NL,NS,ND = ',3I5,' LC(LCLIN),LINK= ',2I10)
        WRITE( PRUNIT, 1110) (IC(JCLIN+I), I=1,10)
 1110   FORMAT(' Header Info : ',10I7)
        WRITE( PRUNIT, 1120) IC(JCLIN+IGMDID)
 1120   FORMAT(' Module Identification : ',I10)
        WRITE( PRUNIT, 1130) (C(JCLIN+I), I= IGDTX, IGDTZ)
 1130   FORMAT(' Module Deviation : ', 3G12.3)
        WRITE( PRUNIT, 1135) (C(JCLIN+I), I= IGMDLX, IGMDLZ)
 1135   FORMAT(' Nominal module position : ', 3G12.3)
        WRITE( PRUNIT, 1140) (C(JCLIN+I), I= IGR11, IGR33)
 1140   FORMAT(3(' Alignment Rotation : ', 3G12.3/))
        WRITE( PRUNIT, 1150) C(JCLIN+IGTCN), C(JCLIN+IGTCT)
 1150   FORMAT('+Normal and Transv Thermal Contraction : ', 2F10.5/)
        IF( CFL .EQ. 'ONE') THEN
          JCLIN = 0
        ELSE IF (CFL .EQ. 'LINEAR') THEN
          JCLIN = LC(JCLIN)
        END IF
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END

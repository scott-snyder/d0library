      SUBROUTINE PRCTHE(PRUNIT, JJCTHE, NCTHE, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the contents of one
C-                         or more CTHE banks.
C-
C-   Inputs  : PRUNIT    [I]   Unit number for printout
C-             JJCTHE    [I]   Pointer to one bank (CFL='ONE') or
C-                             to first bank of linear chain if
C-                             CFL = 'LINEAR'.
C-             NCTHE     [I]   bank number.  (not used in current
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
      INCLUDE 'D0$INC:CALOR_SURVEY.INC'
      INTEGER PRUNIT, JCTHE, NCTHE, IFL, IP, JJCTHE, NDATAW
      CHARACTER*(*) CFL
C
      JCTHE = JJCTHE
      NDATAW = IC( JCTHE-1)         ! number of data words in bank
      DO WHILE (JCTHE .NE. 0)
        WRITE( PRUNIT, 1100) JCTHE
 1100   FORMAT('0Printout for CTHE bank pointed to by LCTHE= ',I10)
        WRITE( PRUNIT, 1110) C(JCTHE+ISMODL), IC(JCTHE+ISNUMB),
     +     IC(JCTHE+ISNMSR)
 1110   FORMAT(' Module type, Kroon Location Numbr, Number of meas :',
     &     A4,2I5)
        WRITE( PRUNIT, 1120) C(JCTHE+ISMCOD)
 1120   FORMAT(' Code indicating which measurements were done :',
     &     Z8)
        WRITE( PRUNIT, 1130) (C(JCTHE+IP),IP=ISNILX, NDATAW)
 1130   FORMAT(8(' CORNER COORDINATES : ',4F12.3/))
        IF( CFL .EQ. 'ONE') THEN
          JCTHE = 0
        ELSE IF (CFL .EQ. 'LINEAR') THEN
          JCTHE = LC(JCTHE)
        END IF
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END

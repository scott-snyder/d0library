      SUBROUTINE PRCMDL(PRUNIT, JJCMDL, NCMDL, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the contents of one
C-                         or more CMDL banks.
C-
C-   Inputs  : PRUNIT    [I]   Unit number for printout
C-             JJCMDL    [I]   Pointer to one bank (CFL='ONE') or
C-                             to first bank of linear chain if
C-                             CFL = 'LINEAR'.
C-             NCMDL     [I]   bank number.  (not used in current
C-                             version.
C-             CFL       [C*]  Character flag:
C-                             'ONE' : LCMDL points to bank.
C-                             'LINEAR' : LCMDL points to first bank.
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
      INTEGER PRUNIT, JCMDL, NCMDL, IFL, IP, JJCMDL, NDATAW
      CHARACTER*(*) CFL
C
      JCMDL = JJCMDL
      NDATAW = IC(JCMDL-1)             ! number of words in bank
      DO WHILE (JCMDL .NE. 0)
        WRITE( PRUNIT, 1100) JCMDL
 1100   FORMAT('0Printout for CMDL bank pointed to by LCMDL= ',I10)
        WRITE( PRUNIT, 1110) C(JCMDL+ISMODL), IC(JCMDL+ISNUMB),
     +     IC(JCMDL+ISNMSR)
 1110   FORMAT(' Module type, Kroon Location Numbr, Number of meas :',
     &     A4,2I5)
        WRITE( PRUNIT, 1120) C(JCMDL+ISMCOD)
 1120   FORMAT(' Code indicating which measurements were done :',
     &     Z8)
        WRITE( PRUNIT, 1130) (C(JCMDL+IP),IP=ISNILX, NDATAW)
 1130   FORMAT(8(' CORNER COORDINATES : ',4F12.3/))
        IF( CFL .EQ. 'ONE') THEN
          JCMDL = 0
        ELSE IF (CFL .EQ. 'LINEAR') THEN
          JCMDL = LC(JCMDL)
        END IF
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END

      SUBROUTINE PRJNEP ( PRUNIT, LJNEP, NJNEP, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of bank
C-                        'JNEP' pointed to by LJNEP.  Abort printing
C-                         if pointer LJNEP is invalid.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LJNEP  [I] : Pointer to the JNEP bank
C-             NJNEP  [I] : Bank number, NOT USED
C-             CFL    [C*]: Character flag, NOT USED
C-             IFL    [I] : Defines the amount of printing: NOT USED
C-
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created   22-NOV-1991     Dhiman Chakraborty
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER PRUNIT, GZJNEP, LJNEP, NJNEP, IFL
      CHARACTER*(*) CFL
      INTEGER J
      CHARACTER*4 CHAR4
      INTEGER ICHAR4
      EQUIVALENCE (ICHAR4,CHAR4)
      DATA     CHAR4 /'JNEP'/
C----------------------------------------------------------------------
      IF( LJNEP .LE. 0 .OR. IQ(LJNEP-4) .NE. ICHAR4) GOTO 666
C  ***  Print the content of the bank pointed by LJNEP
C
      WRITE( PRUNIT, 1100 )  IQ(LJNEP + 1),
     &  (Q(LJNEP+J),J=2,16)
 1100 FORMAT(//,' Contents of JNEP bank ',/,
     &  ' +1     I       bank version no.        ',I12,/,
     &  '  2     F       Ex                      ',F15.7,/,
     &  '  3     F       Ey                      ',F15.7,/,
     &  '  4     F       Ez                      ',F15.7,/,
     &  '  5     F       E                       ',F15.7,/,
     &  '  6     F       Et                      ',F15.7,/,
     &  '  7     F       Theta                   ',F15.7,/,
     &  '  8     F       Phi                     ',F15.7,/,
     &  '  9     F       Eta                     ',F15.7,/,
     &  ' 10     F       sig**2(Ex)              ',F15.7,/,
     &  ' 11     F       sig**2(Ey)              ',F15.7,/,
     &  ' 12     F       RMS eta width           ',F15.7,/,
     &  ' 13     F       RMS phi width           ',F15.7,/,
     &  ' 14     F       Fraction EM_ET/TOTAL_ET ',F15.7,/,
     &  ' 15     I       Flag for merg/split     ',I12,/,
     &  ' 16     F       Fraction E(JNEP)/E(JETS)',F15.7,//)
      GOTO 999
  666 WRITE(PRUNIT,*)'PRJNEP: Problem with LJNEP, printing aborted'
  999 RETURN
      END

      SUBROUTINE PRPPHO ( PRUNIT, LPPHO, NPPHO, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'PPHO'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LPPHO  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NPPHO  [I] : Bank number, used only if CFL='ONE' and LPPHO = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LPPHO point to a bank, or if <0, NPPHO is
C-                                  the bank number.
C-                          'LINEAR' : LPPHO points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created   6-APR-1990 11:48:48.44  Rajendran Raja
C-   Updated  18-SEP-1992   Rajendran Raja  ADDED SHOWER CENTER PRINTING 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPPHO.LINK'
      INCLUDE 'D0$LINKS:IZHMTP.LINK'
C
      INTEGER PRUNIT, LPPHO, NPPHO, IFL
      INTEGER LHMTP,NHMTP
      CHARACTER*(*) CFL
      INTEGER LPPHO1, GZPPHO, LZLOC, J
C----------------------------------------------------------------------
      LPPHO1 = LPPHO
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LPPHO .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LPPHO .LE. 0 ) THEN
          IF( NPPHO .EQ. 0 ) GOTO 980          ! Error exit
          LPPHO1 = LZLOC( IXMAIN, 'PPHO', NPPHO )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LPPHO1 = GZPPHO( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRPPHO ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
      IF(LPPHO1.LE.0) GOTO 999
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LPPHO1
C
      WRITE( PRUNIT, 1100 )  IQ(LPPHO1 + 1), IQ(LPPHO1+2),
     &  (Q(LPPHO1+J),J=3,25)
 1100 FORMAT(//,' Contents of PPHO bank ',/,
     &  ' +1     I       bank version no. ',I6,/,
     &  '  2     I       id               ',I6,/,
     &  '  3     F       Ex               ',G15.6,/,
     &  '  4     F       Ey               ',G15.6,/,
     &  '  5     F       Ez               ',G15.6,/,
     &  '  6     F       E                ',G15.6,/,
     &  '  7     F       Et               ',G15.6,/,
     &  '  8     F       theta            ',G15.6,/,
     &  '  9     F       eta              ',G15.6,/,
     &  ' 10     F       phi              ',G15.6,/,
     &  ' 11     F       (sigEx)**2       ',G15.6,/,
     &  ' 12     F       (sigEy)**2       ',G15.6,/,
     &  ' 13     F        sigEt           ',G15.6,/,
     &  ' 14     F       Trans. EM energy ',G15.6,/,
     &  ' 15     F       Energy in core   ',G15.6,/,
     &  ' 16     F       Energy in isol.  ',G15.6,/,
     &  ' 17     F       EM core energy   ',G15.6,/,
     &  ' 18     F       EM isol. energy  ',G15.6,/,
     &  ' 19     F       CALORIMETER ETA  ',G15.6,/,
     &  ' 20     F       X OF SHOWER CENTER used in roadmaking  ',
     &  G15.6,/,
     &  ' 21     F       Y OF SHOWER CENTER used in roadmaking  ',
     &  G15.6,/,
     &  ' 22     F       Z OF SHOWER CENTER used in roadmaking  ',
     &  G15.6,/,
     &  ' 23     F       spare  ',G15.6,/,
     &  ' 24     F       spare  ',G15.6,/,
     &  ' 25     F       spare  ',G15.6,//)
C
C ****  Now look for HMTP bank
C
      LHMTP = LQ(LPPHO1-IZHMTP)
      IF(LHMTP .NE. 0) THEN
        CALL PRHMTP(PRUNIT,LHMTP,NHMTP,'ONE',IFL)
      ENDIF      
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      LPPHO1 = LQ( LPPHO1 )
      IF( LPPHO1 .NE. 0 ) GOTO 1
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LPPHO
 2000 FORMAT(/' ** PRPPHO ** called for LINEAR without valid bank '
     &     ,   'pointer, LPPHO =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LPPHO, NPPHO
 2100 FORMAT(/'  ** PRPPHO ** called for ONE without bank pointer and '
     &     ,   'bank number, LPPHO =',I10,' NPPHO =', I10/)
      GOTO 999
      END

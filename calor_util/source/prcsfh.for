      SUBROUTINE PRCSFH ( PRUNIT, LCSFH, NCSFH, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'CSFH'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LCSFH  [I] :  Pointer to the first of a linear structure 
C-                          Unused if CFL = 'ALL'.
C-             NCSFH  [I] : Bank number, used only if CFL='ONE' and LCSFH = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'LINEAR' : LCSFH points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created   3-MAR-1992 09:30:41.38  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER PRUNIT, LCSFH, NCSFH, IFL
      CHARACTER*(*) CFL
      INTEGER LCSFH1, GZCSFH, LZLOC, J
C----------------------------------------------------------------------
      LCSFH1 = LCSFH
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LCSFH .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LCSFH1 = GZCSFH( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRCSFH ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LCSFH1
C
      WRITE( PRUNIT, 1100 )  IC( LCSFH1 + 1 ) 
      WRITE( PRUNIT, 1101 ) ( C( LCSFH1 + J ) , J = 2, IC( LCSFH1-1) )
 1100 FORMAT(' CSFH BANK ',/1x,' VERSION ',I2)
 1101 FORMAT(' CCEM  A',1PE10.3E1,' ECEM  A',1PE10.3E1,
     &       ' CCMG  A',1PE10.3E1,' ICD   A',1PE10.3E1,
     &       ' ECMG  A',1PE10.3E1,' CCFH  A',1PE10.3E1,
     &       ' ECIH  A',1PE10.3E1,' ECMH  A',1PE10.3E1,
     &       ' CCCH  A',1PE10.3E1,' ECOH  A',1PE10.3E1)
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'LINEAR' ) THEN
        LCSFH1 = LC( LCSFH1 )
        IF( LCSFH1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LCSFH1 = GZCSFH()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LCSFH
 2000 FORMAT(/' ** PRCSFH ** called for LINEAR without valid bank '
     & ,       'pointer, LCSFH =',I10/)
      GOTO 999
      END

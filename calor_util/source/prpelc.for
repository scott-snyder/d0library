      SUBROUTINE PRPELC ( PRUNIT, LPELC, NPELC, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'PELC'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LPELC  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NPELC  [I] : Bank number, used only if CFL='ONE' and LPELC = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LPELC point to a bank, or if <0, NPELC is
C-                                  the bank number.
C-                          'LINEAR' : LPELC points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created   6-APR-1990 11:45:28.16  Rajendran Raja
C-   Updated  18-SEP-1992   Rajendran Raja  ADDED SHOWER CENTER PRINTING 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPELC.LINK'
      INCLUDE 'D0$LINKS:IZHMTE.LINK'
C
      INTEGER PRUNIT, LPELC, NPELC, IFL
      INTEGER LHMTE,NHMTE
      CHARACTER*(*) CFL
      INTEGER LPELC1, GZPELC, LZLOC, J
C----------------------------------------------------------------------
      LPELC1 = LPELC
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LPELC .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LPELC .LE. 0 ) THEN
          IF( NPELC .EQ. 0 ) GOTO 980          ! Error exit
          LPELC1 = LZLOC( IXMAIN, 'PELC', NPELC )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LPELC1 = GZPELC( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRPELC ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
      IF(LPELC1.LE.0) GOTO 999
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LPELC1
C
      WRITE( PRUNIT, 1100 )  IQ(LPELC1 + 1), IQ(LPELC1+2),
     &  (Q(LPELC1+J),J=3,25)
 1100 FORMAT(//,' Contents of PELC bank ',/,
     &  ' +1     I     bank version no.      ',I5,/,
     &  '  2     I     id                    ',I5,/,
     &  '  3     F     Ex                    ',G15.4,/,
     &  '  4     F     Ey                    ',G15.4,/,
     &  '  5     F     Ez                    ',G15.4,/,
     &  '  6     F     E                     ',G15.4,/,
     &  '  7     F     Et                    ',G15.4,/,
     &  '  8     F     theta                 ',G15.4,/,
     &  '  9     F     eta                   ',G15.4,/,
     &  ' 10     F     phi                   ',G15.4,/,
     &  ' 11     F     (sigEx)**2            ',G15.4,/,
     &  ' 12     F     (sigEy)**2            ',G15.4,/,
     &  ' 13     F     sigEt                 ',G15.4,/,
     &  ' 14     F     Trans. energy in clus ',G15.4,/,
     &  ' 15     F     Core cone energy      ',G15.4,/,
     &  ' 16     F     Isolation cone en.    ',G15.4,/,
     &  ' 17     F     EM core energy        ',G15.4,/,
     &  ' 18     F     EM isol energy        ',G15.4,/,
     &  ' 19     F     Spare                 ',G15.4,/,
     &  ' 20     F     Spare                 ',G15.4,/,
     &  ' 21     F     No. of ZTRKs in road  ',G15.4,/,
     &  ' 22     F     DCLA of ZTRK in road  ',G15.4,/,
     &  ' 23     F     X OF SHOWER CENTER used in roadmaking ',G15.4,/,
     &  ' 24     F     Y OF SHOWER CENTER used in roadmaking ',G15.4,/,
     &  ' 25     F     Z OF SHOWER CENTER used in roadmaking ',G15.4,//)

C ****  Now look for HMTE bank
C
      LHMTE = LQ(LPELC1-IZHMTE)
      IF(LHMTE .NE. 0) THEN
        CALL PRHMTE(PRUNIT,LHMTE,NHMTE,'ONE',IFL)
      ENDIF
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
        LPELC1 = LQ( LPELC1 )
        IF( LPELC1 .NE. 0 ) GOTO 1
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LPELC
 2000 FORMAT(/' ** PRPELC ** called for LINEAR without valid bank '
     &       , 'pointer, LPELC =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LPELC, NPELC
 2100 FORMAT(/'  ** PRPELC ** called for ONE without bank pointer and '
     &      ,  'bank number, LPELC =',I10,' NPELC =', I10/)
      GOTO 999
      END

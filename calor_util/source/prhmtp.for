      SUBROUTINE PRHMTP ( PRUNIT, LHMTP, NHMTP, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'HMTP'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LHMTP  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NHMTP  [I] : Bank number, used only if CFL='ONE' and LHMTP = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LHMTP point to a bank, or if <0, NHMTP is
C-                                  the bank number.
C-                          'LINEAR' : LHMTP points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  25-SEP-1990 12:23:38.33  Norman A. Graf
C-   Updated  18-SEP-1992   Rajendran Raja  added shower center printout 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZHMTP.LINK'
C
      INTEGER PRUNIT, LHMTP, NHMTP, IFL
      CHARACTER*(*) CFL
      INTEGER LHMTP1, GZHMTP, LZLOC, J
C----------------------------------------------------------------------
      LHMTP1 = LHMTP
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LHMTP .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LHMTP .LE. 0 ) THEN
          IF( NHMTP .EQ. 0 ) GOTO 980          ! Error exit
          LHMTP1 = LZLOC( IXMAIN, 'HMTP', NHMTP )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LHMTP1 = GZHMTP( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRHMTP ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LHMTP1
C
      WRITE( PRUNIT, 1100 )  IQ(LHMTP1 + 1), IQ(LHMTP1+2),
     &                      IQ(LHMTP1 + 3), IQ(LHMTP1+4),
     &  (Q(LHMTP1+J),J=5,12)
 1100 FORMAT(//,' Contents of HMTP bank ',/,
     &  ' +1     I       bank version no.      ',I12,/,
     &  '  2     I (0 no hmatrix;1 long;2 full)',I12,/,
     &  '  3     F Longitudinal Hmatrix dim.   ',I12,/,
     &  '  4     F Transverse   Hmatrix dim.   ',I12,/,
     &  '  5     F Chisquared for Long. matrix ',F15.7,/,
     &  '  6     F "Probability" for Long      ',F15.7,/,
     &  '  7     F Chisquared for full         ',F15.7,/,
     &  '  8     F "Probability" for full      ',F15.7,/,
     &  '  9     F Lower Phi limit for Road    ',F15.7,/,
     &  ' 10     F Upper Phi limit for Road    ',F15.7,/,
     &  ' 11     F Lower Theta limit for Road  ',F15.7,/,
     &  ' 12     F Upper Theta limit for Road  ',F15.7,/,
     &  ' 13     F X OF SHOWER CENTER from H Matrix ',F15.7,/,
     &  ' 14     F Y OF SHOWER CENTER from H Matrix ',F15.7,/,
     &  ' 15     F Z OF SHOWER CENTER from H Matrix ',F15.7,//)
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LHMTP1 = LQ( LHMTP1 )
        IF( LHMTP1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LHMTP1 = GZHMTP()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LHMTP
 2000 FORMAT(/' ** PRHMTP ** called for LINEAR without valid bank '
     &        ,'pointer, LHMTP =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LHMTP, NHMTP
 2100 FORMAT(/'  ** PRHMTP ** called for ONE without bank pointer and '
     &        ,'bank number, LHMTP =',I10,' NHMTP =', I10/)
      GOTO 999
      END

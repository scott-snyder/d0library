      SUBROUTINE PRPTAU ( PRUNIT, LPTAU, NPTAU, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'PTAU'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LPTAU  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NPTAU  [I] : Bank number, used only if CFL='ONE' and LPTAU = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LPTAU point to a bank, or if <0, NPTAU is
C-                                  the bank number.
C-                          'LINEAR' : LPTAU points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created   1-NOV-1990 11:45:28.16  Serban Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPTAU.LINK'
C
      INTEGER PRUNIT, LPTAU, NPTAU, IFL,IL(5),LR(5),NRL
      CHARACTER*(*) CFL
      INTEGER LPTAU1, GZPTAU, LZLOC, J, I
C----------------------------------------------------------------------
      LPTAU1 = LPTAU
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LPTAU .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LPTAU .LE. 0 ) THEN
          IF( NPTAU .EQ. 0 ) GOTO 980          ! Error exit
          LPTAU1 = LZLOC( IXMAIN, 'PTAU', NPTAU )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LPTAU1 = GZPTAU( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRPTAU ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
      IF(LPTAU1.LE.0) GOTO 999
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LPTAU1
C
      WRITE( PRUNIT, 1100 )  IQ(LPTAU1 + 1), IQ(LPTAU1+2),
     &  (Q(LPTAU1+J),J=3,11)
 1100 FORMAT(//,' Contents of PTAU bank ',/,
     &  ' +1     I       bank version no.      ',I12,/,
     &  '  2     I       id                    ',I12,/,
     &  '  3     F       Ex                    ',F15.7,/,
     &  '  4     F       Ey                    ',F15.7,/,
     &  '  5     F       Ez                    ',F15.7,/,
     &  '  6     F       E                     ',F15.7,/,
     &  '  7     F       Et                    ',F15.7,/,
     &  '  8     F       theta                 ',F15.7,/,
     &  '  9     F       phi                   ',F15.7,/,
     &  ' 10     F       eta                   ',F15.7,/,
     &  ' 11     F       R(rms)                ',F15.7,/)
C
C  ***  print which jet and tracks are part of tau
C
      NRL=IQ(LPTAU1-3)-1
      DO J=1,NRL
        IL(J)=0
        LR(J)=LQ(LPTAU1-J-1)
        IF(LR(J).NE.0) IL(J)=IQ(LR(J)-5)
      ENDDO
      WRITE(PRUNIT,1101) (IL(J),J=1,NRL)
 1101 FORMAT(' Jet no.',I3,', track nos.',3I3,//)
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
        LPTAU1 = LQ( LPTAU1 )
        IF( LPTAU1 .NE. 0 ) GOTO 1
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LPTAU
 2000 FORMAT(/' ** PRPTAU ** called for LINEAR without valid bank '
     &        ,'pointer, LPTAU =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LPTAU, NPTAU
 2100 FORMAT(/'  ** PRPTAU ** called for ONE without bank pointer and '
     &        ,'bank number, LPTAU =',I10,' NPTAU =', I10/)
      GOTO 999
      END

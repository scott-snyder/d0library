      SUBROUTINE PRFRES ( PRUNIT, LFRES, NFRES, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'FRES'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LFRES  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NFRES  [I] : Bank number, used only if CFL='ONE' and LFRES = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LFRES point to a bank, or if <0, NFRES is
C-                                  the bank number.
C-                          'LINEAR' : LFRES points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  22-OCT-1991   James T. Linnemann
C-   Updated  20-AUG-1992   sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFRES.LINK'
C
      INTEGER PRUNIT, LFRES, NFRES, IFL
      CHARACTER*(*) CFL
      INTEGER LFRES1, GZFRES, LZLOC, J, IWORD,ITOOL,PAR_SET
      BYTE LAST_USED(2,0:127)
C----------------------------------------------------------------------
      LFRES1 = LFRES
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LFRES .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LFRES .LE. 0 ) THEN
          IF( NFRES .EQ. 0 ) GOTO 980          ! Error exit
          LFRES1 = LZLOC( IXMAIN, 'FRES', NFRES )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
        LFRES1 = GZFRES( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
 1000   FORMAT(/' ** PRFRES ** Illegal value of CFL = ',A10/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LFRES1
C
      WRITE( PRUNIT, 1100 ) IQ( LFRES1 + 1 )
 1100 FORMAT(/' FRES: Version = ',I3)
      WRITE( PRUNIT, 1110 ) (J, LQ( LFRES1 + J ) , J = -15,-1)
 1110 FORMAT(' Link',I4,' =',I10)
      IF (IQ(LFRES1 + 1).GE.2) THEN
        CALL UCOPY(IQ(LFRES+2),LAST_USED,64)
        DO J = 1,128
          IF((IFL.GT.1).OR.(LAST_USED(1,J-1).NE.0))THEN
            WRITE (PRUNIT, 1120) J-1, LAST_USED(1,J-1),LAST_USED(2,J-1)
 1120       FORMAT(' L2 Bit',I4,': Last tool run = ',I3,
     &        ' with Parameter Set',I3)
          ENDIF
        ENDDO
      ENDIF
C&IF VAXVMS,VAXELN
C&ELSE
C&        IF (IFL.GT.1) WRITE(PRUNIT,1125)
C&1125   FORMAT(' Beware: order and bit number scrambled in UNIX')
C&ENDIF
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LFRES1 = LQ( LFRES1 )
        IF( LFRES1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LFRES1 = GZFRES()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LFRES
 2000 FORMAT(/' ** PRFRES ** called for LINEAR without valid bank ',
     &        'pointer, LFRES =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LFRES, NFRES
 2100 FORMAT(/'  ** PRFRES ** called for ONE without bank pointer and ',
     &        'bank number, LFRES =',I10,' NFRES =', I10/)
      GOTO 999
      END

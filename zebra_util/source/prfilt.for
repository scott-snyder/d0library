      SUBROUTINE PRFILT ( PRUNIT, LFILT, NFILT, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'FILT'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LFILT  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NFILT  [I] : Bank number, used only if CFL='ONE' and LFILT = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LFILT point to a bank, or if <0, NFILT is
C-                                  the bank number.
C-                          'LINEAR' : LFILT points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-                                    (IFL is IGNORED)
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created 7-MAR-1992   James T. Linnemann
C-   Updated 20-AUG-1992  sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFILT.LINK'
C
      INTEGER PRUNIT, LFILT, NFILT, IFL, LO
      CHARACTER*(*) CFL
      INTEGER LFILT1, GZFILT, LZLOC, J, NMAX
      CHARACTER*25 MSG
C----------------------------------------------------------------------
      LFILT1 = LFILT
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LFILT .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LFILT .LE. 0 ) THEN
          IF( NFILT .EQ. 0 ) GOTO 980          ! Error exit
          LFILT1 = LZLOC( IXMAIN, 'FILT', NFILT )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LFILT1 = GZFILT( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
 1000   FORMAT(/' ** PRFILT ** Illegal value of CFL = ',A/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LFILT1
C
      WRITE(PRUNIT,105) IQ(LFILT1+1)
  105 FORMAT(20X,'FILT BANK',/,
     &' Version = ',I5/)
      LO = LFILT1+1
      WRITE(PRUNIT,110)'SET (L1 bits on)',(IQ(LO+J),J=4,1,-1)
  110 FORMAT('    FILTER BITS ',A, 4(Z8.8,1X))
      LO = LO + 4
      WRITE(PRUNIT,110)'TRIED           ',(IQ(LO+J),J=4,1,-1)
      LO = LO + 4
      WRITE(PRUNIT,110)'PASSED          ',(IQ(LO+J),J=4,1,-1)
      IF (IQ(LFILT1+1).GE.2) THEN
        LO = LO + 4
        WRITE(PRUNIT,110)'UNBIASED        ',(IQ(LO+J),J=4,1,-1)
      ENDIF
      IF (IQ(LFILT1+1).GE.6) THEN
        LO = LO + 4
        WRITE(PRUNIT,120) Q(LO),(IQ(LHEAD+J),J=2,3)
  120   FORMAT(' Level 2 processing time = ',F10.4,' sec in node ',2A4)
        MSG = 'Processing completed normally.'
        IF ( IQ(LO+1).EQ.0 ) THEN
          WRITE(PRUNIT,130)
  130     FORMAT(' Processing completed normally.')
        ELSE
          WRITE(PRUNIT,140)(IQ(LO+J),J=2,3)
  140     FORMAT(' Processing hit time limit during L2 bit',I4,
     &      ', Tool',I4)
        ENDIF
      ENDIF
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LFILT1 = LQ( LFILT1 )
        IF( LFILT1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LFILT1 = LQ( LFILT1 )
        IF (LFILT1.NE.0) GO TO 1
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LFILT
 2000 FORMAT(/' ** PRFILT ** called for LINEAR without valid bank ',
     &        'pointer, LFILT =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LFILT, NFILT
 2100 FORMAT(/'  ** PRFILT ** called for ONE without bank pointer and',
     &        ' bank number, LFILT =',I10,' NFILT =', I10/)
      GOTO 999
      END

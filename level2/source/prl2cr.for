      SUBROUTINE PRL2CR ( PRUNIT, LL2CR, NL2CR, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'L2CR'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LL2CR  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NL2CR  [I] : Bank number, used only if CFL='ONE' and LL2CR = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LL2CR point to a bank, or if <0, NL2CR is
C-                                  the bank number.
C-                          'LINEAR' : LL2CR points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  25-NOV-90         Richard V Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZL2CR.LINK'
C
      INTEGER PRUNIT, LL2CR, NL2CR, IFL
      CHARACTER*(*) CFL
      INTEGER LL2CR1, GZL2CR, LZLOC, J
      INTEGER NCIETA,NCLAYER,I,K,NTOT,IPOINT
C----------------------------------------------------------------------
      LL2CR1 = LL2CR
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LL2CR .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LL2CR .LE. 0 ) THEN
          IF( NL2CR .EQ. 0 ) GOTO 980          ! Error exit
          LL2CR1 = LZLOC( IXMAIN, 'L2CR', NL2CR )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LL2CR1 = GZL2CR( )
      ELSE
        WRITE( PRUNIT, 1000 ) 
 1000   FORMAT(' ** PRL2CR ** Illegal value of CFL')
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LL2CR1
C
      WRITE(PRUNIT, *)
     &  ' Dumping L2 COSMIC RAY bank: L2CR  Version number = '
     & , IC(LL2CR1+ 1)
      NCIETA = IC(LL2CR1+2)
      NCLAYER= IC(LL2CR1+3)
      IPOINT = 5
C---Dump RBEG
      WRITE(PRUNIT,*) ' I,RBEG(I) = transverse distance to that layer '
      WRITE(PRUNIT,*) ' along trajectory of muon. (goes in - direction)'

      DO I = 1, 2*NCLAYER + 2
        WRITE(PRUNIT,*)' ',I,C(LL2CR1+IPOINT + I - 1)
      END DO
      IPOINT = IPOINT + 2*NCLAYER + 2
C---Dump  Z begin positions per eta for each layer
      NTOT = 2*NCIETA + 2
  110 FORMAT(' ',I3,26E10.2)
      WRITE(PRUNIT,*)' ZBEGIN for each eta for each layer...'
      WRITE(PRUNIT,*)' LAY #  ZBEGIN(- ............+eta) '
      DO I = 1, 2*NCLAYER + 2
        WRITE(PRUNIT,110)I,(C(LL2CR1+IPOINT + K -1), K = 1,8)
        WRITE(PRUNIT,110)I,(C(LL2CR1+IPOINT + K -1), K = 9,16)
        WRITE(PRUNIT,110)I,(C(LL2CR1+IPOINT + K -1), K = 16,24)
        WRITE(PRUNIT,110)I,(C(LL2CR1+IPOINT + K -1), K = 24,2*NCIETA+2)
        IPOINT = IPOINT + 2*NCIETA + 2
      END DO
C---Dump  Z end positions per eta for each layer
      WRITE(PRUNIT,*)' ZEND for each eta for each layer...'
      WRITE(PRUNIT,*)' LAY #  ZEND(- ............+eta) '
      DO I = 1, 2*NCLAYER + 2
        WRITE(PRUNIT,110)I,(C(LL2CR1+IPOINT + K -1), K = 1,8)
        WRITE(PRUNIT,110)I,(C(LL2CR1+IPOINT + K -1), K = 9,16)
        WRITE(PRUNIT,110)I,(C(LL2CR1+IPOINT + K -1), K = 16,24)
        WRITE(PRUNIT,110)I,(C(LL2CR1+IPOINT + K -1), K = 24,2*NCIETA+2)
        IPOINT = IPOINT + 2*NCIETA + 2
      END DO
C---Read in the ILAYER Id for each of our 'layer' indices:
      WRITE(PRUNIT,*)' Real offline ILYR each layer corresponds to'
      DO I = 1,2*NCLAYER + 2
        WRITE(PRUNIT,*)' ',I,IC(LL2CR1+IPOINT + I -1)
      END DO
      IPOINT = IPOINT + 2*NCLAYER + 2
C---Read in the IETA id for each of our eta indices:
      WRITE(PRUNIT,*)' Real offline IETA each eta label corresponds to'
      DO I = 1,2*NCIETA + 2
        WRITE(PRUNIT,*)' ',I,IC(LL2CR1+IPOINT + I -1 )
      END DO
      IPOINT = IPOINT + 2*NCIETA + 2
C---Count number of words of this bank and compare:
      IPOINT = IPOINT - 1
      WRITE(PRUNIT,*)' Total number of words in bank is ',IPOINT
C************** END OF DUMP ***************************************
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LL2CR1 = LQ( LL2CR1 )
        IF( LL2CR1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LL2CR1 = GZL2CR()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LL2CR
 2000 FORMAT(' ** PRL2CR ** called for LINEAR without valid bank ',
     &        'pointer, LL2CR =',I10)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LL2CR, NL2CR
 2100 FORMAT('  ** PRL2CR ** called for ONE without bank pointer and ',
     &        'bank number, LL2CR =',I10,' NL2CR =', I10)
      GOTO 999
      END

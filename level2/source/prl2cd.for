      SUBROUTINE PRL2CD ( PRUNIT, LL2CD, NL2CD, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'L2CD'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LL2CD  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NL2CD  [I] : Bank number, used only if CFL='ONE' and LL2CD = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LL2CD point to a bank, or if <0, NL2CD is
C-                                  the bank number.
C-                          'LINEAR' : LL2CD points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created   9-APR-92         D Claes
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZL2CD.LINK'
      INCLUDE 'D0$PARAMS:L2CD.PARAMS'
C
      INTEGER PRUNIT, LL2CD, NL2CD, IFL
      CHARACTER*(*) CFL
      INTEGER LL2CD1, GZL2CD, LZLOC, J
      INTEGER I, NROAD, POINT
C----------------------------------------------------------------------
      LL2CD1 = LL2CD
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LL2CD .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LL2CD .LE. 0 ) THEN
          IF( NL2CD .EQ. 0 ) GOTO 980          ! Error exit
          LL2CD1 = LZLOC( IXMAIN, 'L2CD', NL2CD )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LL2CD1 = GZL2CD( )
      ELSE
        WRITE( PRUNIT, 1000 ) 
 1000   FORMAT(' ** PRL2CD ** Illegal value of CFL')
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LL2CD1
C
      WRITE(PRUNIT, *)
     &  ' Dumping L2 Tracking results bank: L2CD  Version number = '
     & , IQ(LL2CD1+ 1)
C---Dump RBEG
      WRITE(PRUNIT,*) ' Final STATUS of hit-finding = '
     & , IQ(LL2CD1+PSTAT)
      WRITE(PRUNIT,*) ' ETA, PHI of L2_EM candidate'
      WRITE(PRUNIT,*)' ', Q(LL2CD1+PETA), Q(LL2CD1+PPHI)
      WRITE(PRUNIT,*) ' Road limits: Delta_ETA, Delta_PHI '
      WRITE(PRUNIT,*)' ', Q(LL2CD1+PDETA), Q(LL2CD1+PDPHI)
C
      NROAD = IQ(LL2CD1+PNCELL)
      WRITE(PRUNIT,*) ' Number of cells intercepted by road '
      WRITE(PRUNIT,*)' ', NROAD
C
      DO I = 1, NROAD
        POINT = LL2CD + NTOP_L2CD + (I-1)*NREP_L2CD
        WRITE(PRUNIT,*) ' I, LAYER, SECTOR, Nmbr of wires w/ hits '
        WRITE(PRUNIT,*) I, IQ(POINT+1), IQ(POINT+2), IQ(POINT+3)
        WRITE(PRUNIT,*)
     &    'Number of hits in road on INNER,OUTER sense wires'
        WRITE(PRUNIT,*) I, IQ(POINT+13), IQ(POINT+14)
        WRITE(PRUNIT,*)
     &    'Nmbr delay line hits seen BOTTOM delay (LEFT,RIGHT
     &    readout) '
        WRITE(PRUNIT,*) I, IQ(POINT+15), IQ(POINT+16)
        WRITE(PRUNIT,*)
     &    'Nmbr delay line hits seen BOTTOM delay (LEFT,RIGHT
     &    readout) '
        WRITE(PRUNIT,*) I, IQ(POINT+17), IQ(POINT+18)
      ENDDO
C************** END OF DUMP ***************************************
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LL2CD1 = LQ( LL2CD1 )
        IF( LL2CD1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LL2CD1 = GZL2CD()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LL2CD
 2000 FORMAT(' ** PRL2CD ** called for LINEAR without valid bank ',
     &        'pointer, LL2CD =',I10)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LL2CD, NL2CD
 2100 FORMAT('  ** PRL2CD ** called for ONE without bank pointer and ',
     &        'bank number, LL2CD =',I10,' NL2CD =', I10)
      GOTO 999
      END

      SUBROUTINE PRLEGO ( PRUNIT, LLEGO, NLEGO, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'LEGO'. 
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LLEGO  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NLEGO  [I] : Bank number, used only if CFL='ONE' and LLEGO = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LLEGO point to a bank, or if <0, NLEGO is
C-                                  the bank number.
C-                          'LINEAR' : LLEGO points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, .NE.0 print only title
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  15-FEB-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER PRUNIT, LLEGO, NLEGO, IFL
      CHARACTER*(*) CFL
      INTEGER LLEGO1, GZLEGO, LZLOC, J, ND
C----------------------------------------------------------------------
      LLEGO1 = LLEGO
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LLEGO .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LLEGO .LE. 0 ) THEN
          IF( NLEGO .EQ. 0 ) GOTO 980          ! Error exit
          LLEGO1 = GZLEGO(NLEGO)
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LLEGO1 = GZLEGO(1)
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
        GOTO 999
      ENDIF
C
C          Print titles
C
      WRITE(PRUNIT,1001)
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LLEGO1
C
      WRITE( PRUNIT, 1100 ) IQ(LLEGO1-5),( IQ( LLEGO1 + J ) , J = 3,10)
C
      IF(IFL.EQ.0) THEN    ! full printout
        WRITE(PRUNIT,1102) 
        ND=IQ(LLEGO1-1)
        WRITE(PRUNIT,1103) (Q(LLEGO1+J),J=11,ND)
      ENDIF
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
C
C ****  Find the next bank for the ALL and LINEAR command. 
C
        LLEGO1 = LQ(LLEGO1)
        IF( LLEGO1 .NE. 0 ) GOTO 1
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LLEGO
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LLEGO, NLEGO
      GOTO 999
 1000 FORMAT(/' ** PRLEGO ** Illegal value of CFL = ',A,/)
 1001 FORMAT('0',//,1X,80('-'),/,10X,' LEGO BANKS ',/)
 1100 FORMAT(//,' LEGO bank number=',I3,5x,'TITLE= ',8A4)
 1102 FORMAT(/,4(7X,'x',9X,'y',9X,'z',2X))
 1103 FORMAT(12F10.3)
 2000 FORMAT(/' ** PRLEGO ** called for LINEAR without valid bank ',
     &        'pointer, LLEGO =',I10,/)
 2100 FORMAT(/'  ** PRLEGO ** called for ONE without bank pointer and ',
     &        'bank number, LLEGO =',I10,' NLEGO =', I10,/)
      END

      SUBROUTINE PRERMG ( PRUNIT, LERMG, NERMG, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'ERMG'. 
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LERMG  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NERMG  [I] : Bank number, used only if CFL='ONE' and LERMG = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LERMG point to a bank, or if <0, NERMG is
C-                                  the bank number.
C-                          'LINEAR' : LERMG points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-                          (ignored)
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created   9-APR-1992   Andrew J. Milder
C-   Updated  18-APR-1992   James T. Linnemann  standard options ; cleanup
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZERMG.LINK'
      INCLUDE 'D0$INC:ERRZST.INC'
      INCLUDE 'D0$INC:ERRZST_C.INC'
C
      INTEGER PRUNIT, LERMG, NERMG, IFL
      CHARACTER*(*) CFL
      INTEGER LERMG1, GZERMG, LZLOC, J

      INTEGER ISEV
      INTEGER I,LSUP,ICOUNT,LENCHR,LEN1,NFIX
      CHARACTER*1 SEVNAME(5)
      CHARACTER*(MXCHR_ERMG) IDSTRG
      DATA SEVNAME / 'I',  'S',  'W',  'E',  'F' /
C----------------------------------------------------------------------
      LERMG1 = LERMG
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LERMG .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LERMG .LE. 0 ) THEN
          IF( NERMG .EQ. 0 ) GOTO 980          ! Error exit
          LERMG1 = LZLOC( IXMAIN, 'ERMG', NERMG )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LERMG1 = GZERMG( )
        IF (LERMG1.LE.0) GOTO 999
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRERMG ** Illegal value of CFL = ',A/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LERMG1
C
        NFIX = IQ(LERMG1+2)
        ICOUNT = IQ(LERMG1+3)
        ISEV   = IQ(LERMG1+4)
        WRITE(PRUNIT,100) IQ(LERMG1-5),IQ(LERMG1+1),SEVNAME(ISEV),ICOUNT
        LENCHR = MIN(IQ(LERMG1 + 5),MXCHR_ERMG)
        CALL UHTOC(IQ(LERMG1+NFIX+1),LENCHR,IDSTRG,LENCHR)
        LEN1 = MIN(128,LENCHR)
        WRITE(PRUNIT,95) IDSTRG(1:LEN1)
        IF (LENCHR.GT.128) WRITE(PRUNIT,95) IDSTRG((LEN1+1):LENCHR)
C
   95 FORMAT(1X,A)
  100 FORMAT(1X,' ERMG Bank Number',I5,' Bank Version',I2, 
     &  ' Severity = ', A1, ' Message seen',I6,' TIMES:')
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LERMG1 = LQ( LERMG1 )
        IF( LERMG1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command. 
C
        LERMG1 = LQ( LERMG1 )
        IF( LERMG1 .NE. 0 ) GOTO 1
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LERMG
 2000 FORMAT(/' ** PRERMG ** called for LINEAR without valid bank ',
     &        'pointer, LERMG =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LERMG, NERMG
 2100 FORMAT(/'  ** PRERMG ** called for ONE without bank pointer and',
     &        ' bank number, LERMG =',I10,' NERMG =', I10/)
      GOTO 999
      END

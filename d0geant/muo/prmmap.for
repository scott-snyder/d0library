      SUBROUTINE PRMMAP( PRUNIT, LMMAP, NMMAP, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'MMAP'. 
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LMMAP  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-             NMMAP  [I] : Bank number (which is also map number), 
C-                          used only if CFL='ONE' and LMMAP = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LMMAP point to a bank, or if <0, NMMAP is
C-                                  the bank number.
C-                          'LINEAR' : LMMAP points to the first bank of the
C-                                  Linear structure
C-   *(Dummy)  IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created   Dec 20,1988   K.Bazizi
C-   Update    Feb           AKl
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZMMAP.LINK'
C
      INTEGER LMMAP1, LZLOC
      INTEGER I,J,IBX,IBY
      INTEGER PRUNIT,LMMAP,NMMAP,IFL
      INTEGER NWORD,NUMIDB
      REAL BUF(10000)
      INTEGER IBUF(10000)
      EQUIVALENCE (IBUF(1),BUF(1))
      CHARACTER*4 JNAME
      CHARACTER*38 CTITL(20)
      CHARACTER CFL*(*)
      DATA CTITL/'Type','Status','Quality','Lowest run number'
     +, 'Highest run number','Run when generated','Date generated'
     +, 'Generated for type of run','map number','map type'
     +, 'number of points in x direction'
     +, 'number of points in y direction'
     +, 'local x of 1st point','local y of 1st point'
     +, 'distance between points in x direction'
     +, 'distance between points in y direction'
     +, 'spare','spare','spare','spare'/
      DATA IBX/21/,IBY/22/
C----------------------------------------------------------------------
      LMMAP1 = LMMAP
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LMMAP.LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LMMAP .LE. 0 ) THEN
          IF( NMMAP .EQ. 0 ) GOTO 980          ! Error exit
          LMMAP1 = LZLOC( IDVSTP, 'MMAP', NMMAP )
         ENDIF
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
 1000    FORMAT(/' ** PRMMAP ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
      CALL UHTOC( IC(LMMAP1-4 ),4,JNAME,4)
    1 CONTINUE
C
C  ***   Initialize and store BUF
C
      NWORD = IC( LMMAP-1 )
      CALL VZERO(BUF(1),NWORD)
      DO 100 J = 1,NWORD
      IF ( J.LE.12 ) THEN
         IBUF(J) = IC( LMMAP1+J )
      ELSE
         BUF(J) = C( LMMAP1+J )
      ENDIF
100   CONTINUE
      NUMIDB = IC( LMMAP1-5 )
C
C  ***  Print the content of the bank pointed by LMMAP1
C
      WRITE( PRUNIT, 900 )
      WRITE( PRUNIT, 905 ) JNAME
      WRITE( PRUNIT, 910 )
      WRITE( PRUNIT, 915 ) NUMIDB
      WRITE( PRUNIT, 910 )
      DO 200 J = 1,20
      IF ( J.LE.12 ) THEN
         WRITE( PRUNIT, 500 ) J, CTITL(J), IBUF(J)
      ELSE
         WRITE( PRUNIT, 505 ) J, CTITL(J), BUF(J)
      ENDIF
200   CONTINUE
      DO 160 J=1,IC(LMMAP+12)
         DO 150 I=1,IC(LMMAP+11)
         WRITE(PRUNIT,510)I,J,IBX,BUF(IBX),IBY,BUF(IBY)
         IBX = IBX + 2 
         IBY = IBY + 2
  150    CONTINUE
  160 CONTINUE
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LMMAP1 = LC( LMMAP1 )
        IF( LMMAP1 .NE. 0 ) GOTO 1
      ENDIF
      GO TO 999
C
C  ***  Format
C 
  500 FORMAT(1X,I4,'.',2X,A38,'     :  ',I10)
  505 FORMAT(1X,I4,'.',2X,A38,'     :  ',F10.2)
  510 FORMAT(1X,'For (x,y)=(',I2,',',I2,')',5X,I4
     +, '.  BX = ',E12.5,5X,I4,'.  BY = ',E12.5)
  900 FORMAT(1H1)
  905 FORMAT(1H0,16X,'==============================',/
     +,          17X,'I  Contents of Bank :  ',A4,'  I',/
     +,          17x,'==============================') 
  910 FORMAT(3X,75(1H-))
  915 FORMAT(34X,'Bank ID          =',I10)
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LMMAP
 2000 FORMAT(/' ** PRMMAP ** called for LINEAR without valid bank ',
     &        'pointer, LMMAP =',I10/)
      GO TO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LMMAP, NMMAP
 2100 FORMAT(/'  ** PRMMAP ** called for ONE without bank pointer and ',
     &        'bank number ',/,'  LMMAP =',I10,10X,' NMMAP =', I10/)
  999 RETURN
      END

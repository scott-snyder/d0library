      SUBROUTINE PRCSFW ( PRUNIT, LCSFW, NCSFW, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'CSFW'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LCSFW  [I] :  Pointer to the first of a linear structure 
C-                          Unused if CFL = 'ALL'.
C-             NCSFW  [I] : unused 
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'LINEAR' : LCSFW points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created   3-MAR-1992 09:30:41.38  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCSFW.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C
      INTEGER PRUNIT, LCSFW, NCSFW, IFL
      CHARACTER*(*) CFL
      INTEGER LCSFW1, GZCSFW, LZLOC,I,J,IETA,L
C----------------------------------------------------------------------
      LCSFW1 = LCSFW
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LCSFW .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LCSFW1 = GZCSFW( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRCSFW ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LCSFW1
C
      WRITE( PRUNIT, 1101 )  IC( LCSFW1 + 1 ) 
 1101 FORMAT(//'CSFW VERSION ',I10)
      WRITE( PRUNIT, 1102 )  (I,I=1,NLYRL)
 1102 FORMAT('LYR ',17I7,' ETA ')
      DO 1104 IETA = 1,NETAL
        I = (IETA-1)*NLYRL  ! 1 to 81600
        WRITE( PRUNIT, 1103 )(C(LCSFW1+1+L+I),L=1,NLYRL),IETA
 1103   FORMAT(5000(4X,17(1PE7.1E1),I4))
 1104 CONTINUE
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'LINEAR' ) THEN
        LCSFW1 = LC( LCSFW1 )
        IF( LCSFW1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LCSFW1 = GZCSFW()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LCSFW
 2000 FORMAT(/' ** PRCSFW ** called for LINEAR without valid bank '
     & ,       'pointer, LCSFW =',I10/)
      GOTO 999
      END

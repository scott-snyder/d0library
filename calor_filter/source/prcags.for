      SUBROUTINE PRCAGS ( PRUNIT, LCAGS, NCAGS, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'CAGS'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LCAGS  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NCAGS  [I] : Bank number, used only if CFL='ONE' and LCAGS = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LCAGS point to a bank, or if <0, NCAGS is
C-                                  the bank number.
C-                          'LINEAR' : LCAGS points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created 26-APR-1991   James T. Linnemann   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCAGS.LINK'
C
      INTEGER PRUNIT, LCAGS, NCAGS, IFL
      CHARACTER*(*) CFL
      INTEGER LCAGS1, GZCAGS, LZLOC, J
      CHARACTER*(10) LABEL(18)
      DATA LABEL/
     &  ' Type',
     &  ' Status',
     &  ' Quality',
     &  ' Low Run', 
     &  ' High Run',
     &  ' Run Used', 
     &  ' Date Gen',
     &  ' Time Gen', 
     &  ' Run Type',
     &  ' Version',
     &  ' Biggest',
     &  ' Smallest',
     &  ' Step',
     &  ' Ln(Step)',
     &  ' Table:',
     &  ' ECN d(E)',
     &  ' CC  d(E)',
     &  ' ECS d(E)'/
      LCAGS1 = LCAGS
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LCAGS .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LCAGS .LE. 0 ) THEN
          IF( NCAGS .EQ. 0 ) GOTO 980          ! Error exit
          LCAGS1 = LZLOC( IDVSTP, 'CAGS', NCAGS )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LCAGS1 = GZCAGS( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRCAGS ** Illegal value of CFL = ',A/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LCAGS1
C
      WRITE( PRUNIT,1099) (LABEL(J),J=1,10)
 1099 FORMAT(' CAGS BANK:'/10A10)
      WRITE( PRUNIT, 1100 ) ( IC( LCAGS1 + J ) , J = 1,10 )
 1100 FORMAT(10(10I10/))
      WRITE( PRUNIT,1099) (LABEL(J),J= 11,15)
      WRITE( PRUNIT, 1105 ) ( C( LCAGS1 + J ) , J = 11, IC( LCAGS1-1) )
 1105 FORMAT(/1000(10F10.6/))
      IF (IC (LCAGS1 + 10) .GE. 2) THEN
        WRITE( PRUNIT,1099) (LABEL(J), J = 16,18)
        WRITE( PRUNIT,1105) (C(LCAGS1 + J), J = 271,273)
      ENDIF
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LCAGS1 = LC( LCAGS1 )
        IF( LCAGS1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LCAGS1 = GZCAGS()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LCAGS
 2000 FORMAT(/' ** PRCAGS ** called for LINEAR without valid bank ',
     &        'pointer, LCAGS =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LCAGS, NCAGS
 2100 FORMAT(/'  ** PRCAGS ** called for ONE without bank pointer and',
     &        ' bank number, LCAGS =',I10,' NCAGS =', I10/)
      GOTO 999
      END

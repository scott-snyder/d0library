      SUBROUTINE PRTDST ( PRUNIT, LTDST, NTDST, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'tdst'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             Ltdst  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             Ntdst  [I] : Bank number, used only if CFL='ONE' and Ltdst = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : Ltdst point to a bank, or if <0, Ntdst is
C-                                  the bank number.
C-                          'LINEAR' : Ltdst points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  XDATE  XAUTHOR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRUNIT, LTDST, NTDST, IFL
      CHARACTER*(*) CFL
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
c      INCLUDE 'D0$LINKS:IZtdst.LINK'
C----------------------------------------------------------------------
      INTEGER LTDST1, GZTDST, LZLOC, J
      CHARACTER*8 CFLAG
C----------------------------------------------------------------------
      CFLAG  = CFL(1:LEN(CFL))
      LTDST1 = LTDST
C
C
        LTDST1 = GZTDST()
C
C ****  Check address
C
      IF ( LTDST1 .LE. 0 ) THEN
        WRITE( PRUNIT, 1005 )
 1005   FORMAT(/' No tdst bank FOUND')
        GOTO 999
      ENDIF
C
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by Ltdst1
C
c      WRITE( PRUNIT, 1100 ) ( IQ( LTDST1 + J ) , J = 1, IQ( LTDST1-1) )
 1100 FORMAT(/' to be defined, that''s your job...'/)
C
  999 RETURN
      END

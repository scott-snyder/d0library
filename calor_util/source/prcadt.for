      SUBROUTINE PRCADT ( PRUNIT, LCADT, NCADT, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'CADT'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LCADT  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NCADT  [I] : Bank number, used only if CFL='ONE' and LCADT = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LCADT point to a bank, or if <0, NCADT is
C-                                  the bank number.
C-                          'LINEAR' : LCADT points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  20-SEP-1990 10:20:16.41  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCADT.LINK'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C
      CHARACTER*(*) CFL
      INTEGER PRUNIT, LCADT, NCADT, IFL
      INTEGER LCADT1, GZCADT, LZLOC, J, NV, CRATE, PAKADR
      INTEGER ETA(5),PHI(5),LYR(5),PTR(5),ADDR(5),K,N
      BYTE BYTES(4)
      EQUIVALENCE (PAKADR,BYTES)

C----------------------------------------------------------------------
      LCADT1 = LCADT
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LCADT .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LCADT .LE. 0 ) THEN
          IF( NCADT .EQ. 0 ) GOTO 980          ! Error exit
          LCADT1 = LZLOC( IDVSTP, 'CADT', NCADT )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LCADT1 = GZCADT( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRCADT ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LCADT1
C
      NV = IC(LCADT1+1)
      CRATE = IC(LCADT1+2)
      WRITE(PRUNIT,1101) CRATE,NV
      K = 1
      DO J = 3, IC(LCADT1-1)
        ADDR(K)= J-3
        PAKADR = IC(LCADT1+J)
        ETA(K) = BYTES(BYTE4)
        PHI(K) = BYTES(BYTE3)
        LYR(K) = BYTES(BYTE2)
        PTR(K) = IAND(PAKADR,255)
        IF (K.EQ. 4) THEN
          WRITE( PRUNIT, 1100 )
     &      (ADDR(N),ETA(N),PHI(N),LYR(N),PTR(N),N=1,4)
          K = 0
        END IF
        K = K + 1
      END DO
      IF(K.GT.1) THEN
        WRITE( PRUNIT, 1100 )
     &    (ADDR(N),ETA(N),PHI(N),LYR(N),PTR(N),N=1,K-1)
      END IF
 1100 FORMAT(1X,4(Z5,I4,2I3,I4))
 1101 FORMAT(/' CRATE ',I5,' CADT version ',I5,/
     &  1X,4('    I ETA PH LY PTR' ) )
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) THEN
        GOTO 999
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LCADT1 = LC( LCADT1 )
        IF( LCADT1 .NE. 0 ) GOTO 1
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LCADT
 2000 FORMAT(/' ** PRCADT ** called for LINEAR without valid bank '
     &        ,'pointer, LCADT =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LCADT, NCADT
 2100 FORMAT(/'  ** PRCADT ** called for ONE without bank pointer and '
     &        ,'bank number, LCADT =',I10,' NCADT =', I10/)
      GOTO 999
      END

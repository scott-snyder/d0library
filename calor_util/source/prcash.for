      SUBROUTINE PRCASH ( PRUNIT, LCASH, NCASH, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'CASH'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LCASH  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NCASH  [I] : Bank number, used only if CFL='ONE' and LCASH = 0
C-             CFL    [C*]: Character flag, other input depends on its value:
C-                          'ONE' : LCASH point to a bank, or if <0, NCASH is
C-                                  the bank number.
C-                          'LINEAR' : LCASH points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  25-FEB-1992 15:09:00.17  Norman A. Graf
C-   Updated   6-SEP-1994  J. Drinkard                Add SUNOS switch
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER PRUNIT, LCASH, NCASH, IFL
      CHARACTER*(*) CFL
      INTEGER LCASH1, GZCASH, LZLOC, J
C
      INTEGER I,K,KK,LDATA,NCELLS,NR,ETA,VRSION
      INTEGER    NCHLIN
      PARAMETER (NCHLIN=3)
      REAL*4 ENERGY(NCHLIN)
      INTEGER INDCES(NCHLIN)
      CHARACTER*4 PATH
C&IF VAXVMS,SIUNIX
      BYTE BYT(4,NCHLIN)
      EQUIVALENCE (BYT(1,1),INDCES)
C&ENDIF
C----------------------------------------------------------------------
      LCASH1 = LCASH
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LCASH .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LCASH .LE. 0 ) THEN
          IF( NCASH .EQ. 0 ) GOTO 980          ! Error exit
          LCASH1 = LZLOC( IXMAIN, 'CASH', NCASH )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LCASH1 = GZCASH( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LCASH1
C
      VRSION = IQ(LCASH+1)
      WRITE(PRUNIT,102) VRSION
C
      NCELLS = IQ(LCASH+2)
      WRITE(PRUNIT,103) NCELLS
C
C        Print contents of the bank
C
      IF(IFL.EQ.0) THEN
        K=0
        DO 10 I=1,NCELLS
          IF(MOD(I,50).EQ.1) WRITE(PRUNIT,104)
          LDATA=LCASH+3+2*i-3
          K=K+1
          INDCES(K)=IQ(LDATA+1)
          ENERGY(K)=Q(LDATA+2)
          IF(MOD(I,NCHLIN).EQ.0 .OR. I.EQ.NCELLS) THEN
C&IF VAXVMS,SIUNIX
            WRITE(PRUNIT,105) ((BYT(J,KK),J=4,1,-1),ENERGY(KK),KK=1,K)
C&ENDIF
            K=0
          ENDIF
   10   CONTINUE
      ENDIF
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LCASH1 = LQ( LCASH1 )
        IF( LCASH1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LCASH1 = GZCASH()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LCASH
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LCASH, NCASH
      GOTO 999
C
  102 FORMAT(/,
     +' ========================================================='/
     +'      CASH: EM CLUSTER cell bank version: ',I4,            /
     +' ========================================================='/)
  103 FORMAT('  The current CASH bank contains ',I5,' cells.')
C&IF VAXVMS,ULTRIX,SIUNIX,SUNOS
  104 FORMAT(/,1X,<NCHLIN>('   ETA PHI LYR BITS ENERGY'))
  105 FORMAT(<NCHLIN>(2X,3I4,2X,Z2,F8.3))
C&ENDIF
C&IF IBMAIX
C&  104 FORMAT(/,1X,3('   ETA PHI LYR BITS ENERGY'))
C&  105 FORMAT(3(2X,3I4,2X,Z2,F8.3))
C&ENDIF
 1000 FORMAT(/' ** PRCASH ** Illegal value of CFL = ',a/)
 2000 FORMAT(/' ** PRCASH ** called for LINEAR without valid bank '
     &        ,'pointer, LCASH =',I10/)
 2100 FORMAT(/'  ** PRCASH ** called for ONE without bank pointer and '
     &        ,'bank number, LCASH =',I10,' NCASH =', I10/)
      END

      SUBROUTINE PRCSFC ( PRUNIT, LCSFC, NCSFC, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'CSFC'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LCSFC  [I] :  Pointer to the first of a linear structure 
C-                          Unused if CFL = 'ALL'.
C-             NCSFC  [I] : not used 
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'LINEAR' : LCSFC points to the first bank of the
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
      INCLUDE 'D0$LINKS:IZCSFC.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C
      INTEGER PRUNIT, LCSFC, NCSFC, IFL
      CHARACTER*(*) CFL
      INTEGER LCSFC1, GZCSFC, LZLOC, I,J,K,L,IETA,IPHI
C----------------------------------------------------------------------
      LCSFC1 = LCSFC
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LCSFC .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LCSFC1 = GZCSFC( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRCSFC ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LCSFC1
C
      WRITE( PRUNIT, 1100 )  IC( LCSFC1 + 1 ) ,IC( LCSFC1 + 2 ) 
 1100 FORMAT(//' CSFC VERSION ',I10,' LAYER', I2)
 1102 FORMAT(' LYR',17I7,' ETA ')
      DO 1104 IETA = -NETAL,NETAL
        IF(IETA.EQ.0) GOTO 1104
        I = (IETA+NETAL)*NPHIL  ! 1 to 81600
        WRITE( PRUNIT, 1103 )(C(LCSFC1 +2+L+I),L=1,NPHIL),IETA
 1103   FORMAT(5000(3X,16(1PE7.1E1),3(/16(1PE7.1E1)),I4))
 1104 CONTINUE
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'LINEAR' ) THEN
        LCSFC1 = LC( LCSFC1 )
        IF( LCSFC1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LCSFC1 = GZCSFC()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LCSFC
 2000 FORMAT(/' ** PRCSFC ** called for LINEAR without valid bank '
     & ,       'pointer, LCSFC =',I10/)
      GOTO 999
      END

      SUBROUTINE PRCGEV ( PRUNIT, LCGEV, NCGEV, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'CGEV'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LCGEV  [I] :  Pointer to the first of a linear structure 
C-                          Unused if CFL = 'ALL'.
C-             NCGEV  [I] : not used 
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'LINEAR' : LCGEV points to the first bank of the
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
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C
      INTEGER PRUNIT, LCGEV, NCGEV, IFL, NR
      CHARACTER*(*) CFL
      INTEGER LCGEV1, GZCGEV, LZLOC, I,J,K,L,IETA,IPHI
C----------------------------------------------------------------------
      LCGEV1 = LCGEV
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LCGEV .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LCGEV1 = GZCGEV( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRCGEV ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LCGEV1
C
      NR = IC(LCGEV1+3)
      WRITE( PRUNIT, 1100 )  (IC( LCGEV1+I),I=1,3)
 1100 FORMAT(//' CGEV VERSION',I10,/5X,'SCALE ',I5,/5X,' NR ',I5)
      DO 1109 IETA = -NETAL,NETAL
        IF(IETA.EQ.0) GOTO 1109
        DO IPHI = 1, NPHIL
          I = (IPHI-1)*NLYRL+(IETA+NETAL)*NLYRL*NPHIL  ! 1 to 81600
          IF(NR.EQ.1) THEN
            WRITE(PRUNIT,1101)(C(LCGEV1 +1+L+I),L=1,NR*NLYRL),IETA,IPHI
          ELSE IF(NR.EQ.2) THEN
            WRITE(PRUNIT,1102)(C(LCGEV1 +1+L+I),L=1,NR*NLYRL),IETA,IPHI
          ELSE IF(NR.EQ.3) THEN
            WRITE(PRUNIT,1103)(C(LCGEV1 +1+L+I),L=1,NR*NLYRL),IETA,IPHI
          ELSE  
            WRITE(PRUNIT,1104)NR
          END IF
        END DO
 1101   FORMAT(5000(3X,12(1PE9.3E1),
     &      /3X,5(1PE9.3E1),2I4))
 1102   FORMAT(5000(3X,12(1PE9.3E1),
     &      /3X,12(1PE9.3E1),/3X,10(1PE9.3E1),2I4))
 1103   FORMAT(5000(3X,12(1PE9.3E1),/3X,12(1PE9.3E1),
     &      /3X,12(1PE9.3E1),/3X,12(1PE9.3E1),/3X,5(1PE9.3E1),2I4))
 1104   FORMAT ('PRCGEV with NR = ',I2.2,' UNSUPPORTED')
 1109 CONTINUE
C
C  ***  Look if another bank is needed
C
      IF( CFL(1:3) .NE. 'ONE' ) THEN
        LCGEV1 = LC( LCGEV1 )
        IF( LCGEV1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LCGEV1 = GZCGEV()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LCGEV
 2000 FORMAT(/' ** PRCGEV ** called for LINEAR without valid bank '
     & ,       'pointer, LCGEV =',I10/)
      GOTO 999
      END

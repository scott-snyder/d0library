      SUBROUTINE PRCCP ( PRUNIT, LCCP, NTYPE, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'CCPT','CCPC','CCUA'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LCCP  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NTYPE  [I] : Bank TYPE 2=CCPC, 3=CCUAA, OTHER=CCPT
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LCCP point to a bank, or if <0, NTYPE is
C-                                  the bank number.
C-                          'LINEAR' : LCCP points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created   1-AUG-1991  Chip Stewart, Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER PRUNIT, LCCP1, NTYPE, IFL, ICRATE,IADC,IBLS,ITWR,IDEP
      CHARACTER*(*) CFL
      INTEGER LCCP, GZCCPT,GZCCPC,GZCCUA, J, ICHAN, ICAP(0:11), IER
C----------------------------------------------------------------------
      LCCP1 = LCCP
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LCCP .LE. 0 ) GOTO 990
      ELSEIF((CFL.EQ.'ONE'.and.LCCP.LE.0)
     &  .OR. ( CFL .EQ. 'ALL' ) )THEN
        LCCP1 = GZCCPT ()
        IF(NTYPE.EQ.2) LCCP1 = GZCCPC ()
        IF(NTYPE.EQ.3) LCCP1 = GZCCUA ()
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRCCP ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LCCP1
C
      WRITE( PRUNIT, '(//'' BANK:'',A5)') IC(LCCP1-4)
      WRITE( PRUNIT, 1100 ) ( IC( LCCP1 + J ) , J = 1, 5)
 1100 FORMAT(' VERSION ',I5,/' LOW RUN ',I9,' HIGH RUN ',I9,
     &       /' DATE ',I7.6,' TIME ',I7.6)
      ICRATE = IC(LCCP1+6)
      WRITE( PRUNIT, 1101 ) ICRATE
 1101 FORMAT(' ADC CRATE =  ',I5)
      DO IADC = 0,11
        WRITE( PRUNIT, 1102 ) IADC
 1102   FORMAT(' BLS TWR,  ADC CARD =  ',I5)
        DO IBLS = 0,7
          DO ITWR = 0,3
            DO IDEP = 0, 11
              ICHAN = IADC*384 + IBLS*48 + ITWR*12 + IDEP + 1
              IF(NTYPE.EQ.2) THEN
                CALL GTCCPC(ICRATE,ICHAN,ICAP(IDEP),IER)
              ELSE IF(NTYPE.EQ.3) THEN
                CALL GTCCUA(ICRATE,ICHAN,ICAP(IDEP),IER)
              ELSE
                CALL GTCCPT(ICRATE,ICHAN,ICAP(IDEP),IER)
              END IF
            END DO                    !DEPTH
            WRITE (PRUNIT,1103) IBLS,ITWR,(ICAP(IDEP),IDEP=0,11)
 1103       FORMAT(1X,2I4.2,12I5)
            IF(IER.NE.0) THEN
              WRITE (PRUNIT,1104) IER
 1104         FORMAT(1X,' ERROR IN GT ROUTINE ',I5)
              GOTO 999
            END IF
          ENDDO                         ! ITWR
        ENDDO                           ! IBLS
      ENDDO                             ! IADC
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      LCCP1 = LC( LCCP1 )
      IF( LCCP1 .NE. 0 ) GOTO 1
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LCCP
 2000 FORMAT(/' ** PRCCP ** called for LINEAR without valid bank '
     &        ,'pointer, LCCP =',I10/)
      GOTO 999
      END

      SUBROUTINE PRZFIT(PRUNIT, LZFIT, NZFIT, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of bank 'ZFIT' 
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LZFIT  [I] : Pointer to the bank ( CFL = 'ONE' ) 
C-                          Unused if CFL = 'ALL'.
C-             NZFIT  [I] : Bank number, used only if CFL='ONE' and LZFIT = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' OR 'ALL' 
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  20-APR-1990   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZZFIT.LINK'
C
      INTEGER PRUNIT, LZFIT, NZFIT, IFL
      CHARACTER*(*) CFL
      INTEGER LZFIT1, GZZFIT, LZLOC, J
      INTEGER NZTRK, IZTRK, NVERS, I, ICONT(10)
      INTEGER GZZTRH
      INTEGER NXYV, NZV, NXYC, NZC
      LOGICAL FIRST
C----------------------------------------------------------------------
      IF( CFL .EQ. 'ONE' ) THEN
        IF( LZFIT .LE. 0 ) THEN
          IF( NZFIT .EQ. 0 ) GOTO 980          ! Error exit
          LZFIT1 = GZZFIT(NZFIT)
          NVERS = IQ(LZFIT1+1)
          WRITE(PRUNIT, 2000) NVERS
          WRITE(PRUNIT, 2001)
          NXYV = IBITS(IQ(LZFIT1+3),0,16)
          NZV = IBITS(IQ(LZFIT1+3),16,16)
          NXYC = IBITS(IQ(LZFIT1+4),0,16)
          NZC = IBITS(IQ(LZFIT1+4),16,16)
          WRITE(PRUNIT, 2002) IQ(LZFIT1-5),IQ(LZFIT1+6),IQ(LZFIT1+7),
     &        NXYV,NZV,NXYC,NZC,IQ(LZFIT1+5),Q(LZFIT1+8),Q(LZFIT1+9),
     &        Q(LZFIT1+10),Q(LZFIT1+16),Q(LZFIT1+13),Q(LZFIT1+18),
     &        Q(LZFIT1+20),Q(LZFIT1+21),Q(LZFIT1+22),
     &        Q(LZFIT1+23),Q(LZFIT1+24),Q(LZFIT1+25)
        ENDIF
      ELSE
        IF( CFL .EQ. 'ALL' ) THEN
          CALL GTZTRH(ICONT)
          NZTRK = ICONT(2)
          IF (NZTRK.LE.0) GO TO 999
          FIRST = .TRUE.
          DO 100 IZTRK = 1, NZTRK
            LZFIT1 = GZZFIT(IZTRK)
            IF (LZFIT1 .LE. 0) GOTO 100
            IF (FIRST) THEN
              FIRST = .FALSE.
              NVERS = IQ(LZFIT1+1)
              WRITE(PRUNIT, 2000) NVERS
 2000         FORMAT (1X,' Bank ZFIT:',
     &  ' (version',I2,'), Global fitting parameters for ZTRK')
              WRITE(PRUNIT, 2001)
 2001         FORMAT(1X,'ZTRK NXY NZ NXY_V NZ_V NXY_C NZ_C NHIT_F',
     &      ' Chi_XY Chi_RZ      PHI           THETA',
     & '         COS(ALPHA)      COS(BETA)      COS(GAMMA)')
            ENDIF
            NXYV = IBITS(IQ(LZFIT1+3),0,16)
            NZV = IBITS(IQ(LZFIT1+3),16,16)
            NXYC = IBITS(IQ(LZFIT1+4),0,16)
            NZC = IBITS(IQ(LZFIT1+4),16,16)
            WRITE(PRUNIT, 2002) IQ(LZFIT1-5),IQ(LZFIT1+6),IQ(LZFIT1+7),
     &        NXYV,NZV,NXYC,NZC,IQ(LZFIT1+5),Q(LZFIT1+8),Q(LZFIT1+9),
     &        Q(LZFIT1+10),Q(LZFIT1+16),Q(LZFIT1+13),Q(LZFIT1+18),
     &        Q(LZFIT1+20),Q(LZFIT1+21),Q(LZFIT1+22),
     &        Q(LZFIT1+23),Q(LZFIT1+24),Q(LZFIT1+25)
 2002       FORMAT(I5,I4,I3,I6,I5,I6,I5,I7,2F7.1,2(F7.4,'+-',F6.4),
     &        3(F7.4,'+-',F6.4))
  100     CONTINUE
        ELSE
          WRITE( PRUNIT, 1000 ) CFL
1000      FORMAT(/' ** PRZFIT ** Illegal value of CFL = ',a/)
          GOTO 999
        ENDIF
      ENDIF
    1 CONTINUE
C
  999 RETURN
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LZFIT, NZFIT
 2100 FORMAT(/' ** PRZFIT ** called for ONE without bank pointer and',
     &        ' bank number, LZFIT =',I10,' NZFIT =', I10/)
      GOTO 999
      END

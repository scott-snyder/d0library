      SUBROUTINE PRDTRK( PRUNIT, KDTRK, NDTRK, CARFL, IPRFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the bank DTRK ( and DTTH )
C-
C-   Inputs  : PRUNIT [I] : The fortran output unit
C-             KDTRK  [I] : Bank pointer if CARFL = 'SINGLE'
C-             NDTRK  [I] : Bank number  if CARFL = 'SINGLE'
C-             CARFL [C*] : Flag, either 'ALL' or 'SINGLE'
C-             IPRFL  [I] : Print level
C-   Outputs : on the specified unit
C-
C-   Created  14-MAR-1988   Olivier Callot
C-   Updated  14-JUL-1989   Qizhong Li-Demarteau   print version # 
C-   Updated  21-OCT-1991   Qizhong Li-Demarteau   added segment numbers 
C-   Updated   4-MAR-1992   Qizhong Li-Demarteau   removed machine block 
C-   Updated  24-AUG-1992   Qizhong Li-Demarteau   added dE/dx 
C-   Updated  12-FEB-1993   Qizhong Li-Demarteau   added check on bank 
C-                                                 address 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'
      INTEGER PRUNIT, KDTRK, NDTRK, IPRFL
      CHARACTER*(*) CARFL
      CHARACTER*8 ZFORMA
      INTEGER IW, LABEL, LAY, SEC, WIR, HIT, SID, HDTRK, LDTTH
      INTEGER LZFIND, NVERS
      INTEGER SBIT, NBIT, SEGLAY(4), ILAY
      PARAMETER( NBIT = 7 )
      REAL    RES
      CHARACTER*2 TYPE
      INTEGER IPBK, NBBNK,GZDTRH
C----------------------------------------------------------------------
C
      IF (LDTRH .LE. 0) LDTRH = GZDTRH()
C
      IF ( CARFL .EQ. 'ALL' ) THEN
        NBBNK = 0
        IF (LDTRH.GT.0) THEN
          IPBK = LQ( LDTRH-1 )
        ELSE
          IPBK=0
        END IF
        IF( IPBK .EQ. 0 ) THEN
          WRITE( PRUNIT, 3000 )
 3000     FORMAT('0 No DTRK banks for this event')
          GOTO 999
        ENDIF
      ELSE
        NBBNK = 1
        IF ( KDTRK .NE. 0 ) THEN
          IPBK = KDTRK
          IF( IQ( IPBK-4 ) .NE. HDTRK ) THEN
            IF( IPRFL .GE. 5 ) WRITE( PRUNIT, 1000 ) CARFL, IPBK,
     &                                               NDTRK, IQ(IPBK-4)
 1000       FORMAT('0** PRDTRK ** invalid call: CARFL, KDTRK, NDTRK=',
     &             A, 2I10,' Name = ',A4)
            GOTO 999
          ENDIF
        ELSE
          IF (LDTRH .GT. 0) THEN
            IPBK = LZFIND( IXCOM, LQ( LDTRH-1 ), NDTRK, -5 )
          ELSE
            IF( IPRFL .GE. 5 ) WRITE( PRUNIT, 1010 ) NDTRK
 1010       FORMAT('0** PRDTRK ** Bank number ',I6,' not found')
            RETURN
          ENDIF
        ENDIF
      ENDIF
      NVERS = IBITS(IQ(IPBK),13,5) 
      WRITE(PRUNIT, 2001) NVERS
 2001 FORMAT (1X,' Bank DTRK:   (version',I2,'),   CDC track bank')
      WRITE( PRUNIT, 2000 )
 2000 FORMAT(' DTRK  L0  L1  L2  L3',7X,'X0       Y0 Err xy       Z0',
     &        2X,'  Err Z    Phi   dphi Theta dtheta',
     &        ' dE/dx NXY    Chi2  NZ    Chi2')
C
C ****  Now, IPBK is pointing on a selected bank. NBBNK is 0 if all needed
C
    1 CONTINUE
      SBIT = 4
      DO 300 ILAY = 1, 4
        SEGLAY(ILAY) = IBITS(IQ(IPBK+1),SBIT,NBIT)
        SBIT = SBIT + NBIT
  300 CONTINUE
      WRITE (PRUNIT, 2100) IQ(IPBK-5), SEGLAY,
     &          Q(IPBK+7),  Q(IPBK+8),  Q(IPBK+17), Q(IPBK+11), 
     &          Q(IPBK+19), Q(IPBK+6),
     &          Q(IPBK+16), Q(IPBK+9),  Q(IPBK+18), Q(IPBK+20),
     &          IQ(IPBK+2), Q(IPBK+12), IQ(IPBK+5), Q(IPBK+13)
 2100 FORMAT(I5,4I4,2f9.4,f7.4,f9.4,f9.4,4f7.4,f5.2,2(i4,f8.2))
C
      IF( IPRFL .LE. 2 ) GOTO 100
C
C ****  Now, the list of residuals
C
      LDTTH = LQ( IPBK-1 )
      IF (LDTTH .LE. 0) GOTO 100
      DO 10 IW = 1, IQ(LDTTH-1)/2
        TYPE = 'XY'
        IF( IW .GT. IQ( IPBK+2 ) ) TYPE = 'RZ'
        LABEL = IQ( LDTTH+1 )
        RES   = Q( LDTTH+2)
        LDTTH = LDTTH + 2
        LAY = IBITS( LABEL, 16, 2)
        SEC = IBITS( LABEL, 11, 5)
        WIR = IBITS( LABEL,  8, 3)
        HIT = IBITS( LABEL,  1, 7)
        SID = IBITS( LABEL,  0, 1)
        WRITE( PRUNIT, 2500 ) TYPE, LAY, SEC, WIR, HIT, SID, RES
 2500   FORMAT(10X, A2, ' residual on Layer',I3,' Sector',I4,' Wire',I3,
     &         ' Hit',I5,' Side',I3,' = ',F12.4)
   10 CONTINUE
C
C ****  Now, check for another track
C
  100 CONTINUE
      IF( NBBNK .EQ. 1 ) GOTO 999
      IPBK = LQ( IPBK )
      IF( IPBK .NE. 0 ) GOTO 1
C
  999 RETURN
      END

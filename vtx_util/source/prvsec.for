      SUBROUTINE PRVSEC( PRUNIT, JVSEC, NVSEC, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print VSEC bank(s)
C-
C-   Inputs  : PRUNIT : unit number
C-             JVSEC  : Pointer to one bank ( if CFL = 'ONE' )
C-             NVSEC  : Id number of one bank( unused )
C-             CFL    : Character flag, 'ALL' = all VSEC banks
C-                      'ONE' = only the one with JVSEC
C-                      All other values act like 'ONE'
C-             IFL    : Level of print ( 1, 2, 3 ) see comments
C-   Outputs : print out on unit PRUNIT
C-
C-   Daria Zieminska JAN.,1987
C-   Daria Zieminska May  1988 new hit bank format
C-   Peter Grudberg  16-JAN-1989 track id optional
C-   Peter Grudberg  25-JUN-1989 implement CFL, update IFL
C-   Modified 02-OCT-1989  P.G. - fix bug, make printout like DSEC
C-   Updated  25-SEP-1992   Peter M. Grudberg  Fix for IBM compatibility 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:VTXLNK.INC'
      INTEGER PRUNIT, JVSEC, NVSEC, IFL
      CHARACTER*(*) CFL
      INTEGER KPVSEC, I, J, K, NVERS, SECTOR
      INTEGER LAY, SEC, IWIR, IHIT, NBSECT, NBLAYR, SECPR, LAYPR
      INTEGER LHIT, NHIT, NWIR, IPWI, LISHIT(0:31), NUMSEC(0:2)
      INTEGER JH, IADD, LAYER, WIRE, NTOT
      INTEGER ISTAT, IBYTE0, IBYTE1, IBYTE2, IBYTE3
      LOGICAL FIRST
      DATA NUMSEC / 16, 32, 32 /
C----------------------------------------------------------------------
      IF ( CFL .EQ. 'ALL' .AND. IFL .EQ. 1 ) THEN
        FIRST = .TRUE.
C
C ****  Special print, one line per layer
C
        DO 50 LAY = 0, 2
          CALL VZERO( LISHIT(0), 32 )
          DO 60 SEC = 0, NUMSEC(LAY) - 1
            IF( LVSEC(SEC,LAY) .NE. 0 ) THEN
              LISHIT(SEC) = IQ( LVSEC(SEC,LAY)+1 )
              IF (FIRST) THEN
                FIRST = .FALSE.
                NVERS = IBITS(IQ(LVSEC(SEC,LAY)),13,5)
                WRITE(PRUNIT,4000) NVERS, (SECTOR,SECTOR=0,31)
 4000           FORMAT(/,'  Bank VSEC: (version',I2,')',/
     &            12X,'--- number of hits per layer and sectors ---'//
     &            ' L\S',32I4/)
              ENDIF
            ENDIF
   60     CONTINUE
          IF ( .NOT. FIRST ) THEN
            WRITE(PRUNIT, 4100) LAY, (LISHIT(I), I=0, NUMSEC(LAY)-1 )
          ENDIF
 4100     FORMAT(I2,2X,32I4)
   50   CONTINUE
        IF (FIRST) THEN
          WRITE (PRUNIT, 4001)
 4001     FORMAT (/,1X,' Bank VSEC does not exist')
        ENDIF
        GOTO 999
      ENDIF
C
C ****  Detailled or selective print
C
      IF( CFL .EQ. 'ALL' ) THEN
        NBLAYR = 3
      ELSE
        NBLAYR = 1
      ENDIF
      LAY = 0
      DO WHILE ( LAY .LT. NBLAYR )
        IF ( CFL .EQ. 'ALL' ) THEN
          NBSECT = NUMSEC(LAY)
        ELSE
          NBSECT = 1
        ENDIF
        SEC = 0
        DO WHILE ( SEC .LT. NBSECT )
          IF ( CFL .EQ. 'ALL' ) THEN
            KPVSEC = LVSEC( SEC, LAY )
          ELSE
            KPVSEC = JVSEC
          ENDIF
          IF( KPVSEC .EQ. 0 ) GOTO 20
          LAYPR = IBITS( IQ( KPVSEC-5 ), 9, 3 )
          SECPR = IBITS( IQ( KPVSEC-5 ), 4, 5 )
          NVERS = IBITS(IQ(KPVSEC),13,5)
C
C ****  IFL = 1: print total number of hits
C
          WRITE( PRUNIT, 1000 ) NVERS, LAYPR, SECPR
 1000     FORMAT(/,'  Bank VSEC: (version',I2,') for Layer ',I1,
     &      ' Sector ',I2)
          WRITE( PRUNIT, 1010 ) (IQ( KPVSEC+I), I=1,3)
 1010     FORMAT(5X,'Number of hits =',I6,' on the ',I3,' wires.',
     &           ' Each hit is ',I3,' words long.'/)
C
C ****  IFL > 1: print num. hits/wire, pointers
C
          IF( IFL .LE. 1 ) GOTO 20
          LHIT = IQ( KPVSEC+3 )
          NWIR = IQ( KPVSEC+2 )
          NTOT = IQ( KPVSEC+1 )
          IF ( NTOT .LE. 0 ) GOTO 20
          WRITE( PRUNIT, 1020 ) (I-1,I=1,NWIR)
          WRITE( PRUNIT, 1030 ) (IQ( KPVSEC+I+3), I=1,NWIR)
          WRITE( PRUNIT, 1040 ) (IQ( KPVSEC+I+NWIR+3), I=1,NWIR)
 1020     FORMAT(5X,'Wire number    = ',8I6)
 1030     FORMAT(5X,'Number of hits = ',8I6)
 1040     FORMAT(5X,'Pointer to hit = ',8I6)
C
C **** Full printout: IFL > 2
C
          IF ( IFL .LE. 2 ) GO TO 20
          IF ( LHIT .EQ. 11 ) THEN   ! bank includes track id word
            WRITE( PRUNIT, 1050 )
          ELSE                      ! no track id under RECO
            WRITE( PRUNIT, 1060 )
          ENDIF
          DO 200 IWIR = 1, NWIR
            NHIT = IQ( KPVSEC + IWIR + 3 )
            IPWI = IQ( KPVSEC + NWIR + IWIR + 3 )
            DO 210 IHIT = 1, NHIT
              JH = KPVSEC + IPWI - 1 + LHIT*(IHIT-1)
              IADD  = IQ(JH+1)
              LAYER  = IBITS(IADD,9,3)
              SECTOR = IBITS(IADD,4,5)
              WIRE   = IBITS(IADD,1,3)
C  Unpack status word
              ISTAT  = IQ(JH+10)
              IBYTE0 = IBITS(ISTAT,0,8)
              IBYTE1 = IBITS(ISTAT,8,8)
              IBYTE2 = IBITS(ISTAT,16,8)
              IBYTE3 = IBITS(ISTAT,24,8)
              IF ( LHIT .EQ. 11 ) THEN
                WRITE(PRUNIT,1070) LAYER,SECTOR,WIRE,Q(JH+2),Q(JH+3),
     &            Q(JH+5),Q(JH+4),Q(JH+6),Q(JH+7),Q(JH+8),Q(JH+9),
     $            IBYTE3,IBYTE2,IBYTE1,IBYTE0,IQ(JH+11)
              ELSE                  ! no track id under RECO
                WRITE(PRUNIT,1070) LAYER,SECTOR,WIRE,Q(JH+2),Q(JH+3),
     &            Q(JH+5),Q(JH+4),Q(JH+6),Q(JH+7),Q(JH+8),Q(JH+9),
     $            IBYTE3,IBYTE2,IBYTE1,IBYTE0
              ENDIF
  210       CONTINUE
  200     CONTINUE
 1050     FORMAT(/,' LaSecWi Drift+  Drift- DrftErr Z_Charge_Div ',
     $'  Ionization  Drift_Time Status(4I3) Isajet track#'/
     $       '          (cm)    (cm)    (cm)      (cm)    ',
     $'      M.I.P.      (ns)   by3by2by1by0              ')
 1060     FORMAT(/,' LaSecWi Drift+  Drift- DrftErr Z_Charge_Div ',
     $'  Ionization  Drift_Time Status(4I3)'/
     $       '          (cm)    (cm)    (cm)      (cm)    ',
     $'      M.I.P.      (ns)   by3by2by1by0              ')
 1070     FORMAT(1X,I2,I3,I2,F7.4,F8.4,'+-',F6.4,F6.1,'+-',F4.1,
     $      F7.1,'+-',F6.1,F8.1,3X,4I3,I7,6X)
C
C ****  Next bank ?
C
   20     CONTINUE
          SEC = SEC + 1
        ENDDO
        LAY = LAY + 1
      ENDDO
C
  999 RETURN
      END

      SUBROUTINE PRVWDA( PRUNIT, JVWDA, NVWDA, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print VWDA bank(s)
C-
C-   Inputs  : PRUNIT : unit number
C-             JVWDA  : Pointer to one bank ( if CFL = 'SINGLE'
C-             NVWDA  : Id number of one bank( unused )
C-             CFL    : Character flag, 'ALL' = all VWDA banks
C-                      'ONE' = only the one with JVWDA
C-                      All other values act like 'ONE'
C-             IFL    : Level of print = 1, 2, 3 . see comments
C-   Outputs : print out on unit PRUNIT
C-
C-  Daria Zieminska Jan 1987
C-  Daria Zieminska May 1988  new bank format
C-  Peter Grudberg 16-JAN-1989 track id optional
C-  Peter Grudberg 25-JUN-1989 implement CFL, update use of IFL
C-  P.G. 03-OCT-1989 add vers. #, spec. print for 'ALL', IFL=1
C-   Updated  25-SEP-1992   Peter M. Grudberg  FIx for IBM compatibility 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:VTXLNK.INC'
      INTEGER PRUNIT, JVWDA, NVWDA, IFL
      CHARACTER*(*) CFL
      INTEGER KPVWDA, I, NVERS
      INTEGER LAY, SEC, IWIRE, NBSECT, NBLAYR, SECPR, LAYPR
      INTEGER NWIRE, NTOT, IEND, INDEX, IDATA, LAYER
      INTEGER NDATA(0:15), IPTR(0:15)
      INTEGER JD, IADD, ISTAT, IBYTE0, IBYTE1, IBYTE2, IBYTE3
      INTEGER LHIT, NHIT, NFADC, LISHIT(0:31), SECTOR
      INTEGER NUMSEC(0:2)
      DATA NUMSEC / 16, 32, 32 /
      LOGICAL FIRST
C----------------------------------------------------------------------
      IF ( CFL .EQ. 'ALL' .AND. IFL .EQ. 1 ) THEN
        FIRST = .TRUE.
C
C ****  Special print, one line per layer
C
        DO 50 LAY = 0, 2
          CALL VZERO( LISHIT(0), 32 )
          DO 60 SEC = 0, NUMSEC(LAY) - 1
            IF( LVWDA(SEC,LAY) .NE. 0 ) THEN
              LISHIT(SEC) = IQ( LVWDA(SEC,LAY)+1 )
              IF (FIRST) THEN
                FIRST = .FALSE.
                NVERS = IBITS(IQ(LVWDA(SEC,LAY)),13,5)
                WRITE(PRUNIT,4000) NVERS, (SECTOR,SECTOR=0,31)
 4000           FORMAT(/,'  Bank VWDA: (version',I2,')',/
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
 4001     FORMAT (/,1X,' Bank VWDA does not exist')
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
            KPVWDA = LVWDA( SEC, LAY )
          ELSE
            KPVWDA = JVWDA
          ENDIF
          IF( KPVWDA .EQ. 0 ) GOTO 20
          LAYPR = IBITS( IQ( KPVWDA-5 ), 9, 3 )
          SECPR = IBITS( IQ( KPVWDA-5 ), 4, 5 )
          NVERS = IBITS(IQ(KPVWDA),13,5)
C
C **** IFL = 1: print total number of hits
C
          WRITE( PRUNIT, 1000 ) NVERS, LAYPR, SECPR
 1000     FORMAT(/,'  Bank VWDA: (version',I2,') for Layer ',I1,
     &      ' Sector ',I2)
          WRITE( PRUNIT, 1010 ) (IQ( KPVWDA+I), I=1,3)
 1010     FORMAT(5X,'Number of pulses =',I6,' on the ',I3,' FADCs.',
     &           ' Each pulse is ',I3,' words long.'/)
C
C **** IFL = 2: print nuber of hits/FADC, pointers
C
          IF( IFL .LE. 1 ) GOTO 20
          LHIT  = IQ( KPVWDA+3 )
          NFADC = IQ( KPVWDA+2 )
          NWIRE = NFADC / 2                 ! 2 ends per wire
          NTOT  = IQ( KPVWDA+1 )
          IF ( NTOT .LE. 0 ) GOTO 20
          DO 100 IWIRE=0,7
            DO 100 IEND=0,1
              INDEX = IWIRE*2 + IEND
              NDATA(INDEX)= IQ(KPVWDA+INDEX+4)
              IPTR(INDEX) = IQ(KPVWDA+INDEX+4+NFADC)
  100     CONTINUE
          WRITE ( PRUNIT, 1020 ) ( I, I=0,NWIRE-1 )
          WRITE ( PRUNIT, 1030 ) ( NDATA(2*I), I=0,NWIRE-1 )
          WRITE ( PRUNIT, 1035 ) ( NDATA(2*I+1), I=0,NWIRE-1 )
          WRITE ( PRUNIT, 1040 ) ( IPTR(2*I), I=0,NWIRE-1 )
          WRITE ( PRUNIT, 1045 ) ( IPTR(2*I+1), I=0,NWIRE-1 )
 1020     FORMAT(5X,'Wire number         = ',8I6,/,
     &      5X,'---------------------------------------------',
     &      '--------------------------')
 1030     FORMAT(5X,'Num. of hits, end 0 = ',8I6)
 1035     FORMAT(5X,'Num. of hits, end 1 = ',8I6)
 1040     FORMAT(5X,'Hit pointers, end 0 = ',8I6)
 1045     FORMAT(5X,'Hit pointers, end 1 = ',8I6)
C
C **** full printout: IFL > 2
C
          IF ( IFL .LE. 2 ) GO TO 20
          IF ( LHIT .EQ. 9 ) THEN   ! 9 wrds/hit under GEAN
            WRITE ( PRUNIT, 1050 )
          ELSE                      ! no track id under RECO
            WRITE ( PRUNIT, 1060 )
          ENDIF
          DO 200 INDEX=0,15
            DO 200 IDATA=1, NDATA(INDEX)
              JD = KPVWDA + IPTR(INDEX) - 1 + LHIT*(IDATA-1)
              IADD   = IQ(JD+1)
              LAYER  = IBITS(IADD,9,3)
              SECTOR = IBITS(IADD,4,5)
              IWIRE  = IBITS(IADD,1,3)
              IEND   = IBITS(IADD,0,1)
C  Unpack status word
              ISTAT  = IQ(JD+8)
              IBYTE0 = IBITS(ISTAT,0,8)
              IBYTE1 = IBITS(ISTAT,8,8)
              IBYTE2 = IBITS(ISTAT,16,8)
              IBYTE3 = IBITS(ISTAT,24,8)
              IF ( LHIT .EQ. 9 ) THEN
                WRITE ( PRUNIT, 1070 ) LAYER,SECTOR,IWIRE,IEND,Q(JD+2),
     &            Q(JD+6),Q(JD+3),Q(JD+7),Q(JD+4),Q(JD+5),IBYTE3,IBYTE2,
     &            IBYTE1,IBYTE0,IQ(JD+9)
              ELSE                  ! no track id under RECO
                WRITE ( PRUNIT, 1070 ) LAYER,SECTOR,IWIRE,IEND,Q(JD+2),
     &            Q(JD+6),Q(JD+3),Q(JD+7),Q(JD+4),Q(JD+5),IBYTE3,IBYTE2,
     &            IBYTE1,IBYTE0
              ENDIF
  200     CONTINUE
 1050     FORMAT(/
     &      ' LaSecWiEnd     drift_time     pulse_area  pulse_width',
     $'  peak_hight  status(4I3)  track#'/
     $        '                   (ns)           (ns)        (ns)    ',
     $'             by3by2by1by0')
 1060     FORMAT(/
     &      ' LaSecWiEnd     drift_time     pulse_area  pulse_width',
     $'  peak_hight  status(4I3)'/
     $        '                   (ns)           (ns)        (ns)    ',
     $'             by3by2by1by0')
 1070     FORMAT(1X,I2,I3,I2,I3,F7.1,'+-',F6.1,F7.1,'+-',F6.1,F7.1,6X,
     $      F7.1,6X,4I3,I6)
C
C ****  Is a loop needed ?
C
   20     CONTINUE
          SEC = SEC + 1
        ENDDO
        LAY = LAY + 1
      ENDDO
C
  999 RETURN
      END

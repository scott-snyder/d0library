      SUBROUTINE PRDCDA( PRUNIT, JDCDA, NDCDA, CARFL, IPRFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print DCDA bank(s)
C-
C-   Inputs  : PRUNIT : unit number
C-             JDCDA  : Pointer to one bank ( if CARFL = 'SINGLE'
C-             NDCDA  : Id number of one bank( unused )
C-             CARFL  : Character flag, 'ALL' = all DCDA banks
C-                      'SINGLE' = only the one with JDCDA
C-             IPRFL  : Level of print = 1, 2, 3 . see comments
C-   Outputs : print out on unit PRUNIT
C-
C-   Created  12-AUG-1987   Olivier Callot
C-   Updated  11-FEB-1988   Ghita Rahal-Callot   
C-   Updated  14-JUL-1989   Qizhong Li-Demarteau  print version # 
C-                              and add print out for standard dump
C-   Updated  22-AUG-1992   Qizhong Li-Demarteau  added call GZDCDA
C-                              and fixed branches into in IF block
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INTEGER PRUNIT, JDCDA, NDCDA, IPRFL
      CHARACTER*(*) CARFL
      CHARACTER*8   ZFORMA
      INTEGER KPDCDA, KPCDCH, KPLAYR, KPDSEC, I, J, K, NVERS
      INTEGER LAY, SEC, IWIR, IHIT, NBSECT, NBLAYR, SECPR, LAYPR
      INTEGER LHIT, NHIT, NWIR, IPWI, IPHI, LISHIT(0:31), SECTOR
      INTEGER GZDCDA
      LOGICAL FIRST
C----------------------------------------------------------------------
      IF ( CARFL .EQ. 'ALL' .AND. IPRFL .EQ. 1 ) THEN
        FIRST = .TRUE.
C
C ****  Special print, one line per layer
C
        DO 50 LAY = 0, 3
          CALL VZERO( LISHIT(0), 32 )
          DO 60 SEC = 0, 31
            IF (LDCDA(SEC,LAY) .LE. 0) 
     &        LDCDA(SEC,LAY) = GZDCDA(SEC,LAY)
            IF (LDCDA(SEC,LAY) .NE. 0) THEN
              LISHIT(SEC) = IQ( LDCDA(SEC,LAY)+1 )
              IF (FIRST) THEN
                FIRST = .FALSE.
                NVERS = IBITS(IQ(LDCDA(SEC,LAY)),13,5) 
                WRITE(PRUNIT,4000) NVERS, (SECTOR,SECTOR=0,31)
 4000           FORMAT(/,'  Bank DCDA: (version',I2,')',/
     &          12X,'--- number of hits per layer and sectors ---'//
     &          ' L\S',32I4/)
              ENDIF
            ENDIF
   60     CONTINUE
          WRITE(PRUNIT, 4100) LAY, LISHIT
 4100     FORMAT(I2,2X,32I4)
   50   CONTINUE
        IF (FIRST) THEN
          WRITE (PRUNIT, 4001)
 4001     FORMAT (/,1X,' Bank DCDA does not exist')
        ENDIF
        GOTO 999
      ENDIF
C
C ****  Detailled or selective print
C
      NBLAYR = 1
      NBSECT = 1
      IF (CARFL .EQ. 'ALL') THEN
        NBLAYR = 4
        LAY = 0
    5   CONTINUE
        NBSECT = 32
        SEC = 0
   15   CONTINUE
        KPDCDA = LDCDA( SEC, LAY )
      ELSE
        KPDCDA = JDCDA
      ENDIF
 101  CONTINUE
      IF( KPDCDA .EQ. 0 ) GOTO 20
      LAYPR = IBITS( IQ( KPDCDA-5 ), 9, 2 )
      SECPR = IBITS( IQ( KPDCDA-5 ), 4, 5 )
      NVERS = IBITS(IQ(KPDCDA),13,5) 
C
C ****  Number on each wires 
C
      WRITE( PRUNIT, 1000 ) NVERS, LAYPR, SECPR
 1000 FORMAT('  Bank DCDA: (version',I2,') for Layer ',I1,' Sector ',I2)
      WRITE( PRUNIT, 1010 ) (IQ( KPDCDA+I), I=1,3)
 1010 FORMAT(10X,'Number of pulses =',I6,'  on the ',I3,' FADCs.',
     &           ' Each pulse is ',I3,' words long.'/)
      IF( IPRFL .LE. 1 ) GOTO 20
      LHIT = IQ( KPDCDA+3)
      NWIR = IQ( KPDCDA+2)
      WRITE( PRUNIT, 1020 ) (I-1,I=1,NWIR)
      WRITE( PRUNIT, 1021 ) (IQ( KPDCDA+I+3), I=1,NWIR)
      WRITE( PRUNIT, 1022 ) (IQ( KPDCDA+I+NWIR+3), I=1,NWIR)
 1020 FORMAT(10X,'FADC number      = ',12I5)
 1021 FORMAT(10X,'Number of pulses = ',12I5)
 1022 FORMAT(10X,'Pointer to pulse = ',12I5)
C
C ****  Detailled print-out ( IPRFL > 2 )
C
      IF( IPRFL .LE. 2 ) GOTO 20
      WRITE( PRUNIT, 1023 )
 1023 FORMAT(/'    numb      time      area     width      peak',
     &              '  err-time  err-area    status     GEANT'/)
      DO 30 IWIR = 1, NWIR
        NHIT = IQ( KPDCDA+IWIR+3 )
        IPWI = IQ( KPDCDA+NWIR+IWIR+3)
        DO 40 IHIT = 1, NHIT
          IPHI = KPDCDA + IPWI + (IHIT-1) * LHIT
          WRITE( PRUNIT, 1030 ) ZFORMA(IQ(IPHI+1)),
     &          ( Q(IPHI+I), I=2,7), ZFORMA(IQ(IPHI+8)),
     &          (IQ(IPHI+I),I=9,LHIT)
 1030     FORMAT(A8,6F10.2,2X,A8,5I7)
   40   CONTINUE
   30 CONTINUE
C
C ****  Is a loop needed ?
C
   20 CONTINUE
      SEC = SEC + 1
      IF (SEC .LT. NBSECT) THEN
        KPDCDA = LDCDA(SEC, LAY)
        GOTO 101
      ENDIF
   10 CONTINUE
      LAY = LAY + 1
      IF (LAY .LT. NBLAYR) THEN
        NBSECT = 32
        SEC = 0
        KPDCDA = LDCDA(SEC, LAY)
        GO TO 101
      ENDIF
C
  999 RETURN
      END

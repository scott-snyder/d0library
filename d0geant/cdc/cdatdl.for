      SUBROUTINE CDATDL ( IFADC, HITDL, NHIT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill the Zebra structure
C-                         (CDCH -- DLYR -- DSEC)--DCDA from the hits HIT
C-                         given by GEANT ( JHITS ) For the Delay Lines
C-
C-
C-   Inputs  : IFADC(3) = layer, sector, wire
C-   Outputs : none
C-
C-   Created   8-FEB-1988   Ghita Rahal-Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$LINKS:IZDALH.LINK'
C
C
      INTEGER IFADC(3), NHIT(2), IDELAY
      INTEGER NMAX, NWORD, I
      PARAMETER ( NWORD = 18, NMAX = 50 )
      REAL HITDL(NWORD, 2*NMAX)
      REAL TIMPOS(2*NMAX), TIMNEG(2*NMAX), ERTPOS(2*NMAX),
     &     ERTNEG(2*NMAX)
C
      REAL X, Y, Z, DIST
      INTEGER LDALL, LDALS, IPT, NHITOT
      REAL CARSET ( 200 )
      REAL DATAS ( NWORD, 2*NMAX )
      INTEGER IADR(3), ITIMES ( 2*NMAX ), IHIT
C----------------------------------------------------------------------
C
C **** DL corresponding to the SW 0 :  7 for the +Z end
C ****                                 8   "   " -Z end
C **** DL corresponding to the SW 6 :  9 for the +Z end
C ****                                 10  "   " -Z end
C
      IDELAY = 9
      IF (IFADC(3) .EQ. 1 ) IDELAY = 7
C
C ****  Compute the Drift distance in the cell
C
      LDALH = LC ( LSCDC - IZDALH )
      IF ( LDALH .LE. 0 ) THEN
        WRITE ( LOUT, * ) '****** CDHITS : bank LDALH not defined'
        CALL EXIT(1)
      ENDIF
      LDALL = LC ( LDALH - IFADC(1) - 1 )
      IF ( LDALL .LE. 0 ) THEN
        WRITE ( LOUT, * ) '****** CDHITS : bank LDALL not defined',
     &    ' for the layer', IFADC(1)
        CALL EXIT(1)
      ENDIF
      LDALS = LC ( LDALL - IFADC(2) - 1 )
      IF ( LDALS .LE. 0 ) THEN
        WRITE ( LOUT, * ) '****** CDHITS : bank LDALS not defined',
     &    ' for the sector ', IFADC(2)
        CALL EXIT(1)
      ENDIF
C
C ****  Get time in one Z_end of the delay line
C ****  and perform digitization
C
      CALL UCOPY ( IFADC, IADR, 3)
      NHITOT = NHIT(1) + NHIT(2)
      IF ( NHIT(1) .NE. 0 ) THEN
        IADR(3) = IFADC(3) - 1
      ENDIF
      IPT = LDALS + 6 + IC ( LDALS + 6 ) * IADR(3)
      DO 300 IHIT = 1, NHITOT
        X = (HITDL(1,IHIT) + HITDL(4,IHIT)) / 2.
        Y = (HITDL(2,IHIT) + HITDL(5,IHIT)) / 2.
        DIST=  (X-C(IPT+1)) * C(LDALS+3)  + (Y-C(IPT+2)) * C(LDALS+4)
        Z = (HITDL ( 3, IHIT ) + HITDL ( 6, IHIT ))/ 2.
        Z = Z - C(IPT + 3)
        CALL CDELAY ( IADR, DIST, Z, TIMPOS(IHIT), TIMNEG(IHIT),
     &                ERTPOS(IHIT), ERTNEG(IHIT) )
        IF ( IHIT .EQ. NHIT(1) ) THEN
          IADR(3) = IFADC(3)
          IPT = LDALS + 6 + IC ( LDALS + 6 ) * IADR(3)
        ENDIF
  300 CONTINUE
C
C ****  Order hits by time on the +Z end of the DL
C
      CALL SORTZV (  TIMPOS, ITIMES, NHITOT, 1, 0, 0, 0, CARSET )
C
C ****  Get the array DATAS ( to fill the ZEBRA Data banks )
C
      CALL DVECDA ( HITDL, NWORD, NHITOT, TIMPOS, ERTPOS,
     &              ITIMES, IDELAY, DATAS )
C
C ****  Put the hits in Zebra Structure CDCH -- DLYR -- DSEC --DCDA
C
      CALL UCOPY ( IFADC, IADR, 3 )
      IADR ( 3 ) = IDELAY
      CALL FIDCDA ( DATAS, NHITOT, IADR )
C
C
C ****  Order hits by time on the -Z end of the DL
C
      IDELAY = IDELAY + 1
      CALL SORTZV (  TIMNEG, ITIMES, NHITOT, 1, 0, 0, 0, CARSET )
C
C ****  Get the array DATAS ( to fill the ZEBRA Data banks )
C
      CALL DVECDA ( HITDL, NWORD, NHITOT, TIMNEG, ERTNEG,
     &              ITIMES, IDELAY, DATAS )
C
C ****  Put the hits in Zebra Structure CDCH -- DLYR -- DSEC --DCDA
C
      CALL UCOPY ( IFADC, IADR, 3 )
      IADR ( 3 ) = IDELAY
      CALL FIDCDA ( DATAS, NHITOT, IADR )
  999 RETURN
      END

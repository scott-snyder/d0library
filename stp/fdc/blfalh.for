      SUBROUTINE BLFALH
C------------------------------------------------------------------------
C-
C-   Purpose and Methods : Build the Alignment banks
C-
C-   Inputs  : None, but ZEBRA structure up to SFDC should exist.
C-   Outputs : Fills FDC Alignment Zebra Banks
C-
C-   Created  15-JUL-1988   Jeffrey Bantly
C-   Updated  15-DEC-1988   Jeffrey Bantly   put in design wire locations
C-   Updated   3-OCT-1989   Jeffrey Bantly   re-organize geometry for speedup
C-   Updated  19-FEB-1990   Jeffrey Bantly   modify to use logical format
C-   Updated   2-MAY-1990   Jeffrey Bantly   correct Phi stagger bug 
C-   Updated  13-AUG-1992   Robert E. Avery  Change COSD, SIND to use radians. 
C-   Updated  13-AUG-1992   Robert E. Avery  Make FALH into
C-      linear chains, containing different versions of MC STP banks.
C-      FTRAKS package will choose correct bank based on flag in HSTR bank.
C-
C------------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INCLUDE 'D0$LINKS:IZFALH.LINK'
      INCLUDE 'D0$LINKS:IZFGEH.LINK'
      INCLUDE   'D0$LINKS:IZFWAL.LINK'
      INCLUDE     'D0$LINKS:IZFWTA.LINK'
      INCLUDE     'D0$LINKS:IZFWTB.LINK'
      INCLUDE     'D0$LINKS:IZFWPH.LINK'
      INCLUDE   'D0$LINKS:IZFDRT.LINK'
      INCLUDE     'D0$LINKS:IZFDTA.LINK'
      INCLUDE     'D0$LINKS:IZFDTB.LINK'
      INCLUDE     'D0$LINKS:IZFDPH.LINK'
C
      INTEGER MPFALH(5), MPFAHF(5), MPFATH(5)
      INTEGER MPFAQD(5), MPFAPH(5), MPFASE(5)
      INTEGER LOWRUN, HIGRUN, NUMTWR, NUMPWR
      INTEGER NPARWR, NPARDL, NUMTDL, NUMPDL
      INTEGER HALF, THETA, UNIT, QUAD, SECTOR
      INTEGER WIRE, DELAY
      INTEGER LBANK, LKFASE
      INTEGER NWLPAR, NWAPAR, NWBPAR, NWPPAR
      INTEGER NDTPAR, NDAPAR, NDBPAR, NDPPAR
      PARAMETER( NUMTWR = 8 )
      PARAMETER( NUMTDL = 1 )
      PARAMETER( NUMPWR = 16 )
      PARAMETER( NUMPDL = 0 )
C
      REAL XCENT(0:NUMTWR-1), YCENT(0:NUMTWR-1), ZCENT(0:NUMTWR-1)
      REAL THETAT(0:NUMTWR-1), PHIT(0:NUMTWR-1), OMEGAT(0:NUMTWR-1)
      REAL XCENP(0:NUMPWR-1), YCENP(0:NUMPWR-1), ZCENP(0:NUMPWR-1)
      REAL THETAP(0:NUMPWR-1), PHIP(0:NUMPWR-1), OMEGAP(0:NUMPWR-1)
      REAL RO, RI, PPHI, XO, YO, XI, YI, XC, YC, M1, B1, STAG, ZH, ZT
C
      LOGICAL FIRST
C
      DATA MPFALH / 0,  3,  3,  2,  2 /
      DATA MPFAHF / 0,  2,  2,  2,  2 /
      DATA MPFATH / 0,  8,  8,  2,  2 /
      DATA MPFAQD / 0,  6,  6,  2,  2 /
      DATA MPFAPH / 0, 36, 36,  2,  2 /
      DATA MPFASE / 0,  0,  0,  0,  0 /
      DATA LOWRUN,HIGRUN,NPARWR,NPARDL / 999999998, 999999999, 6, 6/
      DATA XCENT,YCENT,ZCENT,THETAT,PHIT,OMEGAT/ NUMTWR*0.0,
     &          NUMTWR*0.0,NUMTWR*0.0,NUMTWR*0.0,NUMTWR*0.0,NUMTWR*0.0/
      DATA XCENP,YCENP,ZCENP,THETAP,PHIP,OMEGAP/ NUMPWR*0.0,
     &          NUMPWR*0.0,NUMPWR*0.0,NUMPWR*0.0,NUMPWR*0.0,NUMPWR*0.0/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL UCTOH('FALH',MPFALH(1),4,4)
        CALL UCTOH('FAHF',MPFAHF(1),4,4)
        CALL UCTOH('FATH',MPFATH(1),4,4)
        CALL UCTOH('FAQD',MPFAQD(1),4,4)
        CALL UCTOH('FAPH',MPFAPH(1),4,4)
        CALL UCTOH('FASE',MPFASE(1),4,4)
      ENDIF
C
C  Create the permanent link area FDCPRM, if not already done.
C
      CALL FSPLNK
C
C  Get geometry bank constants and locations
C
      IF(LSFDC.EQ.0) CALL BKSFDC(LBANK)
      IF(LFGEH.EQ.0) CALL BLFGEH
      IF(LFGEH.EQ.0) GOTO 999
      LFWAL = LC( LFGEH - IZFWAL )
      LFWTA = LC( LFWAL - IZFWTA )
      LFWTB = LC( LFWAL - IZFWTB )
      LFWPH = LC( LFWAL - IZFWPH )
      LFDRT = LC( LFGEH - IZFDRT )
      LFDTA = LC( LFDRT - IZFDTA )
      LFDTB = LC( LFDRT - IZFDTB )
      LFDPH = LC( LFDRT - IZFDPH )
      NWLPAR = IC( LFWAL + 2 )
      NWAPAR = IC( LFWTA + 2 )
      NWBPAR = IC( LFWTB + 2 )
      NWPPAR = IC( LFWPH + 2 )
      NDAPAR = IC( LFDTA + 3 )
      NDBPAR = IC( LFDTB + 3 )
      NDPPAR = IC( LFDPH + 3 )
C
C  Make top level alignment bank FALH
C
      CALL MZFORM( 'FASE', '6I -F', MPFASE(5) )
      IF ( LC( LSFDC-IZFALH) .EQ. 0 ) THEN
        CALL MZLIFT( IDVSTP, LFALH, LSFDC, -IZFALH, MPFALH, -1 )
        IC( LFALH-5 ) = 1
        IC( LFALH+1 ) = LOWRUN
        IC( LFALH+2 ) = HIGRUN
      ELSE
        GOTO 999
      ENDIF
C
      DO 10 HALF = 0, 1
        CALL MZLIFT( IDVSTP, LFAHF(HALF), LFALH, -(HALF+1), MPFAHF, -1)
        IC( LFAHF(HALF)-5 ) = HALF
        IC( LFAHF(HALF)+1 ) = LOWRUN
        IC( LFAHF(HALF)+2 ) = HIGRUN
        ZH = C( LFGEH+8 ) * ((-1.)**(HALF+1))
C
C   Alignment Banks for the Theta Chambers
C
        UNIT = 0
        CALL MZLIFT( IDVSTP, LFAUN(HALF,UNIT), LFAHF(HALF), -(UNIT+1),
     &                             MPFATH, -1)
        IC( LFAUN(HALF,UNIT)-5 ) = UNIT
        IC( LFAUN(HALF,UNIT)+1 ) = LOWRUN
        IC( LFAUN(HALF,UNIT)+2 ) = HIGRUN
        DO 30 QUAD = 0, 7
          THETA = 0
          IF( QUAD.GE.4) THETA = 1
          ZT = C( LFWAL+2+4+(2*NWLPAR) ) * ((-1.)**(HALF+THETA))
          CALL MZLIFT( IDVSTP, LFAQD(HALF,QUAD), LFAUN(HALF,
     &        UNIT), -(QUAD+1), MPFAQD,-1)
          IC( LFAQD(HALF,QUAD)-5 ) = QUAD
          IC( LFAQD(HALF,QUAD)+1 ) = LOWRUN
          IC( LFAQD(HALF,QUAD)+2 ) = HIGRUN
          DO 40 SECTOR = 0, 5
            MPFASE(4) =  60
            CALL MZLIFT( IDVSTP, LFASE(HALF,UNIT,QUAD,SECTOR),
     &                LFAQD(HALF,QUAD), -(SECTOR+1), MPFASE, -1)
            IC( LFASE(HALF,UNIT,QUAD,SECTOR)-5 ) = SECTOR
            IC( LFASE(HALF,UNIT,QUAD,SECTOR)+1 ) = LOWRUN
            IC( LFASE(HALF,UNIT,QUAD,SECTOR)+2 ) = HIGRUN
            IC( LFASE(HALF,UNIT,QUAD,SECTOR)+3 ) = NUMTWR
            IC( LFASE(HALF,UNIT,QUAD,SECTOR)+4 ) = NPARWR
            IC( LFASE(HALF,UNIT,QUAD,SECTOR)+5 ) = NUMTDL
            IC( LFASE(HALF,UNIT,QUAD,SECTOR)+6 ) = NPARDL
            LKFASE = LFASE(HALF,UNIT,QUAD,SECTOR)
            LKFASE = LKFASE + 6
            DO 50 WIRE = 0, NUMTWR-1
              IF(QUAD.EQ.1 .OR. QUAD.EQ.3 .OR. QUAD.EQ.5
     &                                        .OR. QUAD.EQ.7) THEN
                XCENT(WIRE) = 0.
                IF(QUAD .LE. 3) THEN
                  YCENT(WIRE) = ( C( LFWTA+9+(SECTOR*NWAPAR)+6 ) +
     &                   C( LFDTA+5+25+SECTOR ) ) * ( (QUAD-2)*(-1.))
                  IF( SECTOR .GE. 3 ) YCENT(WIRE) = YCENT(WIRE) +
     &                   (C( LFDTA+5+31+WIRE ) * ( (QUAD-2)*(-1.)))
                ELSE
                  YCENT(WIRE) = ( C( LFWTA+9+(SECTOR*NWAPAR)+6 ) +
     &                   C( LFDTA+5+25+SECTOR ) ) * ( (QUAD-6)*(-1.))
                  IF( SECTOR .GE. 3 ) YCENT(WIRE) = YCENT(WIRE) +
     &                   (C( LFDTA+5+31+WIRE ) * ( (QUAD-6)*(-1.)))
                ENDIF
                IF( THETA .EQ. 0 ) THEN
                  XCENT(WIRE) = ABS(YCENT(WIRE)) *
     &                   COS(((QUAD)*90.+45.)*RADIAN)
                  YCENT(WIRE) = ABS(YCENT(WIRE)) *
     &                   SIN(((QUAD)*90.+45.)*RADIAN)
                ENDIF
                ZCENT(WIRE) = ZH + ZT -
     &                   ( C( LFWTA+9+(SECTOR*NWAPAR)+7) +
     &                   (C( LFDTA+5+38+(SECTOR*NDAPAR)+7) + 5.05) +
     &                   C( LFDTA+6+WIRE ) ) * ((-1.)**(HALF+THETA))
              ELSE
                IF(QUAD.LE.3) THEN
                  XCENT(WIRE) = ( C( LFWTB+9+(SECTOR*NWBPAR)+5 ) +
     &                   C( LFDTB+5+25+SECTOR ) ) * ( (QUAD-1)*(-1.))
                  IF( SECTOR .GE. 3 ) XCENT(WIRE) = XCENT(WIRE) +
     &                   (C( LFDTB+5+31+WIRE ) * ( (QUAD-1)*(-1.)))
                ELSE
                  XCENT(WIRE) = ( C( LFWTB+9+(SECTOR*NWBPAR)+5 ) +
     &                   C( LFDTB+5+25+SECTOR ) ) * ( (QUAD-5)*(-1.))
                  IF( SECTOR .GE. 3 ) XCENT(WIRE) = XCENT(WIRE) +
     &                   (C( LFDTB+5+31+WIRE ) * ( (QUAD-5)*(-1.)))
                ENDIF
                YCENT(WIRE) = 0.
                IF( THETA .EQ. 0 ) THEN
                  YCENT(WIRE) = ABS(XCENT(WIRE)) *
     &                   SIN(((QUAD)*90.+45.)*RADIAN)
                  XCENT(WIRE) = ABS(XCENT(WIRE)) *
     &                   COS(((QUAD)*90.+45.)*RADIAN)
                ENDIF
                ZCENT(WIRE) = ZH + ZT -
     &                   ( C( LFWTB+9+(SECTOR*NWBPAR)+7) +
     &                   (C( LFDTB+5+38+(SECTOR*NDBPAR)+7) + 5.05) +
     &                   C( LFDTB+6+WIRE ) ) * ((-1.)**(HALF+THETA))
              ENDIF
              C( LKFASE+1 ) = XCENT(WIRE)
              C( LKFASE+2 ) = YCENT(WIRE)
              C( LKFASE+3 ) = ZCENT(WIRE)
              C( LKFASE+4 ) = THETAT(WIRE)
              C( LKFASE+5 ) = PHIT(WIRE)
              C( LKFASE+6 ) = OMEGAT(WIRE)
              LKFASE = LKFASE + NPARWR
   50       CONTINUE
            DO 60 DELAY = 0, NUMTDL-1             !  adjacent to wire 0
              C( LKFASE+1 ) = XCENT(0)
              C( LKFASE+2 ) = YCENT(0)
              C( LKFASE+3 ) = ZCENT(0)
              C( LKFASE+4 ) = THETAT(0)
              C( LKFASE+5 ) = PHIT(0)
              C( LKFASE+6 ) = OMEGAT(0)
              LKFASE = LKFASE + NPARDL
   60       CONTINUE
   40     CONTINUE
   30   CONTINUE
C
C   Alignment Banks for the Phi Chambers
C
        UNIT=1
        QUAD=0
        CALL MZLIFT( IDVSTP, LFAUN(HALF,UNIT), LFAHF(HALF),
     &                            -(UNIT+1), MPFAPH, -1)
        IC( LFAUN(HALF,UNIT)-5 ) = UNIT
        IC( LFAUN(HALF,UNIT)+1 ) = LOWRUN
        IC( LFAUN(HALF,UNIT)+2 ) = HIGRUN
        RO = C( LFDPH+69+3 )
        RI = C( LFDPH+69+2 )
        DO 70 SECTOR = 0, 35
          MPFASE(4) = 102
          CALL MZLIFT( IDVSTP, LFASE(HALF,UNIT,QUAD,SECTOR),
     &              LFAUN(HALF,UNIT), -(SECTOR+1), MPFASE, -1)
          IC( LFASE(HALF,UNIT,QUAD,SECTOR)-5 ) = SECTOR
          IC( LFASE(HALF,UNIT,QUAD,SECTOR)+1 ) = LOWRUN
          IC( LFASE(HALF,UNIT,QUAD,SECTOR)+2 ) = HIGRUN
          IC( LFASE(HALF,UNIT,QUAD,SECTOR)+3 ) = NUMPWR
          IC( LFASE(HALF,UNIT,QUAD,SECTOR)+4 ) = NPARWR
          IC( LFASE(HALF,UNIT,QUAD,SECTOR)+5 ) = NUMPDL
          IC( LFASE(HALF,UNIT,QUAD,SECTOR)+6 ) = NPARDL
          LKFASE = LFASE(HALF,UNIT,QUAD,SECTOR)
          LKFASE = LKFASE + 6
          DO 80 WIRE = 0, NUMPWR-1
            ZCENP(WIRE) = (C( LFGEH+8 ) + C( LFDPH+6+WIRE )) *
     &                     ((-1.)**(HALF+1))
C                 calculate exact center of wire using analytic geometry in x,y
            PPHI = 10.*SECTOR + 5.
            XO = RO * COS( PPHI * RADIAN)
            YO = RO * SIN( PPHI * RADIAN)
            XI = RI * COS( PPHI * RADIAN)
            YI = RI * SIN( PPHI * RADIAN)
            XC = ( XO + XI ) / 2.
            YC = ( YO + YI ) / 2.
            M1 = ( YO - YI ) / ( XO - XI )
            B1 = YO - M1 * XO
            STAG = C( LFDPH+54+WIRE )
            YCENP(WIRE) = YC + (STAG*SIN((PPHI+90.)*RADIAN))
            XCENP(WIRE) = XC + (STAG*COS((PPHI+90.)*RADIAN))
            C( LKFASE+1 ) = XCENP(WIRE)
            C( LKFASE+2 ) = YCENP(WIRE)
            C( LKFASE+3 ) = ZCENP(WIRE)
            C( LKFASE+4 ) = THETAP(WIRE)
            C( LKFASE+5 ) = PHIP(WIRE)
            C( LKFASE+6 ) = OMEGAP(WIRE)
            LKFASE = LKFASE + NPARWR
   80     CONTINUE
   70   CONTINUE
   10 CONTINUE
C
C---------------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END

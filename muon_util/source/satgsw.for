      SUBROUTINE SATGSW(DIR,LTRG2,RT0,AT0,PH0,BT0,SBH,SAH,
     &                  MAGH,COSBM,TUBE_ADD,GEOM_ADD,NSH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Trigger #2 for SAMUS :
C-                         search 3-points in A, B stations
C-                         with the same PHI polar angle.
C-
C-   Inputs  : DIR    - SAMUS direction (1 - N, 2 - S).
C-   Outputs : LTRG2  - number of the founded triggers.
C-             RT0(24) - R-distances in point interactions.
C-             AT0(24) - TAN(THETA) of tracks after magnet.
C-             PH0(24) - PHI-angles of tracks.
C-             BT0(24) - TAN(THETA) of tracks before magnet.
C-             SBH(24,3)- Samus B hits
C-             SAH(24,3)- Samus A hits
C-             MAGH(24,3)  - Ficticeous magnet hit 
C-             COSBM(24,3) - Direction cosines before magnet
C-             TUBE_ADD(40)- packed tube address of used triplets
C-             GEOM_ADD(40)- geometry address 
C-             NSH(24)     - # hits in each road 
C-   Controls: 
C-
C-   Created  16-FEB-1994   Alexei Volkov  
C-   Modified 12-JUL-1994   Andre Sznajder (Addition of some output
C-                                           variables and L2 flag)
C-   Modified 20-FEB-1995   Andre Sznajder ( output tube and geom. address)
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER DIR,LTRG1,LTRG2,LTR2,LT2
      INTEGER LSAMT,LSAHS,GZSAMT,GZSAHS
      INTEGER NLT,NJA,NJB,NTRPL,NTRG2,N_PLANES,STATION
      INTEGER NST,IST,IND,INDA,INDB,LDA,LDB
      INTEGER MA,MB,NA,NB,NWA,NWB,KEY
      INTEGER IPL,L3,L1,N3,N,N1,NHITS,NW,NBI,NS
      INTEGER NSPAR,IBUF,NBUF(7),IERR,IBITS,MXR,MSH
      PARAMETER( MSH=40 )    ! Max # hits in a road
      PARAMETER( MXR = 24 )  ! Max. # of roads
      INTEGER NSH(MXR),TUBE_ADD(MXR,MSH),GEOM_ADD(MXR,MSH)
      INTEGER NAA(MXR),NBB(MXR),NAC(MXR)
      REAL    WDD(MXR),RT0(MXR),AT0(MXR),PH0(MXR),BT0(MXR)
      REAL    CENTER(3),ANGLES(3),SIZE(3),HOLE(3)
      REAL    RT1,RT2,WDB,DB,TB,APHI,PHIM
      REAL    PXA,PYA,PXB,PYB,PXC,PYC,SBA,SBB
      REAL    RA,RB,ZA,ZB,ZC,RTEMP,RTA,RMIN,RMAX 
      REAL    XY,XYMIN,AMN,AMX,TAM,TBM,ZMAG,SCAL,HA
      REAL    DR2AB,TG2AB,DISCM,ZBEAM
      REAL    SBH(MXR,3),SAH(MXR,3),MAGH(MXR,3),COSBM(MXR,3)
      REAL    SPAR(6),XPAR(3),ROTM(3,3)
      CHARACTER*4 HSHAPE
      LOGICAL FIRST
      PARAMETER (N_PLANES=3)
      SAVE    NTRPL,NTRG2,FIRST
      SAVE    DR2AB,TG2AB,DISCM,ZBEAM
      DATA FIRST /.TRUE./
C
C
C ****  calculate initial parameters
C
      LTRG2 = 0
      LSAMT = GZSAMT ()
      LSAHS = GZSAHS ()
      IF (LSAHS .LE. 0) GO TO 999
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET  ('NTRPL', NTRPL, IERR)
        CALL EZGET  ('NTRG2', NTRG2, IERR)
        CALL EZGET  ('TG2AB', TG2AB, IERR)
        CALL EZGET  ('DISCM', DISCM, IERR)
        CALL EZGET  ('DR2AB', DR2AB, IERR)
        CALL EZGET  ('ZBEAM', ZBEAM, IERR)
        CALL EZRSET
      END IF
      IQ(LSAHS+30+DIR) = 0
      IF (DIR .EQ. 1) THEN
        NST = 1
      ELSE
        NST = 4
      END IF
      CALL GTSMAG (DIR, HSHAPE, NSPAR, SPAR, XPAR, ROTM,
     &             NBUF, IBUF)
      ZMAG = XPAR(3)
      ZMAG = ZMAG - ZBEAM
      STATION = NST + 2
      CALL SAGSTA(STATION,CENTER,ANGLES,SIZE,HOLE)
      ZC = CENTER(3)
C
C ****  check number of 3-plets in A and B stations 
C
      INDA = 18 + NST
      INDB = 19 + NST
      MA = IQ(LSAHS+INDA)       ! number of the 3-plets in A station
      IF (MA .LE. 0) GO TO 999
      MB = IQ(LSAHS+INDB)       ! number of the 3-plets in B station
      IF (MB .LE. 0) GO TO 999
C
C ****  find PHI-coincidence in A and B stations
C
      LDB = LQ(LSAHS-INDB)      ! address of the 3-plets bank in B
      DO 100 NB = 1, MB
C
C ****  loop on 3-plets in B station
C
        NWB = IQ(LDB+1)
        IF (NWB .GE. 0 .AND. NWB .LT. NTRPL) GO TO 100
        IF (NWB .GT. 32) NWB = 32
        PXB = Q(LDB+2)
        PYB = Q(LDB+3)
        RB = SQRT (PXB * PXB + PYB * PYB)
        ZB = Q(LDB+4)
        ZB = ZB - ZBEAM
        TB = RB / ZB
        KEY = 0
        IF (ABS(PXB) .GE. ABS(PYB)) KEY = 1
        IF (KEY .EQ. 0) THEN
          RTEMP = PXB
          PXB = PYB
          PYB = RTEMP
        END IF
        SBB = ABS(PXB / RB)
        XY = PYB / PXB
        DB = SBB * TG2AB
C
C ****  loop on 3-plets in A station
C
        LDA = LQ(LSAHS-INDA)    ! address of the 3-plets bank in A
        DO 200 NA = 1, MA
          NWA = IQ(LDA+1)
          IF (NWA .GE. 0 .AND. NWA .LT. NTRPL) GO TO 200
          IF (NWA .GT. 32) NWA = 32
          PXA = Q(LDA+2)
          PYA = Q(LDA+3)
          ZA =  Q(LDA+4)
          ZA = ZA - ZBEAM
          RA = SQRT (PXA * PXA + PYA * PYA)
          RTA = TB * ZA
          RMIN = RTA - DR2AB
          RMAX = RTA + DR2AB
          IF (RA .LE. RMIN .OR. RA .GE. RMAX) GO TO 200
          IF (KEY .EQ. 0) THEN
            RTEMP = PXA
            PXA = PYA
            PYA = RTEMP
          END IF
          IF (PXA .GT. 0. .AND. PXB .LE. 0.) GO TO 200
          IF (PXA .LT. 0. .AND. PXB .GE. 0.) GO TO 200
          SBA = XY * PXA
          AMN = SBA - DB
          AMX = SBA + DB
          IF (PYA .LT. AMN .OR. PYA .GT. AMX) GO TO 200
C
C ****  find region in C station WAMUS
C
          TBM = RA / ZA
          RT1 = TBM * ZMAG
          TAM = (RB - RT1) / (ZB - ZMAG)
          RT2 = RT1 - TAM * ZMAG
          PXB = Q(LDB+2)
          PYB = Q(LDB+3)
          RT1 = (RT2 + TAM * ZC) / RB
          PXC = ABS(RT1 * PXB)
          PYC = ABS(RT1 * PYB)
          XYMIN = SIZE(1) + DISCM + DISCM
C>>>>               for testing - add SAMUS CUT in C LAYER
          IF (PXC .LT. XYMIN .AND. PYC .LT. XYMIN) GO TO 200
          IF (ABS(PXB) .LT. 1.0E-6) PXB = 1.0E-6
          APHI = ATAN (ABS(PYB / PXB))
          IF (PYB .LT. 0. .AND. PXB .GT. 0.) THEN
            PHIM = TWOPI - APHI
          ELSE IF (PYB .LT. 0. .AND. PXB .LT. 0.) THEN
            PHIM = PI + APHI
          ELSE IF (PYB .GT. 0. .AND. PXB .LT. 0.) THEN
            PHIM = PI - APHI
          ELSE
            PHIM = APHI
          END IF
          NS = NWA + NWB
          WDB = ABS(SBA - PYA)
          LTR2 = LTRG2
  500     CONTINUE
          IF (LTR2 .GT. 0) THEN
            IF (NA .NE. NAA(LTR2)) THEN
              IF (NB .NE. NBB(LTR2)) THEN
                LTR2 = LTR2 - 1
                GO TO 500
              END IF
            END IF
            IF (NS .LT. NAC(LTR2)) GO TO 200
            IF (NS .EQ. NAC(LTR2)) THEN
              IF (WDB .GE. WDD(LTR2)) GO TO 200
            END IF
            LT2 = LTR2
          ELSE
            LTRG2 = LTRG2 + 1
            LT2 = LTRG2
            IF (LTRG2 .GT. NTRG2) LTRG2 = 0
            IF (LTRG2 .LE. 0) GO TO 999
          END IF
          NAA(LT2) = NA
          NBB(LT2) = NB
          NAC(LT2) = NS
          WDD(LT2) = WDB
          RT0(LT2) = RT2
          AT0(LT2) = TAM
          PH0(LT2) = PHIM
          BT0(LT2) = TBM
          SBH(LT2,1) =Q(LDB+2)
          SBH(LT2,2) =Q(LDB+3)
          SBH(LT2,3) =Q(LDB+4)
          SAH(LT2,1) =Q(LDA+2)
          SAH(LT2,2) =Q(LDA+3)
          SAH(LT2,3) =Q(LDA+4)
          IF (ZA.NE.0.) SCAL=ABS(ZMAG/ZA)
          MAGH(LT2,1)=Q(LDA+2)*SCAL
          MAGH(LT2,2)=Q(LDA+3)*SCAL
          MAGH(LT2,3)=XPAR(3)
          HA=SQRT(RA**2+ZA**2)
          IF (HA.NE.0.) THEN
            COSBM(LT2,1)=Q(LDA+2)/HA
            COSBM(LT2,2)=Q(LDA+3)/HA
            COSBM(LT2,3)=ZA/HA
          ENDIF
  200   LDA = LDA + 4
  100 LDB = LDB + 4
C
C ****  mark "good" hits if in full tracking mode
C
      IF (LTRG2 .LE. 0) GO TO 999
      CALL VZERO(NSH,24)
      DO NLT = 1, LTRG2
        LDA = LQ(LSAHS-INDA)    ! address of the 3-plets bank in A
        LDB = LQ(LSAHS-INDB)    ! address of the 3-plets bank in B
        NJA = LDA + 4 * NAA(NLT) - 3
        NJB = LDB + 4 * NBB(NLT) - 3
        IQ(NJA) = -NLT
        IQ(NJB) = -NLT
        DO IST = NST, NST+1
          IND = 18 + IST
          LTRG1 = IQ(LSAHS+IND)            ! number of the 3-hits
          L3 = LQ(LSAHS-IND)               ! address of the 3-hits
          DO N3 = 1, LTRG1
            IF (IQ(L3+1) .EQ. -NLT) THEN ! "good" 3-hit
              NW = (N3 - 1) / 32
              IF (NW+5 .LE. 13) THEN
                NBI = N3 - NW * 32 - 1
                DO IPL = 1, N_PLANES
                  IND = 3 * (IST - 1) + IPL
                  L1 = LQ(LSAHS-IND)          ! address of the hits
                  NHITS = IQ(LSAHS+IND)
                  DO N1 = 1, NHITS
                    N = IBITS (IQ(L1+NW+5), NBI, 1)
                    IF (N .EQ. 1) THEN
                      IQ(L1+1) = 3
                      NSH(NLT)=NSH(NLT)+1             ! # hits in road NLT
                      IF (NSH(NLT).GE.MSH) GOTO 800   ! protection  MSH=40
                      TUBE_ADD(NLT,NSH(NLT))=IQ(L1+2) ! packed tube address
                      GEOM_ADD(NLT,NSH(NLT))=IQ(L1+3) ! geometry address
                    ENDIF
                    L1 = L1 + 13
                  END DO
                END DO
              END IF
            END IF
            L3 = L3 + 4
          END DO
        END DO
  800   CONTINUE
      END DO
  999 CONTINUE
      IQ(LSAHS+30+DIR) = LTRG2 
      RETURN
      END

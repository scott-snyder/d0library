      SUBROUTINE SSWFIL(JDIR,JTRK,IMUOT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Fill MUOT, STTH and MHTT banks for SSW tracks
C-
C-   Inputs  : JDIR   1 = north, 2 = south
C-             JTRK   Track number in bank SATN/SATS
C-
C-   Outputs : IMUOT  Track number in bank MUOT
C-
C-   Controls: none.
C-
C-   Created   4-OCT-1994   J. de Mello Neto
C-                         based on M. Fortner's SAMUOT
C-   Updated   3-JAN-1994   Andre Sznajder ( fill MHTT bank ) 
C-   Clean up 22-FEB-1995   Andre Sznajder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER JDIR,JTRK,IMUOT
      INTEGER I,NMAX,NST,NSC
      PARAMETER (NMAX=40)
      INTEGER IFLG,NHA,NHBC,NTA,NTBC,L2F1,L2F2
      REAL PT,THT,PHI,XYZI(6),XYZO(6),CHI,CHO,DTRK,PLAN,DVTX,ECAL,EFE
      INTEGER NSH,IHIT(NMAX),IGEO(NMAX),IDIS(NMAX)
      INTEGER NWH,IQUAD,IF1,IF2,IF3,IF4
      REAL XI,YI,ZI,XO,YO,ZO,CHS,DPT,BDL
      INTEGER KADD(NMAX),KPTR(NMAX),KFLG(NMAX)
      REAL DRFT(NMAX)
      CHARACTER*4 HSHAPE
      INTEGER NSPAR,IBUF,NBUF(7)
      REAL SPAR(6),XPAR(3),ROTM(3,3)
      REAL CENTER(3),ANGLES(3),SIZE(3),HOLE(3)
C>>
      INTEGER LP,LMUOT,GZMUOT,LMUOH,GZMUOH,LMHTT,LPMUOH,LPMHTT
      INTEGER LPF,NPL,NMAXROADS,NMAXHIT,NHTMX,NMWPL,LPWAREA
      INTEGER GZSATW,LPWTRK,LPWOTRK,IHIT_MUOH
C----------------------------------------------------------------------------
C 
C *** Get track information from working banks SATN/SATS
C
      CALL GTSATR(JDIR,JTRK,IFLG,PT,THT,PHI,XYZI,XYZO,NHA,NHBC,
     &    CHI,CHO,DTRK,PLAN,DVTX,ECAL,EFE,NTA,NTBC,L2F1,L2F2,BDL,
     &    NSH,IHIT,IGEO,IDIS)
      IF (IFLG.EQ.-999) RETURN
      IF (IFLG.NE.1) RETURN
C
C *** Get pointers for SSW working banks
C
        NPL=12
        NMAXROADS = 24
        NMAXHIT   = 24
        NHTMX = 100
        NMWPL = 12
C
      LPWAREA = 1 + GZSATW() + 2*NHTMX*NPL + NPL + 33
      LPWTRK = LPWAREA + 1 + 4*NMAXROADS + 7*NMAXROADS*NMAXHIT
      LPWOTRK=LPWTRK+1+NMAXROADS*(NMWPL+1)     ! end of wamus work bank
      LP=LPWOTRK+(JTRK-1)*12*5                 ! hits on track info bank pointer
      NWH=IQ(LP+2)                             ! # hits on this track
C
C *** Calculate quantities for MUOT
C     IQUAD = 15 (N) or 16 (S)
C
      IQUAD = 14 + JDIR
C     IF1 in MUOT.ZEB : 0 = 3 mod, if vertex used in non-bend, add 10 
C     IF1 here: 6 hits in SA and SB, 3 in WC
      IF1 = 0
      IF2 = 1024
      CHS = CHI + CHO
      DPT = 0.2*ABS(PT)
C
C *** Inner and outer coordinates
C
      CALL GTSMAG (JDIR,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
      ZO = XPAR(3)
      NST = 3 * JDIR - 2
      CALL SAGSTA (NST, CENTER, ANGLES, SIZE, HOLE)
      ZI = CENTER(3)
      XI = XYZI(1) + (ZI-XYZI(3))*(XYZI(4)/XYZI(6))
      YI = XYZI(2) + (ZI-XYZI(3))*(XYZI(5)/XYZI(6))
      XO = XYZO(1) + (ZO-XYZO(3))*(XYZO(4)/XYZO(6))
      YO = XYZO(2) + (ZO-XYZO(3))*(XYZO(5)/XYZO(6))
C
C *** Book and fill bank MUOT
C
      CALL MUOTFL(IMUOT,NWH,NSH,IQUAD,IF1,IF2,IF3,IF4,XI,YI,ZI,
     &    XO,YO,ZO,XYZI(4),XYZI(5),XYZI(6),XYZO(4),XYZO(5),XYZO(6),
     &    CHS,DTRK,BDL,PT,DPT,ECAL,EFE,PLAN)
C
C *** Book and fill bank MHTT
C
      LMUOT=GZMUOT(IMUOT)                      ! MUOT pointer for this track 
      CALL BKMHTT(LMUOT,NWH*5,LMHTT)          ! book MHTT
      LMUOH=GZMUOH(0)                          ! MUOH pointer
      DO I=1,NWH
        LPF=LP+2+(I-1)*3                       ! hits on track info bank pointer
        IHIT_MUOH=IQ(LPF+1)
        LPMUOH=LMUOH+28*(IHIT_MUOH-1)
        LPMHTT=LMHTT+5*(I-1)
        IQ(LPMHTT+1)=IQ(LPMUOH+1)           ! wire address
        IQ(LPMHTT+2)=IHIT_MUOH               ! IHIT (pointer into MUOH bank)
        IQ(LPMHTT+3)=IQ(LPF+2)               ! drift time choice (-1,1)
        IQ(LPMHTT+4)=1 ! (we are allways taking TDIV1)
        IQ(LPMHTT+5)=IQ(LPF+3)               ! signed # of waves -1 
      END DO
C
C *** Book and fill bank STTH
C
      DO I=1,NSH
        NST = IBITS(IHIT(I),0,5)         ! station number
        NSC = IBITS(IHIT(I),5,5)         ! section number
        NSC = 3 - MOD(NSC+1,3)           ! plane number
        KADD(I) = (NST-1)*3 + NSC
        KPTR(I)=IHIT(I)
      ENDDO
      CALL STTHFL(IMUOT,NSH,KADD,KPTR,KFLG,DRFT)
C
  999 CONTINUE
      RETURN
      END

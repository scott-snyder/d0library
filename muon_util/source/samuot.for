      SUBROUTINE SAMUOT(JDIR,JTRK,IMUOT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Fill MUOT bank for SAMUS tracks
C-
C-   Inputs  : JDIR   1 = north, 2 = south
C-             JTRK   Track number in bank SATN/SATS
C-
C-   Outputs : IMUOT  Track number in bank MUOT
C-
C-   Controls: none.
C-
C-   Created  16-JUN-1994   M. Fortner
C-   Updated  19-SEP-1994   D. Denisov - define IFW1=10 for all tracks
C-   Updated  20-DEC-1994   I. Mandrichenko - change interface to STTHFL
C-   Updated  04-FEB-1995   I. Mandrichenko - change interface to STTHFL
C-                              (actually no changes were made)
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER JDIR,JTRK,IMUOT
      INTEGER I,J,NMAX,NST,NSC,MODULE,ITUBE
      PARAMETER (NMAX=40)
      INTEGER IFLG,NHA,NHBC,NTA,NTBC,L2F1,L2F2
      REAL PT,THT,PHI,XYZI(6),XYZO(6),CHI,CHO,DTRK,PLAN,DVTX,ECAL,EFE
      INTEGER NHIT,IHIT(NMAX),IGEO(NMAX),IDIS(NMAX)
      INTEGER NHW,IQUAD,IF1,IF2,IF3,IF4
      REAL XI,YI,ZI,XO,YO,ZO,CHS,DPT,BDL
      INTEGER IADD(NMAX)
      REAL DRFT(NMAX),GEOM(6,NMAX)
      CHARACTER*4 HSHAPE
      INTEGER NSPAR,IBUF,NBUF(7)
      REAL SPAR(6),XPAR(3),ROTM(3,3)
      REAL CENTER(3),ANGLES(3),SIZE(3),HOLE(3)
      REAL RTUBE(3,2),VTUBE(3,2),TLEN(2)
C
C   Get track information from working banks SATN/SATS
C
      CALL GTSATR(JDIR,JTRK,IFLG,PT,THT,PHI,XYZI,XYZO,NHA,NHBC,
     &    CHI,CHO,DTRK,PLAN,DVTX,ECAL,EFE,NTA,NTBC,L2F1,L2F2,BDL,
     &    NHIT,IHIT,IGEO,IDIS)
      IF (IFLG.EQ.-999) RETURN
      IF (IFLG.NE.1) RETURN
C
C   Calculate quantities for MUOT
C
      IQUAD = 12 + JDIR
      IF1=10
      IF2=0
      IF3=0
      CHS = CHI + CHO
      DPT = 0.2*ABS(PT)
C
C   Inner and outer coordinates
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
C   Book and fill bank MUOT
C
      CALL MVBITS(JTRK,0,4,IF3,12) ! reference to track in SATN/S bank
      CALL MUOTFL(IMUOT,NHW,NHIT,IQUAD,IF1,IF2,IF3,IF4,XI,YI,ZI,
     &    XO,YO,ZO,XYZI(4),XYZI(5),XYZI(6),XYZO(4),XYZO(5),XYZO(6),
     &    CHS,DTRK,BDL,PT,DPT,ECAL,EFE,PLAN)

C
C   Book and fill bank STTH
C
      DO I=1,NHIT
        NST  =  IBITS (IHIT(I), 0, 5)    ! station number
        NSC  =  IBITS (IHIT(I), 5, 5)    ! section number
        ITUBE = IBITS (IHIT(I), 16, 16)  ! tube number
        CALL    SATOPM(NST,NSC,MODULE)
        IADD(I) = 256*MODULE + ITUBE
        CALL GTSSTG(NST,NSC,-IGEO(I),RTUBE,VTUBE,TLEN)
        DO J=1,3
          GEOM(J,I) = RTUBE(J,1)
          GEOM(J+3,I) = VTUBE(J,1)
        END DO
        DRFT(I) = FLOAT(IDIS(I))*1.E-5
      END DO
      CALL STTHFL(IMUOT,NHIT,IADD,GEOM,DRFT)
C
  999 CONTINUE
      RETURN
      END

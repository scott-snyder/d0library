      SUBROUTINE MUTRK2(IQUAD)
C     =======================================================
C
C     THIS ROUTINE RECONSTRUCTS MUON TRACK. IT WILL LOOK FOR
C     3-module tracks first and then for 2-module tracks.
C     A maximum of 5 tracks/quadrant are found
C
C      6/90: does 3d tracking. does multiple loops through looking
C            for multiple tracks. uses vertex to aid in tracking
C            one could alternatively assume straight line in non-bend
C            different multiple scattering problems and not yet optimized
C      INPUT: IQUAD IS QUADRANT NUMBER IN WHICH TO DO TRACKING
C
C      OUTPUT WILL BE 3D TRACKS AND HITS ON TRACKS IN ZEBRA BANKS FILLED
C      USING CALL TO MUTHIT
C
C      4-20-86  HEDIN  (MODIFICATION OF 2-86 DZ CODE)
C      11-86 DH CONVERT TO 4-MODULE TEST SETUP
C      1/88 DH CHANGE ZBEND; SHOULD DO BETTER
C      5/88 DH BACK TO 3 MOD SETUP;LOOSE CRITERIA
C      DH 7/88 ALLOW STARIGHT LINE IN BEND VIEW; ALLOW 2 MODULE TRACKING;
C              GET ROADS FROM GTSRCP
C      DH 11/88 GET ZBEND FROM MUZBND
C     DH 10/89 speed up non-bend; only one pass in bend
C     DH 3/90 allow for fourth layer in forward direstion
C     DH 3/90 fix bug in non-bend view
C     DH 4/90 NEW I/O
C     DH 5/90 allow 2 module tracks; allow 2 tracks in same quadrant
C       DH 7/90 ADD IVER: IF NE 0 IGNORE VERTEX (LIKE COSMICS)
C     DH 8/90 add another loop with first requiring 3-module track
C     DH 6/91 hardwire raods for level 2 ease
C     SA 7/91 NEW ARGUMENT ADDED TO VERXYZ
C     DH 8/91 skip out if no A or B layers:comment out most of this until
C             it is shown it is needed
C     DH 10/91 reduce requirements in a-layer; add IQUAD to MUTRK3
C     DH 4/92 widen roads for current data
C     DH 4/92 require 2B+2C for BC segment
C     DH 6/92 add IOK to MUTHIT call
C     DH 9/92 track by octants in top/bottom central
C     DH 9/92 tighten nonbend roads now that we have calibrated
C     DH 9/92 remove 1/2 from chimx. require vertex in first pass ends
C     DH 10/92 add to MUTRK3 call bc vs A indicator; change road for A
C     DH 12/92 continued loop for A +BC, bug fix on NB,NC; no 3 loop on ends
C     DH 1/93 ALLOW LESS HITS IN CENTRAL; loops over A candidates for B/C
C     DH 4/93. change array to 40; widen end roads; return from MUTHIT bug
C     DH 2/94 remove third pass for central quads (already out for ends)
C     DH 2/94 save intermediate reults for second loop
C     DH 4/95 fix bug in saving intermediate results
C     =======================================================
      IMPLICIT NONE
      INTEGER I,ITBESTB,NTR,J,NB,QPASS,JQUAD,JBC,
     A   IQUAD,IVER,ITBEST,ILAYR(4),NLAYR,JTRK,EPASS,NTR1,ITR1(40),
     & NSPTR1(40),K,MINSOZ,NSPTR(40),NTRB,NSPTRB(40),NTRK1,
     &        NSP(13),NHIT(13),NPL,IPL,ISP,IPASS,NV,NC,IOK,LTRK
      REAL X(100,13),Y(100,13),XSP(100,13),ZPL(100,13),
     & XSPTR(13,40),ZSPTR(13,40),XSPTR1(13,40),YSPTR1(13,40),
     &     XSPTRB(13,40),YSPTRB(13,40),ZSPTRB(13,40),ZSPTR1(13,40),
     &     XGBC,Z(100,13),XV,YV,ZV,
     &     ZGBC,SLBC,XGA,ZGA,SLA,VERTEX(3),
     &     YG,SLY,ZYG,CHIMX(2),ZBEND,YSPTR(13,40),YSP(100,13),
     &     ROAD(2),YGA,SLYA,ZYGA,ROADB,ROADNB,SCALE(3)
      REAL ROADBC,ROADBE,ROADNBC,ROADNBE
      REAL ROADAC,ROADAE
      DATA ROADAC,ROADAE/25.,20./
      DATA SCALE/1.,1.,1.4/    ! SCALES ROADS FOR DIFFERENT PASSES
      DATA ROADBC,ROADNBC/8.,100./
      DATA ROADBE,ROADNBE/5.,100./
      DATA ROADB,ROADNB/6.,150./   ! ROADS IN BEND AND NONBEND
      CALL VERXYZ(IVER,VERTEX,NV)
      YV=0.
      NTRK1=0
      IF(IQUAD.LE.4) THEN
        ZV=0.
        XV=VERTEX(3)
        ROADB=ROADBC
        ROADNB=ROADNBC
      ELSE
        ZV=VERTEX(3)
        XV=0.
        ROADB=ROADBE
        ROADNB=ROADNBE
      ENDIF
      CALL MUZBND(IQUAD,ZBEND)
C
      QPASS=1
 3000 CONTINUE
      JQUAD=QPASS*IQUAD
      JTRK=0
      LTRK=-1
      IPASS=1
      EPASS=1
 2000 CONTINUE
      JBC=0
      SLBC=0.
      SLA=0.
C
C     FILL UP TEMPORARY BANK WITH HITS IN GIVEN QUADRANT
C
      IF(LTRK.NE.JTRK) THEN
        CALL MUTSET(JQUAD,SLBC,SLA,NHIT,X,Y,Z,ILAYR,NLAYR)
        LTRK=JTRK
      ENDIF
C   SKIP IF ONLY ONE LAYER HIT
      IF(NLAYR.LE.1) GO TO 1001     !*************************************
C   SKIP IF NO HITS IN A AND B LAYER
      IF(ILAYR(1).LE.1.AND.ILAYR(2).EQ.0) GO TO 1001    !*****************
      IF(IPASS.EQ.2) THEN    ! SECOND PASS ALLOW 2-MOD
CCC  REUSE INFO FROM FIRST PASS
        IF(NTR1.EQ.0) GO TO 999    ! NO BC SEGMENTS
CCCC   LOOP OVER BC SEGMENTS; STARTING WITH BEST SEGMENT
        CHIMX(1)=ROADB
        CHIMX(2)=ROADNB
        IF(IQUAD.GE.5) THEN
          CHIMX(1)=ROADB*2.
          CHIMX(2)=ROADNB*1.5
        ENDIF
CCC   DO EPASS=1
        DO I=1,NTR1
          IF(ITR1(I).EQ.2) NSPTR1(I)=-IABS(NSPTR1(I))
        ENDDO
        CALL MUTRK3(IQUAD,IVER,XV,ZV,0,NTR1,NSPTR1,XSPTR1,YSPTR1,
     A  ZSPTR1,ITBESTB,CHIMX,XGBC,ZGBC,SLBC,YG,ZYG,SLY)
CCC   DO EPASS=2
        IF(ITBESTB.EQ.0) THEN
          DO I=1,NTR1
            IF(ITR1(I).EQ.1) NSPTR1(I)=-IABS(NSPTR1(I))
            IF(ITR1(I).EQ.2) NSPTR1(I)=IABS(NSPTR1(I))
          ENDDO
        ENDIF
        CALL MUTRK3(IQUAD,IVER,XV,ZV,0,NTR1,NSPTR1,XSPTR1,YSPTR1,
     A  ZSPTR1,ITBESTB,CHIMX,XGBC,ZGBC,SLBC,YG,ZYG,SLY)
        IF(ITBESTB.EQ.0) GO TO 999      ! NO GOOD BC SEGMENTS
        ZGA=ZBEND
        XGA=XGBC+SLBC*(ZBEND-ZGBC)
        SLA=(XGA-XV)/(ZGA-ZV)
        ZYGA=ZBEND
        YGA=YG+SLY*(ZBEND-ZYG)
        SLYA=(YGA-YV)/(ZYGA-ZV)
        GO TO 998              ! TRACK FOUND
      ENDIF
      NTR1=0
C
C     FIND TRACK ELEMENT IN (B+C) LAYERS IN BEND PLANE (X,Z)
C
      IF((ILAYR(2).EQ.0.AND.ILAYR(3).EQ.0).OR.
     A   (ILAYR(2).EQ.0.AND.ILAYR(4).EQ.0).OR.
     A   (ILAYR(3).EQ.0.AND.ILAYR(4).EQ.0))   GO TO 999
C
 2999 CONTINUE
      MINSOZ=4
      NPL=0
      DO IPL=1,9
        IF(NHIT(IPL).GT.0) THEN
          NPL=NPL+1
          NSP(NPL)=NHIT(IPL)
          DO  ISP=1,NHIT(IPL)
            XSP(ISP,NPL)=X(ISP,IPL)
            YSP(ISP,NPL)=Y(ISP,IPL)
            ZPL(ISP,NPL)=Z(ISP,IPL)
          ENDDO
        ENDIF
      ENDDO
      IF(NPL.LT.MINSOZ) GO TO 999
      ROAD(1)=ROADB*SCALE(IPASS)
      ROAD(2)=ROADNB*SCALE(IPASS)
      CHIMX(1)=ROAD(1)
      CHIMX(2)=ROAD(2)
      IF(EPASS.EQ.1.AND.IVER.EQ.0) THEN    ! use vertex
C        IF(IQUAD.GE.5) MINSOZ=5
        NPL=NPL+1
        NSP(NPL)=1
        ZPL(1,NPL)=ZV
        XSP(1,NPL)=XV
        YSP(1,NPL)=YV
C        ROAD(1)=15.                  ! hardwire this road
C        ROAD(2)=ROADNB*SCALE(IPASS)
        IF(IQUAD.GE.5) THEN
          ROAD(1)=ROAD(1)*2.
          ROAD(2)=ROAD(2)*1.5
        ENDIF
        CHIMX(1)=ROAD(1)
        CHIMX(2)=ROAD(2)
        CALL PTRAK1(4,NPL,MINSOZ,NSP,XSP,YSP,ZPL,ROAD,NTRB,
     A    NSPTRB,XSPTRB,YSPTRB,ZSPTRB)
        IF(NTRB.EQ.0) THEN
          EPASS=2
          GO TO 2999                   ! TRY AGAIN, NO VERTEX FORCE
        ENDIF
      ELSE
        CALL PTRAK1(0,NPL,MINSOZ,NSP,XSP,YSP,ZPL,ROAD,NTRB,
     A    NSPTRB,XSPTRB,YSPTRB,ZSPTRB)
      ENDIF
      IF(NTRB.EQ.0) GO TO 999     ! NO BC LAYER TRACK
CCC   COUNT NUMBER OF B- AND C-LAYER HITS---SOMEWHAT CLUMSY......
      DO I=1,NTRB
        NB=0
        NC=0
        DO J=1,NSPTRB(I)
CCC     TURN OFF VERTEX POINT VEFORE CALL TO MUTRK3
          IF(ABS(ZSPTRB(J,I)).LT.200.) ZSPTRB(J,I)=999999.
          DO K=1,9
            DO ISP=1,NHIT(K)
              IF(XSPTRB(J,I).EQ.X(ISP,K)) THEN
                IF(K.GE.7.AND.K.LE.9) NB=NB+1
                IF(K.GE.1.AND.K.LE.6) NC=NC+1
                GO TO 111
              ENDIF
            ENDDO
          ENDDO
 111      CONTINUE
        ENDDO
CC  REQUIRE 2 HITS EACH MODULE IN ENDS
        IF(IQUAD.GE.5) THEN
           IF(NB.LT.2.OR.NC.LT.2) NSPTRB(I)=-1
        ENDIF
      ENDDO
      DO I=1,NTRB
        IF(NSPTRB(I).GT.1.AND.NTR1.LT.40) THEN
          NTR1=NTR1+1
          NSPTR1(NTR1)=NSPTRB(I)
          ITR1(NTR1)=EPASS 
          DO J=1,NSPTRB(I)
            XSPTR1(J,NTR1)=XSPTRB(J,I)
            YSPTR1(J,NTR1)=YSPTRB(J,I)
            ZSPTR1(J,NTR1)=ZSPTRB(J,I)
          ENDDO
        ENDIF
      ENDDO
CCCC   LOOP OVER BC SEGMENTS; STARTING WITH BEST SEGMENT
  996 CONTINUE
      CALL MUTRK3(IQUAD,IVER,XV,ZV,0,NTRB,NSPTRB,XSPTRB,YSPTRB,ZSPTRB,
     A  ITBESTB,CHIMX,XGBC,ZGBC,SLBC,YG,ZYG,SLY)
      IF(ITBESTB.EQ.0.AND.EPASS.EQ.1.AND.IVER.EQ.0) THEN
        EPASS=2
        GO TO 2999                   ! TRY AGAIN, NO VERTEX FORCE
      ENDIF
      IF(ITBESTB.EQ.0) GO TO 999     ! NO GOOD BC LAYER TRACK LEFT
C
C     FIND TRACK ELEMENT IN A LAYERS IN BEND PLANE (X,Z)
C     INTRODUCE PROJECTION TO BEND PLANE AS PSUEDOPOINT
C     ALSO USE VERTEX AS PSEUDOPOINT----NOTE THAT THESE ARE NOT REQUIRED
C     TO BE ON THE TRACK; MAY WANT TO CHANGE
      ITBEST=0
      IF(ILAYR(1).GT.1) THEN
        NPL=1
        MINSOZ=3
        DO IPL=10,13
          IF(NHIT(IPL).GT.0) THEN
            NPL=NPL+1
            NSP(NPL)=NHIT(IPL)
            DO ISP=1,NHIT(IPL)
              XSP(ISP,NPL)=X(ISP,IPL)
              YSP(ISP,NPL)=Y(ISP,IPL)
              ZPL(ISP,NPL)=Z(ISP,IPL)
            ENDDO
          ENDIF
        ENDDO
CCCC  PSUEDOPOINT IN CENTER OF MAGNET
        NSP(1)=1
        ZPL(1,1)=ZBEND
        XSP(1,1)=XGBC+SLBC*(ZBEND-ZGBC)
        YSP(1,1)=YG+SLY*(ZBEND-ZYG)
        ROAD(1)=ROADB*1.7*SCALE(IPASS) ! SLIGHTLY LARGER as use vertex
CCCC  PSUEDOPOINT AT VERTEX FOR BEAM
        IF(IVER.EQ.0) THEN
          MINSOZ=4
          NPL=NPL+1
          NSP(NPL)=1
          ZPL(1,NPL)=ZV
          XSP(1,NPL)=XV
          YSP(1,NPL)=YV
          IF(IQUAD.LE.4) ROAD(1)=ROADAC
          IF(IQUAD.GE.5) ROAD(1)=ROADAE
        ENDIF
        ROAD(2)=ROADNB*SCALE(IPASS)
        IF(IQUAD.GE.5) ROAD(2)=ROAD(2)*1.5
        CHIMX(1)=ROAD(1)
        CHIMX(2)=ROAD(2)
        IF(IVER.EQ.0) THEN
          CALL PTRAK1(3,NPL,MINSOZ,NSP,XSP,YSP,ZPL,ROAD,NTR,NSPTR,XSPTR,
     A    YSPTR,ZSPTR)
        ELSE
          CALL PTRAK1(1,NPL,MINSOZ,NSP,XSP,YSP,ZPL,ROAD,NTR,NSPTR,XSPTR,
     A    YSPTR,ZSPTR)
        ENDIF
        IF(NTR.GT.0) THEN
          CALL MUTRK3(IQUAD,IVER,XV,ZV,1,NTR,NSPTR,XSPTR,YSPTR,
     A      ZSPTR,ITBEST,CHIMX,XGA,ZGA,SLA,YGA,ZYGA,SLYA)
        ENDIF
      ENDIF
        IF(ITBEST.EQ.0) THEN ! NO A-B-C
          NSPTRB(ITBESTB)=0
          GO TO 996          ! REPEAT FOR NEXT BEST SEGMENT
        ELSE
          GO TO 998       ! ABC TRACK FOUND
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC        NO BC LAYER TRACK---look for ac or ab layer tracks
  999 CONTINUE
      JBC=1
      IF(IPASS.EQ.1) GO TO 1000
      IF(ILAYR(1).LE.1) GO TO 1000      ! NO A LAYER HIT EITHER***************
C     FIND TRACK ELEMENT IN A LAYERS IN BEND PLANE (X,Z)
C     USE VERTEX  AS PSUEDOPOINT
      NPL=0
      MINSOZ=3
      DO  IPL=10,13
        IF(NHIT(IPL).GT.0) THEN
          NPL=NPL+1
          NSP(NPL)=NHIT(IPL)
          DO  ISP=1,NHIT(IPL)
            XSP(ISP,NPL)=X(ISP,IPL)
            YSP(ISP,NPL)=Y(ISP,IPL)
            ZPL(ISP,NPL)=Z(ISP,IPL)
          ENDDO
        ENDIF
      ENDDO
CCCC  PSUEDOPOINT AT VERTEX
      IF(IVER.EQ.0) THEN
        NPL=NPL+1
        NSP(NPL)=1
        ZPL(1,NPL)=ZV
        XSP(1,NPL)=XV
        YSP(1,NPL)=YV
      ENDIF
      IF(NPL.LT.MINSOZ) GO TO 1000
      ROAD(1)=ROADB*1.5*SCALE(IPASS)                ! SLIGHTLY LARGER
      ROAD(2)=ROADNB*SCALE(IPASS)
      CHIMX(1)=ROAD(1)
      CHIMX(2)=ROAD(2)
      IF(IVER.EQ.0) THEN
        CALL PTRAK1(2,NPL,MINSOZ,NSP,XSP,YSP,ZPL,ROAD,NTR,NSPTR,XSPTR,
     A    YSPTR,ZSPTR)
      ELSE
        CALL PTRAK1(0,NPL,MINSOZ,NSP,XSP,YSP,ZPL,ROAD,NTR,NSPTR,XSPTR,
     A    YSPTR,ZSPTR)
      ENDIF
      IF(NTR.EQ.0) GO TO 1000      ! NO A LAYER   ***************************
1998  CONTINUE
      CALL MUTRK3(IQUAD,IVER,XV,ZV,1,NTR,NSPTR,XSPTR,YSPTR,ZSPTR,
     A  ITBEST,CHIMX,XGA,ZGA,SLA,YGA,ZYGA,SLYA)
      IF(ITBEST.EQ.0) GO TO 1000     ! NO A LAYER TRACK
CCCCCCCCCCCCCCC   NOW LOOK FOR B OR C LAYER TRACK WITH MAGNET PSEUDOPOINT
      NPL=0
      MINSOZ=3
      DO IPL=1,9
        IF(NHIT(IPL).GT.0) THEN
          NPL=NPL+1
          NSP(NPL)=NHIT(IPL)
          DO ISP=1,NHIT(IPL)
            XSP(ISP,NPL)=X(ISP,IPL)
            YSP(ISP,NPL)=Y(ISP,IPL)
            ZPL(ISP,NPL)=Z(ISP,IPL)
          ENDDO
        ENDIF
      ENDDO
CCCC  PSUEDOPOINT IN CENTER OF MAGNET
      NPL=NPL+1
      IF(NPL.LT.MINSOZ) GO TO 1000
      NSP(NPL)=1
      ZPL(1,NPL)=ZBEND
      XSP(1,NPL)=XGA+SLA*(ZBEND-ZGA)
      YSP(1,NPL)=YGA+SLYA*(ZBEND-ZYGA)
      ROAD(1)=ROADB*2.*SCALE(IPASS)              ! SLIGHTLY LARGER
      ROAD(2)=ROADNB*2.0*SCALE(IPASS)   ! AS POOR EXTRAPOLATION
      CHIMX(1)=ROAD(1)
      CHIMX(2)=ROAD(2)
      CALL PTRAK1(2,NPL,MINSOZ,NSP,XSP,YSP,ZPL,ROAD,NTRB,NSPTRB,XSPTRB,
     A  YSPTRB,ZSPTRB)
      IF(NTRB.EQ.0) THEN
        NSPTR(ITBEST)=0
        GO TO 1998
      ENDIF
C      IF(NTRB.EQ.0) GO TO 1000     ! NO BC LAYER TRACK  *********************
      CALL MUTRK3(IQUAD,IVER,XV,ZV,0,NTRB,NSPTRB,XSPTRB,YSPTRB,ZSPTRB,
     A  ITBESTB,CHIMX,XGBC,ZGBC,SLBC,YG,ZYG,SLY)
      IF(ITBESTB.EQ.0) THEN
        NSPTR(ITBEST)=0
        GO TO 1998
      ENDIF
C      IF(ITBESTB.EQ.0) GO TO 1000     ! NO BC LAYER TRACK *******************
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  998 CONTINUE
CC
CC    CALL ROUTINE TO PLACE HITS ON TRACK AND FILL OUTPUT ZEBRA BANK
CC
      NTRK1=NTRK1+1
      JTRK=JTRK+1
      XGBC=XGBC+SLBC*(ZBEND-ZGBC)
      XGA=XGA+SLA*(ZBEND-ZGA)
      YG=YG+SLY*(ZBEND-ZYG)
      YGA=YGA+SLYA*(ZBEND-ZYGA)
      ROAD(1)=ROADBC      ! USE CENTRAL ROAD FOR BOTH END AND CENTRAL
      ROAD(2)=ROADNB
      IF(JQUAD.GE.5) ROAD(2)=1.5*ROADNB
      CALL MUTHIT(JQUAD,SLBC,SLA,SLY,SLYA,ZBEND,XGBC,XGA,YG,YGA,
     A  ROAD,IOK)
CC
      IF(IOK.EQ.0) THEN    ! NO TRACK FILLED IN MUTHIT
        JTRK=JTRK-1
        IF(JBC.EQ.0) GO TO 999 ! TRY STARTING AT A-LAYER
        GO TO 1000      ! DO NEXT PASS
      ENDIF
      IF(JTRK.EQ.5) THEN
        GO TO 1001
      ELSE
        IPASS=1
        EPASS=1
        GO TO 2000   ! REPEAT UNTIL ALL TRACKS ARE FOUND
      ENDIF          ! ALLOW 5 TRACKS/QUAD--PRESUME GARBAGE IF MORE
 1000 CONTINUE
      IF(IPASS.EQ.1) THEN
        IPASS=2
        EPASS=1
        GO TO 2000                ! TRY AGAIN allow 2-modules
      ENDIF
 1001 CONTINUE
CCCC  FOR TOP/BOTTOM CENTRAL, REPEAT FOR OTHER OCTANT
      IF(IQUAD.EQ.2.OR.IQUAD.EQ.4) THEN
        IF(QPASS.EQ.1) THEN
          QPASS=-1
          GO TO 3000
        ENDIF
      ENDIF
      RETURN
      END

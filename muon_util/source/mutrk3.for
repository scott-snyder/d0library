      SUBROUTINE MUTRK3(IQUAD,IVER,VB,VZ,IAB,NTR,NSPTR,XSPTR,
     A YSPTR,ZSPTR,ITBEST,CHIMX,XG,ZXG,SLX,YG,ZYG,SLY)
C     =======================================================
C
C     THIS ROUTINE FINGS THE BEST OF A SERIES OF TRACKS. UTILITY CALLED
C     BY MUTRK2. NOTE THAT DEFINIG THE 'BEST' TRACK ISN'T OBVIOUS
C     INPUT: IQUAD = QUAD NO.      IVER  = 0 THEN USE VERTEX
C            VB,VZ    VERTEX LOCATION BEND AND Z (NB ALWAYS 0) 
C        IAB=0 BC LAYER, =1 A-LAYER         NTR     = NUMBER OF TRACKS
C        NSPTR,XSPTR,YSPTR,ZSPTR = NUMBER OF POINTS AND THE X Y Z OF EACH POINT
C     OUTPUT: ITBEST = TRACK NUMBER OF BEST TRACK
C             CHIMX = MAXIMUM ALLOWABLE CHI     
C          XG,ZXG,SLX = TRACK PARAMETERS OF BEST TRACK  - X VIEW
C          YG,ZYG,SLY = TRACK PARAMETERS OF BEST TRACK  - Y VIEW
C     DH 5/90
C     DH 8/90 DIFFERENT IF COSMIC, ALSO TIGHTEN VERTEX IF BEAM
C     DH 11/91 ADD QUADRANT
C     DH 10/92 different for a-layer
C     DH 11/92 larger array sizes; larger nonbend central
C     DH 12/92 add bend view vertex in 'best' selection
C     DH 4/93 increase array size to 40; looser in ends
C     DH 3/95 loosen vertex nonbend in central as poor deltaT
C     =======================================================
      IMPLICIT NONE
      INTEGER ITR,IVER,IAB,NTR,MAXHIT,IQUAD,NSPTR(40),INEW,ITBEST
      REAL XSPTR(13,40),ZSPTR(13,40),CHIX,XG,ZXG,SLX,VXG,VSL,WPL(13),
     &     CHIA,XGA,ZGA,SLA,CHIMX(2),YG,ZYG,SLY,CHIY,YSPTR(13,40),
     &     YGA,ZYGA,SLYA,CHIYA,XV,YV,DELV,XVA,YVA,DELVC,DELVE,
     &     DELB,DELBC,DELBE,DELBCA,DELBEA,VB,VZ
      DATA DELVC,DELVE/500.,550./    ! nonbend
      DATA DELBC,DELBE/400.,500./    ! bend
      DATA DELBCA,DELBEA/130.,200./    ! bend a-layer
      DATA WPL/13*1./
C
      DELV=DELVE
      IF(IQUAD.LE.4) DELV=DELVC      ! CENTRAL REGION
      IF(IAB.EQ.0) THEN     ! BC LAYER
        DELB=DELBE
        IF(IQUAD.LE.4) DELB=DELBC      ! CENTRAL REGION
      ELSE
        DELB=DELBEA
        IF(IQUAD.LE.4) DELB=DELBCA      ! CENTRAL REGION
      ENDIF
      ITBEST=0
      MAXHIT=0
      DO ITR=1,NTR
        IF(NSPTR(ITR).GT.0) THEN
          CALL LINFIT(NSPTR(ITR),XSPTR(1,ITR),ZSPTR(1,ITR),
     &            WPL(1),XGA,ZGA,SLA,VXG,VSL,CHIA)
          CALL LINFIT(NSPTR(ITR),YSPTR(1,ITR),ZSPTR(1,ITR),
     &            WPL(1),YGA,ZYGA,SLYA,VXG,VSL,CHIYA)
          IF(IVER.NE.0) THEN
            XVA=0.        ! NO REQUIREMENT
            YVA=0.
          ELSE
            XVA=ABS(XGA-SLA*(ZGA-VZ)-VB)
            YVA=ABS(YGA-SLYA*(ZYGA-VZ))
          ENDIF
CCC   REQUIRE COARSE POINTING TO VERTEX; TIGHTER IN NON-BEND
          IF(YVA.LT.DELV.AND.XVA.LT.DELB) THEN
            IF(CHIA.LT.CHIMX(1).AND.CHIYA.LT.CHIMX(2)) THEN
              IF(MAXHIT.EQ.0) THEN        ! FIRST TIME THROUGH
                MAXHIT=NSPTR(ITR)
                CHIX=CHIA
                XG=XGA
                ZXG=ZGA
                SLX=SLA
                YG=YGA
                ZYG=ZYGA
                SLY=SLYA
                CHIY=CHIYA
                XV=XVA
                YV=YVA
                ITBEST=ITR
C try to decide which is better
              ELSE
                INEW=0
                IF(NSPTR(ITR).GT.MAXHIT+1) THEN
                  INEW=1
                ELSE IF(NSPTR(ITR).GE.MAXHIT-1) THEN
                  IF(XV-XVA.GT.100.) THEN   ! PICK STIFFEST
                    INEW=1
                  ELSE IF(XV-XVA.GT.20.AND.NSPTR(ITR).GT.MAXHIT) THEN   
                    INEW=1
                  ELSE
                    IF((CHIA+.2)/(CHIX+.001).LT..5) THEN
                      INEW=1
                    ELSE IF(CHIA/(CHIX+.001).LT.1.4) THEN
                      IF(YV-YVA.GT.100.) THEN
                        INEW=1
                      ELSE IF(NSPTR(ITR).GT.MAXHIT) THEN
                        INEW=1
                      ELSE IF(NSPTR(ITR).EQ.MAXHIT.AND.
     A                                            CHIA.LT.CHIX) THEN
                        INEW=1
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
                IF(INEW.EQ.1)THEN
                  MAXHIT=NSPTR(ITR)
                  XG=XGA
                  ZXG=ZGA
                  SLX=SLA
                  CHIX=CHIA
                  YG=YGA
                  ZYG=ZYGA
                  SLY=SLYA
                  CHIY=CHIYA
                  XV=XVA
                  YV=YVA
                  ITBEST=ITR
                ENDIF
              ENDIF
            ENDIF
          END IF
        ENDIF
      ENDDO
      RETURN
      END

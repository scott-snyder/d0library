      SUBROUTINE MULOCAL(IMUOT,IABC,NSEG,NH,XYZ,DXYZ,MMOD,CXYZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns points on local muon segments
C-           plus errors. allows for multiple segments
C-
C-   Inputs  : IMUOT      = MUOT track       IABC =0 A, =1 BC
C-   Outputs : NSEG, NH = number of segments, no. hits/segment
C-             XYZ,DXYZ   = x,y,z locations and errors of points on segment
C-             MMOD       = module numbers of points on segment
C-             CXYZ       = correlated errors (geometry) for points on segment
C-             60.96 cm   = length of repeat on pad
C
C-   Created  22-March-1993  D.Hedin. VERSION 1, ONLY 1 SEGMENT
C     6/94 DH return with NSEG=0 if no MUOH bank
C     6/94 DO A-LAYER STUBS WITH MULTIPLE SEGMENTS
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INTEGER   ORIEN,NSM,NHM,GZMUOH,NHP
      INTEGER RUN,ID
      PARAMETER (NSM=10)       ! MAXIMUM NUMBER OF SEGMENTS
      PARAMETER (NHM=20)       ! MAXIMUM NUMBER OF HITS/SEGMENT
      PARAMETER (NHP=10)       ! MAXIMUM NUMBER OF HITS/PLANE
      INTEGER   NP(4),NSDR,NSPD,JHDR(NSM,4),ISOLDR(NSM,4),ISDT(NSM,4)
      INTEGER   ISPD(NSM,4),ITEMP(4),JMOD(4,NHP),MUOR,MUOROLD
      INTEGER   NSEG,NH(NSM),MMOD(NSM,NHM),IMUOT,IABC,JMUOH(4,NHP)
      INTEGER   LMUOH
      INTEGER   NSAM,IFW3,ISPARE,IWADD
      INTEGER   IMOD,IPLN,IWIR,IERR,IHIT,I,J
      INTEGER   JSPD,JSDR
      INTEGER   KK,NPTRAK,QUAD,IFW1,IFW2
      INTEGER   IHMUOH,ITSIGN,IDELT,IPAD,IFWH1,IFWH2,INRAW,IORIEN,NHWIR
      REAL      XHTRAK(4,NHP),YHTRAK(4,NHP),ZHTRAK(4,NHP),WLEN(NHP),
     &          DR(2,4,NHP),DT(2,4,NHP),PD(2,4,NHP),QD(4,NSM),QP(4,NSM)
      REAL      XYZ(NSM,NHM,3),DXYZ(NSM,NHM,3),CXYZ(NSM,NHM,3),TDXYZ(3)
      REAL      XYZGER(3),TXYZ(3)
      REAL      CORT1,CORT2,CORP1,CORP2,CORDT1,ELCAL,ELFE,SPR1,SPR2,
     &          QMIN,QMINP,DMIN,CORDT2,DDIS1,DDIS2,TDIV1,TDIV2,
     &          VERD1,VERD2,XCWIR,YCWIR,ZCWIR
      REAL      XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     &          YCOSOM,ZCOSOM,CHSQBV,CHSNBV,RMOM,RMOMER
C----------------------------------------------------------------------
      NSEG=0
      DO I=1,NSM
        NH(I)=0
      ENDDO
      IF(IMUOT.LE.0) RETURN
      IF(GZMUOH(0).EQ.0) RETURN
      CALL GTMUOT(IMUOT,NPTRAK,NSAM,QUAD,IFW1,IFW2,IFW3,ISPARE,
     &         XI,YI,ZI,XMAGC,YMAGC,
     &         ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,YCOSOM,ZCOSOM,
     &         CHSQBV,CHSNBV,RMOM,RMOMER,ELCAL,ELFE,SPR1,SPR2)
      IF (IFW1.NE.5) THEN      ! normal 2/3 layer segments
        DO IHIT=1,NPTRAK              !loop over hits
          CALL GTMHTT(IMUOT,IHIT,IWADD,IHMUOH,ITSIGN,IDELT,IPAD)
          CALL MUADD(IWADD,IMOD,IPLN,IWIR,IERR)
          IF((IABC.EQ.0.AND.IMOD.LE.99).OR.(IABC.EQ.1.AND.IMOD.GE.
     &             100)) THEN

C--  TEMP FOR SINGLE SEGMENT
            IF(ITSIGN.NE.0) THEN
              CALL MULOC1(IHMUOH,ITSIGN,IDELT,IPAD,TXYZ,TDXYZ)
              NH(1)=NH(1)+1
              MMOD(1,NH(1))=IMOD
              CALL MUGMER(IHMUOH,XYZGER)   ! geometry errors
              CXYZ(1,NH(1),1)=XYZGER(1)
              CXYZ(1,NH(1),2)=XYZGER(2)
              CXYZ(1,NH(1),3)=XYZGER(3)
              DO I=1,3
                XYZ(1,NH(1),I)=TXYZ(I)
                DXYZ(1,NH(1),I)=TDXYZ(I)
              ENDDO
            ENDIF
          ENDIF
        ENDDO
        IF(NH(1).GE.2) NSEG=1
      ELSE                         ! A-LAYER STUBS
        IF(IABC.EQ.1) GO TO 999
        JSDR=-1
        JSPD=-1
        DO I=1,4
          NP(I)=0
        ENDDO
        MUOROLD = 0
C  LOOP OVER HITS AND FILL TEMP ARRAY:ASSUMES ONLY A-LAYER HITS
        DO IHIT=1,NPTRAK              !loop over hits
          CALL GTMHTT(IMUOT,IHIT,IWADD,IHMUOH,ITSIGN,IDELT,IPAD)
          CALL MUADD(IWADD,IMOD,IPLN,IWIR,IERR)
          IF (IMOD .GE. 100) GOTO 111
          CALL GTMUOH(IHMUOH,IWADD,IFWH1,IFWH2,INRAW,IORIEN,NHWIR,
     &              CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1,
     &              DDIS2,TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR)
C
          ORIEN=IABS(IORIEN)                ! module orientation
          MUOR=ORIEN
          NP(IPLN+1)=NP(IPLN+1)+1           ! # hits/plane
          JMUOH(IPLN+1,NP(IPLN+1))=IHMUOH   ! pointer to MUOH bank
          JMOD( IPLN+1,NP(IPLN+1))=IMOD     ! # modules hit
C
          DR( 1,IPLN+1,NP(IPLN+1))=DDIS1    ! drift distance 1C
          DT( 1,IPLN+1,NP(IPLN+1))=TDIV1    ! time division 1
          DR( 2,IPLN+1,NP(IPLN+1))=9999.
          DT( 2,IPLN+1,NP(IPLN+1))=9999.
C
          IF(NHWIR.EQ.2) THEN               ! # times on wire
            DR(2,IPLN+1,NP(IPLN+1))=DDIS2   ! drift distance 2
            DT(2,IPLN+1,NP(IPLN+1))=TDIV2   ! time division 2
          ENDIF
C
          PD(1,IPLN+1,NP(IPLN+1))=VERD1     ! vernier distance 1
          PD(2,IPLN+1,NP(IPLN+1))=VERD2     ! vernier distance 2
C
          IF (MUOR.EQ.1) THEN
            XHTRAK(IPLN+1,NP(IPLN+1))=YCWIR      ! DT VIEW ALONG WIRE
            YHTRAK(IPLN+1,NP(IPLN+1))=XCWIR      ! INTERPLANE
            ZHTRAK(IPLN+1,NP(IPLN+1))=ZCWIR      ! DRIFT DIRECTION
          ELSE IF (MUOR.EQ.2) THEN
            XHTRAK(IPLN+1,NP(IPLN+1))=XCWIR      ! DT VIEW ALONG WIRE
            YHTRAK(IPLN+1,NP(IPLN+1))=YCWIR      ! INTERPLANE
            ZHTRAK(IPLN+1,NP(IPLN+1))=ZCWIR      ! DRIFT DIRECTION
          ELSE IF (MUOR.EQ.3) THEN
            XHTRAK(IPLN+1,NP(IPLN+1))=YCWIR      ! DT VIEW ALONG WIRE
            YHTRAK(IPLN+1,NP(IPLN+1))=ZCWIR      ! INTERPLANE
            ZHTRAK(IPLN+1,NP(IPLN+1))=XCWIR      ! DRIFT DIRECTION
          ELSE IF (MUOR.EQ.4) THEN
            XHTRAK(IPLN+1,NP(IPLN+1))=XCWIR      ! DT VIEW ALONG WIRE
            YHTRAK(IPLN+1,NP(IPLN+1))=ZCWIR      ! INTERPLANE
            ZHTRAK(IPLN+1,NP(IPLN+1))=YCWIR      ! DRIFT DIRECTION
          ENDIF
C
          LMUOH = GZMUOH(IHIT)
          WLEN(IHIT) = Q(LMUOH + 24)/2. + XHTRAK(IPLN+1,NP(IPLN+1))
C
  111     CONTINUE
        ENDDO
C
        DO I=1,4
          IF ( (NP(I) .EQ. 1) .AND. (DR(1,I,NP(I)) .EQ. 99999.) )
     &      NP(I) = 0
        END DO
C
C   LOOK FOR BEND VIEW SOLUTIONS
        CALL MULOC2(NP,ZHTRAK,YHTRAK,DR,NSDR,JHDR,ISOLDR,QD)
C
C   SAVE SOLUTIONS; IF TOO MANY SAVE BEST; WITH DRIFT TAKING PRECEDENCE
C
        IF(NSDR.GT.0) THEN
C          DO II=1,2      ! LOOP OVER BEND SOLUTIONS
            QMIN=9999.
            DMIN=9999.
            DO J=1,NSDR    ! FIND BEST
              IF (QD(1,J) .LT. QMIN) THEN
                QMIN=QD(1,J)
                DMIN=QD(2,J)
                JSDR=J
              ENDIF
            ENDDO
C
C   JSDR=BEND SOLUTION. FOR NONBEND, USE SAME POINTS
            DO KK=1,4
              ITEMP(KK)=JHDR(JSDR,KK)
            ENDDO
C
C   LOOK FOR NONBEND VIEW SOLUTIONS
            CALL MULOC3(ITEMP,XHTRAK,YHTRAK,DT,PD,WLEN,NSPD,ISDT,ISPD,
     &          QP)
            QMINP=99999.
            IF(NSPD.GT.0) THEN
C              DO I=1,2            ! LOOP OVER NONBEND SOLUTIONS
                DO J=1,NSPD    ! FIND BEST
                  IF(QP(1,J).LT.QMINP) THEN
                    QMINP=QP(1,J)
                    JSPD=J
                  ENDIF
                ENDDO
C              ENDDO        ! NONBEND SOLUTIONS LOOP
C
            END IF
C          ENDDO            ! BEND SOLUTIONS LOOP
C
            CALL EVNTID(RUN,ID)
C            WRITE (0,101) RUN,ID,NSDR,NSPD,QMIN,QMINP
C 101        FORMAT(' RUN ID QMIN QMINP',2I10,2I3,2F12.2)
        ENDIF
CC  CONVERT TO OUTPUT QUANTITIES
        NH(NSEG) = 0
        IF (JSDR.LE.0.OR.JSPD.LE.0) THEN
          GO TO 999
        END IF
        NSEG=1
C
C  best solution
C
        DO IHIT=1,4              !loop over 4 planes
          ITSIGN=ISOLDR(JSDR,IHIT)
          IF(ITSIGN.NE.0) THEN
            IHMUOH=JMUOH(IHIT,JHDR(JSDR,IHIT))
            IF(ISDT(JSPD,IHIT).GT.0) IDELT=1
            IF(ISDT(JSPD,IHIT).LT.0) IDELT=2
            IPAD=ISPD(JSPD,IHIT)
            CALL MULOC1(IHMUOH,ITSIGN,IDELT,IPAD,TXYZ,TDXYZ)
            NH(NSEG)=NH(NSEG)+1
            MMOD(1,NH(NSEG))=JMOD(IHIT,JHDR(JSDR,IHIT))
            CALL MUGMER(IHMUOH,XYZGER)   ! geometry errors
            CXYZ(1,NH(NSEG),1)=XYZGER(1)
            CXYZ(1,NH(NSEG),2)=XYZGER(2)
            CXYZ(1,NH(NSEG),3)=XYZGER(3)
            DO KK=1,3
              XYZ(1,NH(NSEG),KK)=TXYZ(KK)
              DXYZ(1,NH(NSEG),KK)=TDXYZ(KK)
            ENDDO
          ENDIF
        ENDDO      ! PLANE LOOP
        IF (NH(1).LT.4) THEN
          NSEG=0
        END IF
      ENDIF
  999 RETURN
      END

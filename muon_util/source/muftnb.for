      SUBROUTINE MUFTNB(QUAD,IFW1,IVER,VY,VX,NN,YT,XT,XV,VER,ZBND,
     &  WLEN,XM,XA,ZM,SLI,SLO,C1,CT,CV,IS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fit the nonbend view (allow for multiple
C         scattering bend in magnet)
C         including both time division info and vernier pads
C-             QUAD,IFW1: quadrant, flag if 3 layer
C-   Inputs  : IVER = 0 then beam-->use vertex point
C              VY,VX = vertex location (VX always 0)
C              NN = number of points on track: maximum of 20
C              YT = location between planes for nth hit
C              XT = location along wire from deltaT
C              XV = vernier - 2 solutions modulo wavelength
C              VER = vernier wavelength
C              ZBND = bend point
C-   Outputs : XM,XA,ZM,SLI,SLO = x(inside+outside),z,slope (inside+outside)
C             C1 is sqrt((xp-x)**2/n) using both delta t and pads
C             CT,CV  are  for time and vernier separately
C             IS = vernier solution for nth hit (which wavelength)
C-
C-   Created   8-AUG-1988   David Hedin
C-          5/89 DH optimize CPU efficiency
CC     this initial library routine is not yet optimized. Exactly
C      how to do 'final' fit with A layer somehow constrained hasn't
C      been decided. Also, there will probably be the need for removing
C      of bad points. I want to wait until there is some decent data
C      (either MC or D0 chambers but calibrated) before 'tuning'
C  DH 8/90 minor change in array sizes
C       SEW 10/90 nfit back to 10; omit bend possibility
C  DH 4/91 raise NMAX from 20 to 40
C  DH 2/92. find ambiguities with straight line fit. but then refit
C           allowing bend in magnet. use vertex point. a spline fit
C  using only pad points was tested but is commented out. Instead, the
C  inside direction is simply the vertex plus the magnet center
C  DH 6/92 do not use vertex in initial fit
C    DH 9/92 change SIGT to 10; allow reweighting of deltaT points
C    fix edge of chamber, use VTX tohelp determine pad
C  DH 10/92 replace inadverdent delete of second deltaT fit
C  DH 11/92 if level 2, only BC pads for 3 layer tracks
C  DH 4/93 DO ENDS DIFFERENT THAN CENTRAL
C  DH 2/95 remove different code in L2
C  DH 7/95 skip out if only 1 pad
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMAX,N,I,NN,N1,NV,NT,II,J,NFIT,IVER,QUAD
      PARAMETER (NMAX=40)
      PARAMETER (NFIT=20)
      REAL YT(NMAX),XT(NMAX),XV(2,NMAX),VER,CT,CV,X1,X3,Z1,Z3,
     A  SL1,SL3,C1,C3,SIGT,WT(NFIT),SWT,SXT,SZT,SX2T,SZ2T,SXZT,
     A  Y(NFIT),X(NFIT),XP,VXG,VSL,XXV(2,NFIT),XM,ZM,SLI,SLO,ZBND
      INTEGER IV(NFIT),JV(NFIT),ISOL(NFIT),IS(NMAX),K1(NFIT),K2(NFIT)
      INTEGER LA,LB,IFW1
      REAL DELA,DELB,VX,VY,XA,WLEN(NMAX),SIGP,WTF(50),XF(50),YF(50)
      DATA SIGT/10./       ! RELATIVE WEIGHT FOR DELTA T; NEEDS TUNING
C
C    INIT
      N=NN
      DO I=1,NMAX
        IS(I)=0
      ENDDO
      IF(N.GT.NFIT) N=NFIT
C   DO B-C LAYER
C   FIT TO STRAIGHT LINE USING TIME DIVISION ONLY TO GET GOING
C
      J=0
      IF(IVER.EQ.0) THEN    ! USE VERTEX TO AID IN PAD SOLUTION
        J=J+1
        WT(J)=1./30.**2    ! LESS WEIGHT THAN TIME DIVISION
        IF(QUAD.GE.5) WT(J)=1./5.**2  ! MORE WEIGHT THAN TIME DIVISION
        X(J)=VX
        Y(J)=VY
      ENDIF
      DO I=1,N
        IF(ABS(XT(I)).LT.8099.) THEN
          J=J+1
          WT(J)=1./SIGT**2         ! CAREFUL TO BE CONSISTENT
          X(J)=XT(I)
          Y(J)=YT(I)
        ENDIF
      ENDDO
      IF(J.LT.2) GO TO 999
      CALL MUFSUM(J,X,Y,WT,SWT,SZT,SZ2T,SXT,SX2T,SXZT)
      CALL MUSLIN(SWT,SXT,SZT,SX2T,SZ2T,SXZT,X3,Z3,SL3,VXG,VSL,C3)
      IF(C3.LT.-1000.) GO TO 999
C   REFIT WEIGHTING POOR DT POINTS
      J=0
      IF(IVER.EQ.0) THEN    ! USE VERTEX TO AID IN PAD SOLUTION
        J=J+1
        WT(J)=1./30.**2    ! LESS WEIGHT THAN TIME DIVISION
        IF(QUAD.GE.5) WT(J)=1./5.**2  ! MORE WEIGHT THAN TIME DIVISION
        X(J)=VX
        Y(J)=VY
      ENDIF
      DO I=1,N
        IF(ABS(XT(I)).LT.8099.) THEN
          J=J+1
          SIGP=ABS(X3+SL3*(YT(I)-Z3)-XT(I))
          IF(SIGP.LE.60.) THEN
            WT(J)=1./SIGT**2
          ELSE
            WT(J)=60.**2/SIGT**2/SIGP**2
          ENDIF
          X(J)=XT(I)
          Y(J)=YT(I)
        ENDIF
      ENDDO
      IF(J.LT.2) GO TO 999
      CALL MUFSUM(J,X,Y,WT,SWT,SZT,SZ2T,SXT,SX2T,SXZT)
      CALL MUSLIN(SWT,SXT,SZT,SX2T,SZ2T,SXZT,X3,Z3,SL3,VXG,VSL,C3)
      IF(C3.LT.-1000.) GO TO 999
C
C    SET UP PADS
C    FIND NEAREST VERNIER PAD IN ORDER TO SEED
C
      J=0
      DO I=1,N
        IF(ABS(XV(1,I)).LT.8099.) THEN   ! SKIP UNPHYSICAL
          J=J+1
          XP=X3+SL3*(YT(I)-Z3)
CCC   SEES IF VERNIER SOLUTION 1 OR 2 IS CLOSEST
          LA=(XP-XV(1,I))/VER+.5
          DELA=VER*LA+XV(1,I) -XP
          LB=(XP-XV(2,I))/VER+.5
          DELB=VER*LB+XV(2,I) -XP
          IF(ABS(DELA).LT.ABS(DELB)) THEN     ! 1 CLOSEST
            IV(J)=LA+1
          ELSE
            IV(J)=LB+1
          ENDIF
            IF(IV(J).LT.1) IV(J)=1
            K1(J)=1
            K2(J)=6
            IF(IV(J).EQ.1) K1(J)=3     ! EDGE OF CHAMBER
CCC  NOT EXACTLY RIGHT SINCE 1 SOLUTION CAN BE OK, OTHER OFF
            IF(IV(J)*VER+XV(1,I).GT.WLEN(I).AND.
     A         IV(J)*VER+XV(2,I).GT.WLEN(I)) K2(J)=4
          Y(J)=YT(I)
          XXV(1,J)=XV(1,I)
          XXV(2,J)=XV(2,I)
          JV(J)=I
        ENDIF
C        ENDIF
      ENDDO
CCCCC   SET REMAINDER TO 1
      IF(J.LT.NFIT) THEN
        DO I=J+1,NFIT
          K1(I)=1
          K2(I)=1
        ENDDO
      ENDIF
      IF(J.LE.1.OR.J.GT.NFIT) GO TO 999      ! can't fit
C
C    fit b-c layer
C
      CALL MUFIT3(VER,SWT,SZT,SZ2T,SXT,SX2T,SXZT,J,IV,K1,K2,Y,XXV,
     A  X1,Z1,SL1,C1,ISOL)
      IF(C1.LT.-.1) GO TO 999
C      SAVE DRIFT SOLUTIONS IN PACKED FORM
C
      DO I=1,J
        IS(JV(I))=ISOL(I)
      ENDDO
CCCC  REFIT CHANGING WEIGHT FOR DELTATS, DON'T USE VERTEX
      J=0
      DO I=1,N
        IF(ABS(XT(I)).LT.8099.) THEN
          J=J+1
          WTF(J)=1./(20.)**2
          XF(J)=XT(I)
          YF(J)=YT(I)
        ENDIF
        IF(ABS(XV(1,I)).LT.8999.) THEN   ! PADS
          II=0
          IF(IS(I).GT.0) II=2
          IF(IS(I).LT.0) II=1
          IF(II.NE.0) THEN
            J=J+1
            YF(J)=YT(I)
            XF(J)=VER*(IABS(IS(I))-1)+XV(II,I)
            WTF(J)=1.
          ENDIF
        ENDIF
        IF(J.EQ.50) GO TO 1537
      ENDDO
 1537 CONTINUE
      IF(J.LE.2) GO TO 999
      CALL MUFSUM(J,XF,YF,WTF,SWT,SZT,SZ2T,SXT,SX2T,SXZT)
      CALL MUSLIN(SWT,SXT,SZT,SX2T,SZ2T,SXZT,X1,Z1,SL1,VXG,VSL,C3)
      XM=X1+SL1*(ZBND-Z1)
      XA=XM
      ZM=ZBND
      SLO=SL1
      SLI=SL1
C   CALCULATE CHI SEPARATELY FOR DT AND VERNIER
C
      C1=0.
      CT=0.
      CV=0.
      N1=0
      NT=0
      NV=0
      DO I=1,N
        IF(ABS(YT(I)).GT.ABS(ZBND)) THEN
          XP=XM+SLO*(YT(I)-ZM)
        ELSE IF(ABS(YT(I)).LT.ABS(ZBND)) THEN
          XP=XA+SLI*(YT(I)-ZM)
        ENDIF
        IF(ABS(XT(I)).LT.9999.) THEN     ! TIME DIVISION
          N1=N1+1
          C1=C1 + (XP-XT(I))**2/20.**2   ! 20 cm weight
          NT=NT+1
          CT=CT + (XP-XT(I))**2        ! NO WEIGHT
        ENDIF
        IF(ABS(XV(1,I)).LT.8999.) THEN   ! PADS
          II=0
          IF(IS(I).GT.0) II=2
          IF(IS(I).LT.0) II=1
          IF(II.NE.0) THEN
            N1=N1+1
            C1=C1 + (XP-(VER*(IABS(IS(I))-1)+XV(II,I)))**2
            NV=NV+1
            CV=CV + (XP-(VER*(IABS(IS(I))-1)+XV(II,I)))**2
          ENDIF
        ENDIF
      ENDDO
      IF(N1.GT.1) THEN
        C1=SQRT(C1/(N1-1))
      ELSE
        C1=-1.
      ENDIF
      IF(NT.GT.1) THEN
        CT=SQRT(CT/(NT-1))
      ELSE
        CT=-1.
      ENDIF
      IF(NV.GT.1) THEN
        CV=SQRT(CV/(NV-1))
      ELSE
        CV=-1.
      ENDIF
C   use vertex to get inside direction cosine
      IF(IVER.EQ.0) THEN          ! BEAM
        SLI=(XM-VX)/(ZBND-VY)
      ENDIF
C----------------------------------------------------------------------
      RETURN
C-
  999 CONTINUE       ! CAN'T FIT FOR SOME REASON OR ANOTHER
      X1=9999.
      Z1=9999.
      SL1=9999.
      C1=9999.
      CT=9999.
      CV=9999.
      RETURN
      END

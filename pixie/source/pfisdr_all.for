      SUBROUTINE PFISDR_ALL(PHIA,PHIB,PHIC,PHID,IVIEW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw all charged particle Isajet tracks. 
C-
C-   Inputs  : PHIA,PHIB,PHIC,PHID = Phi angle limits for tracks drawn
C-             IVIEW = 1=XZ,2=YZ,3=RZ,4=XY
C-
C-   Created  16-AUG-1990   Jeffrey Bantly
C-   Updated  30-APR-1991   Jeffrey Bantly  add check on acos(val gt 1) 
C-   Updated   7-FEB-1992   Robert E. Avery   Check phi bounds with call to
C-       PFPHICHK. Upper half of plot is PHI1-PHI2.
C-   Updated   7-FEB-1992   Robert E. Avery  Change colour. 
C-   Updated  23-MAR-2004   compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$LINKS:IZISV2.LINK'
      INCLUDE 'D0$LINKS:IZISP2.LINK'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER MXISP2 
      PARAMETER( MXISP2 = 10 )
      INTEGER IDV1, ID1, IDV2, MID2, ID2(MXISP2)
      INTEGER LISV1,LISP1,LISV2,LISP2,KISV2,JISV2,FISV2
      INTEGER IVIEW,IVTYPE,MISP2,I
      INTEGER ISCHAR
      INTEGER RSIGN, PFPHICHK
C
      REAL    PHIA,PHIB,PHIC,PHID
      REAL    PV1(4),X1,Y1,Z1,R1
      REAL    PP1(4),PHI1,THETA1,ETA1,PPERP1
      REAL    PV2(4),PP2(4,MXISP2),X2,Y2,Z2,R2
      REAL    KPV2(4),KX2,KY2,KZ2,TZ2,TX2,TY2,TR2
      integer kidv2
      REAL    MPHI2,MTHETA2,META2,MPP2(4),FRACT
      REAL    PHI2(MXISP2),THETA2(MXISP2),ETA2(MXISP2),PPERP2(MXISP2)
      REAL    ANGLE(MXISP2),COSANGLE,NUM,DENOM1,DENOM2
C
      LOGICAL FIRST_ISV2
      logical xflag
      CHARACTER*4 TRKCLR
      DATA TRKCLR /'CYA '/
C----------------------------------------------------------------------
C  Get colour
C
      CALL EZPICK('PX_FDCDIS_RCP')
      CALL PUGETA('FDC COLR ISAJET',TRKCLR)
      CALL EZRSET
      CALL PXCOLR(TRKCLR)
C
      LISV1=0
  100 CALL GTISV1(LISV1,LISV1,IDV1,PV1,X1,Y1,Z1)        ! CHECK FOR ISV1
      IF(LISV1.LE.0) GOTO 101
      R1=SQRT(X1**2. + Y1**2.)
      RSIGN = PFPHICHK(X1,Y1,PHIA,PHIB,PHIC,PHID) 
      IF(RSIGN.LT.0.0) R1=R1*RSIGN
      IF(ABS(Z1).GT.100.) GOTO 100
C
      LISP1=LISV1-IZISP1
  200 CALL GTISP1(LISP1,LISP1,ID1,PP1,PHI1,THETA1,ETA1)   ! CHECK FOR ISP1
      IF(LISP1.LE.0) GOTO 201
      RSIGN = PFPHICHK(COS(PHI1),SIN(PHI1),PHIA,PHIB,PHIC,PHID) 
      IF ( RSIGN.EQ.0 ) THEN
        GOTO 200
      ENDIF
C
      FIRST_ISV2=.TRUE.
      LISV2=LISP1-IZISV2
  300 CALL GTISV2(LISV2,LISV2,IDV2,PV2,X2,Y2,Z2,IVTYPE) ! CHECK FOR DECAY
      xflag = .false.
 401  continue
      IF(LISV2.GT.0) THEN
        if (xflag) then
          xflag = .false.
          goto 400
        endif
        IF(FIRST_ISV2) THEN
          FISV2=LISV2
          IF( LQ(LISV2-2).NE.LISV1 ) GOTO 301
        ELSE
          IF( LQ(LISV2-2).EQ.LISV1 ) GOTO 301
          IF( LQ(LISV2-2).NE.FISV2 ) GOTO 301
        ENDIF
        R2=SQRT(X2**2. + Y2**2.)
        RSIGN = PFPHICHK(X2,Y2,PHIA,PHIB,PHIC,PHID) 
        IF (RSIGN.LT.0.0) R2=R2*RSIGN 
        IF(FIRST_ISV2 .AND. ISCHAR(ID1).NE.0) THEN
          TZ2=Z2
          IF(ABS(Z2).GT.150.) TZ2=150.*Z2/ABS(Z2)
          FRACT=(TZ2-Z1)/(Z2-Z1)
          TX2=X1 + (X2-X1)*FRACT
          TY2=Y1 + (Y2-Y1)*FRACT
          TR2=SQRT(TX2**2. + TY2**2.)
          RSIGN = PFPHICHK(TX2,TY2,PHIA,PHIB,PHIC,PHID) 
          IF (RSIGN.LT.0.0) TR2=TR2*RSIGN 
          IF (IVIEW.EQ.1) THEN
            CALL JMOVE(Z1,X1)
            CALL JDRAW(TZ2,TX2)
          ELSEIF(IVIEW.EQ.2) THEN
            CALL JMOVE(Z1,Y1)
            CALL JDRAW(TZ2,TY2)
          ELSEIF(IVIEW.EQ.3) THEN
            CALL JMOVE(Z1,R1)
            CALL JDRAW(TZ2,TR2)
          ELSEIF(IVIEW.EQ.4) THEN
            CALL JMOVE(X1,Y1)
            CALL JDRAW(TX2,TY2)
          ENDIF
        ENDIF
        FIRST_ISV2=.FALSE.
C
C ****  If ISV2 vertex outside CD, skip to next ISV2 vertex
C
        IF( ABS(Z2) .GT. 140. .OR. R2 .GT. 62. ) GOTO 300
C
C ****  Loop through all the ISP2 banks associated with the current ISV2 bank.
C
        MISP2=0
        LISP2=LISV2-IZISP2
  400   CALL GTISP2(LISP2,LISP2,MID2,MPP2,MPHI2,MTHETA2,META2)
        IF(LISP2.GT.0 .AND. MISP2.LT.10) THEN
C
          MISP2=MISP2+1
          ID2(MISP2)    =MID2
          DO I=1,4
            PP2(I,MISP2)=MPP2(I)
          ENDDO
          PHI2(MISP2)   =MPHI2
          THETA2(MISP2) =MTHETA2
          ETA2(MISP2)   =META2
C
          NUM = PP1(1)*PP2(1,MISP2) + PP1(2)*PP2(2,MISP2) +
     &                                        PP1(3)*PP2(3,MISP2)
          DENOM1 = SQRT(PP1(1)**2. + PP1(2)**2. + PP1(3)**2.)
          DENOM2 = SQRT(PP2(1,MISP2)**2. + PP2(2,MISP2)**2. +
     &                                        PP2(3,MISP2)**2.)
          COSANGLE = NUM / (DENOM1*DENOM2)
          IF(COSANGLE .GT. 1.000000000000) COSANGLE=1.0000000000
          ANGLE(MISP2) = ACOS(COSANGLE)
          PPERP1= SQRT(PP1(1)**2. + PP1(2)**2.)
          PPERP2(MISP2)= SQRT(PP2(1,MISP2)**2. + PP2(2,MISP2)**2.)
C
          IF(LQ(LISP2-4).GT.0) THEN     ! another vertex
            KISV2=LISP2-IZISV2
            JISV2=LQ(LISP2-1)
            CALL GTISV2(KISV2,KISV2,KIDV2,KPV2,KX2,KY2,KZ2,IVTYPE)
            KZ2=150.*MPP2(3)/ABS(MPP2(3))
C            KX2=( ABS(KZ2-Z2)*TAN(MTHETA2) ) * COS(MPHI2)
C            KY2=( ABS(KZ2-Z2)*TAN(MTHETA2) ) * SIN(MPHI2)
            KX2=(KZ2-Z2)*MPP2(1)/MPP2(3)
            KY2=(KZ2-Z2)*MPP2(2)/MPP2(3)
          ELSE                          ! ISP2 goes to infinity
            KZ2=150.*MPP2(3)/ABS(MPP2(3))
C            KX2=( ABS(KZ2-Z2)*TAN(MTHETA2) ) * COS(MPHI2)
C            KY2=( ABS(KZ2-Z2)*TAN(MTHETA2) ) * SIN(MPHI2)
            KX2=(KZ2-Z2)*MPP2(1)/MPP2(3)
            KY2=(KZ2-Z2)*MPP2(2)/MPP2(3)
          ENDIF
          IF(KZ2.EQ.Z2) THEN
            TZ2=Z2
            TX2=X2
            TY2=Y2
            TR2=R2
          ELSE
            TZ2=KZ2
            IF(ABS(KZ2).GT.150.) TZ2=150.*KZ2/ABS(KZ2)
            FRACT=(TZ2-Z2)/(KZ2-Z2)
C            TX2=X2 + (KX2-X2)*FRACT
C            TY2=Y2 + (KY2-Y2)*FRACT
            TX2=X2 + (KX2)*FRACT
            TY2=Y2 + (KY2)*FRACT
            TR2=SQRT(TX2**2. + TY2**2.)
            RSIGN = PFPHICHK(TX2,TY2,PHIA,PHIB,PHIC,PHID) 
            IF (RSIGN.LT.0.0) TR2=TR2*RSIGN 
          ENDIF
        ENDIF
      ELSE
        GOTO 301
      ENDIF
      IF(ISCHAR(MID2).NE.0) THEN
        IF(IVIEW.EQ.1) THEN
          CALL JMOVE(Z2,X2)
          CALL JDRAW(TZ2,TX2)
        ELSEIF(IVIEW.EQ.2) THEN
          CALL JMOVE(Z2,Y2)
          CALL JDRAW(TZ2,TY2)
        ELSEIF(IVIEW.EQ.3) THEN
          CALL JMOVE(Z2,R2)
          CALL JDRAW(TZ2,TR2)
        ELSEIF(IVIEW.EQ.4) THEN
          CALL JMOVE(X2,Y2)
          CALL JDRAW(TX2,TY2)
        ENDIF
      ENDIF
      IF(LISP2.GT.0) GOTO 401              ! End of valid ISP2 banks
C
      GOTO 300                             ! End of valid ISV2 banks
  301 CONTINUE
C
      IF(FIRST_ISV2 .AND. ISCHAR(ID1).NE.0) THEN   ! no ISV2 for this ISP1
        TZ2=150.*PP1(3)/ABS(PP1(3))
        TX2=(TZ2-Z1)*PP1(1)/PP1(3)
        TY2=(TZ2-Z1)*PP1(2)/PP1(3)
        TR2=SQRT(TX2**2. + TY2**2.)
        TR2=RSIGN*TR2
        IF(IVIEW.EQ.1) THEN
          CALL JMOVE(Z1,X1)
          CALL JDRAW(TZ2,TX2)
        ELSEIF(IVIEW.EQ.2) THEN
          CALL JMOVE(Z1,Y1)
          CALL JDRAW(TZ2,TY2)
        ELSEIF(IVIEW.EQ.3) THEN
          CALL JMOVE(Z1,R1)
          CALL JDRAW(TZ2,TR2)
        ELSEIF(IVIEW.EQ.4) THEN
          CALL JMOVE(X1,Y1)
          CALL JDRAW(TX2,TY2)
        ENDIF
      ENDIF
      GOTO 200
  201 CONTINUE                             ! End of valid ISP1 banks
      GOTO 100
  101 CONTINUE                             ! End of valid ISV1 banks
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END

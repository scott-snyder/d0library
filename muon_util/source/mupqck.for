      SUBROUTINE MUPQCK(QQUAD,XI,YI,ZI,X,Y,Z,
     A  DIX,DIY,DIZ,DOX,DOY,DOZ,P,DP,ELCAL,ELFE,BDL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC quick and dirty momentum finder for muon tracks. Includes
CC pseudo field map and energy loss. The forward region (corners)
CC is degraded as does only projection
CC     INPUT: QQUAD  0=basement, 1-4=central, 5-12=ends
CC            XI,YI,ZI position at A-layer
CC            X,Y,Z  POSITION MAGNET CENTER (FROM B_C LAYER)
CC            DIX-DOZ, direction cosines inside and outside
CC     OUTPUT: P,DP momentum (signed) and error
CC             ELCAL,ELFE  energy loss in cal and iron
CC             BDL  integral field
CC    Hedin 3/6/89:: ONLY DO QUADRANTS 0-4 FOR NOW
CC    DH 4/89 CHANGE SIGN QUAD 2,4
CC    DH 9/89 change to average of point/line and line-line for
CC      all momentum. point-line is small angle approximation
CC      (basement only for now)
CC    DH 3/90 do quick and dirty integral bdl; also end quadrants
CC       USE LINE-LINE FOR OUTPUT.
CC    DH 9/90 FLIP SIGNS
CC    DH 11/90 add simple momentum resolution; SAME FOR 2-3 MODULE TRACKS
CC    DH 1/91 SAVE ENERGY LOSS
CC    DH 8/91 do end corners better; comment out. change ELOSSFE meaning
CC    DH 10/91 allow for multiquads
CC    DH 1/92 CALL BETTER FIELD MAP
C-   Updated   5-FEB-1992   Daria Zieminska  turn on the "corners" code
CC    DH 2/92 only do 'corner' code if pads used in fit
CC    DH 4/92 add longer length for SAMUS
CC    Atsushi Taketani 02-OCT-92 read STP information, but step size is 
CC                               not fixed yet.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INTEGER QUAD,I,QQUAD,FIRST,IPDFIT,IER
      REAL X,Y,Z,DIX,DIY,DIZ,DOX,DOY,DOZ,P,DP,BX,BY,BZ,ELFE,ELCAL
      REAL DT,YCOS2,BDL,XI,YI,ZI,DELX,L1,P1,P2,BX1,BY1,BZ1,
     A  XX,YY,ZZ,A,B,T,TR,RES,VECT(3),BB(3),THICK
C to read STP
      CHARACTER*4 HSHAPE
      INTEGER NSPAR,NBUF,IBUF
      REAL    SPAR(3),XPAR(3),ROTM(3,3)
C
      DATA FIRST/0/
      IF(FIRST.EQ.0) THEN
        FIRST=1
        CALL EZGET('IPDFIT',IPDFIT,IER)
      ENDIF
      QUAD=MOD(QQUAD,100)
C
CCC   QUICK AND DIRTY INTEGRAL BDL. 21 POINTS. FIND AVERAGE
CCC    use only one set of direction cosines
      BX=0.
      BY=0.
      BZ=0.
      DO I=1,21
        IF(QUAD.EQ.1.OR.QUAD.EQ.3) THEN
          CALL MUMAGS(QUAD,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
          XX=X-SPAR(1)+I*5.
          YY=Y+(XX-X)*DOY/DOX
          ZZ=Z+(XX-X)*DOZ/DOX
C          CALL MUBFLD(QUAD,XX,YY,ZZ,BX1,BY1,BZ1)
        ELSE IF(QUAD.EQ.0.OR.QUAD.EQ.2.OR.QUAD.EQ.4) THEN
          IF ( QUAD.NE.0 ) THEN
            CALL MUMAGS(QUAD,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
          ELSE
            SPAR(1) = 55.0   ! for basement
          END IF
          YY=Y-SPAR(1)+I*5.
          XX=X+(YY-Y)*DOX/DOY
          ZZ=Z+(YY-Y)*DOZ/DOY
C          CALL MUBFLD(QUAD,XX,YY,ZZ,BX1,BY1,BZ1)
        ELSE IF(QUAD.GT.4) THEN      ! NORTH/SOUTH ENDS
          IF ( QUAD.GE.5.AND.QUAD.LE.8 ) THEN  ! North ends
            CALL MUMAGS(25,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
          ELSE                                 ! South ends
            CALL MUMAGS(21,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
          END IF
          ZZ=Z-SPAR(3)+I*7.
          XX=X+(ZZ-Z)*DOX/DOZ
          YY=Y+(ZZ-Z)*DOY/DOZ
C          CALL MUBFLD(QUAD,XX,YY,ZZ,BX1,BY1,BZ1)
        ENDIF
        VECT(1)=XX
        VECT(2)=YY
        VECT(3)=ZZ
        CALL GTMFLD(QUAD,VECT,BB)
        BX=BX+BB(1)/21.
        BY=BY+BB(2)/21.
        BZ=BZ+BB(3)/21.
C        BX=BX+BX1/21.
C        BY=BY+BY1/21.
C        BZ=BZ+BZ1/21.
      ENDDO
C
      DT=0.
      DELX=0.
      IF(IPDFIT.EQ.0.OR.QUAD.GT.12) THEN     ! USE PADS IN FIT
        IF(QUAD.EQ.0) THEN       ! BASEMENT
          DT=SQRT((DIY-DOY)**2+(DIZ-DOZ)**2)
          IF(DIZ-DOZ.LT.0.) DT=-DT
          YCOS2=DOY/SQRT(DOY**2+DOZ**2)
          BDL=BX*.03/YCOS2 ! FIELD LENGTH include path
          L1=(Y-YI)/YCOS2       ! length
          DELX=(Z-ZI) + DOZ/DOY*(YI-Y)
        ELSE IF(QUAD.EQ.2.OR.QUAD.EQ.4) THEN    ! CENTRAL TOP/BOTTOM
          DT=SQRT((DIY-DOY)**2+(DIZ-DOZ)**2)
          IF(DIZ-DOZ.LT.0.) DT=-DT
          YCOS2=DOY/SQRT(DOY**2+DOZ**2)
          BDL=-BX*.03/YCOS2*1.09
          L1=(Y-YI)/YCOS2       ! length
          DELX=(Z-ZI) + DOZ/DOY*(YI-Y)
        ELSE IF(QUAD.EQ.1.OR.QUAD.EQ.3) THEN    ! CENTRAL SIDES
          DT=SQRT((DIX-DOX)**2+(DIZ-DOZ)**2)
          IF(DIZ-DOZ.LT.0.) DT=-DT
          YCOS2=DOX/SQRT(DOX**2+DOZ**2)
          BDL=BY*.03/YCOS2*1.09          ! 1.09 M
          L1=(X-XI)/YCOS2       ! length
          DELX=(Z-ZI) + DOZ/DOX*(XI-X)
        ELSE IF(QUAD.GT.4) THEN
CCC  FIRST GET MAGNET THICKNESS;
          THICK=1.52
          IF(ABS(X).LT.81.3.AND.ABS(Y).LT.89.9) THEN   ! IN SAMUS
            THICK=1.656
          ENDIF
          IF (ABS(BY).GT.ABS(BX)) THEN ! end side
            DT=SQRT((DIX-DOX)**2+(DIZ-DOZ)**2)
            IF(DIX-DOX.LT.0.) DT=-DT
            YCOS2=DOZ/SQRT(DOX**2+DOZ**2)
            BDL=-BY*.03/YCOS2*THICK
            L1=(Z-ZI)/YCOS2       ! length
            DELX=(X-XI) + DOX/DOZ*(ZI-Z)
CC  DO CORNERS
            IF(ABS(BY).GT.1.) THEN
              IF(ABS(BX).GT.0.7*ABS(BY)) THEN
                DT=SQRT((DIX-DOX)**2+(DIY-DOY)**2+(DIZ-DOZ)**2)
                IF(DIX-DOX.LT.0.) DT=-DT
                BDL=-BY*.03*1.52/DOZ*SQRT((DOZ**2+DOX**2)+BX**2/BY**2*
     A            (DOZ**2+DOY**2)-2./BY*BX*DOX*DOY)
              ENDIF
            ENDIF
          ELSE IF (ABS(BX).GT.ABS(BY)) THEN ! end top/bottom
            DT=SQRT((DIY-DOY)**2+(DIZ-DOZ)**2)
            IF(DIY-DOY.LT.0.) DT=-DT
            YCOS2=DOZ/SQRT(DOY**2+DOZ**2)
            BDL=BX*.03/YCOS2*THICK
            L1=(Z-ZI)/YCOS2       ! length
            DELX=(Y-YI) + DOY/DOZ*(ZI-Z)
CC  DO CORNERS
            IF(ABS(BX).GT.1.) THEN
              IF(ABS(BY).GT.0.7*ABS(BX)) THEN
                DT=SQRT((DIX-DOX)**2+(DIY-DOY)**2+(DIZ-DOZ)**2)
                IF(DIY-DOY.LT.0.) DT=-DT
                BDL=BX*.03*1.52/DOZ*SQRT((DOZ**2+DOY**2)+BY**2/BX**2*
     A            (DOZ**2+DOX**2)-2./BX*BY*DOX*DOY)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ELSE   ! JUST USE DELTAT'S SO RESOLUTION POORER
        IF(QUAD.EQ.0) THEN       ! BASEMENT
          DT=SQRT((DIY-DOY)**2+(DIZ-DOZ)**2)
          IF(DIZ-DOZ.LT.0.) DT=-DT
          YCOS2=DOY/SQRT(DOY**2+DOZ**2)
          BDL=BX*.03/YCOS2 ! FIELD LENGTH include path
          L1=(Y-YI)/YCOS2       ! length
          DELX=(Z-ZI) + DOZ/DOY*(YI-Y)
        ELSE IF(QUAD.EQ.2.OR.QUAD.EQ.4) THEN    ! CENTRAL TOP/BOTTOM
          DT=SQRT((DIY-DOY)**2+(DIZ-DOZ)**2)
          IF(DIZ-DOZ.LT.0.) DT=-DT
          YCOS2=DOY/SQRT(DOY**2+DOZ**2)
          BDL=-BX*.03/YCOS2*1.09
          L1=(Y-YI)/YCOS2       ! length
          DELX=(Z-ZI) + DOZ/DOY*(YI-Y)
        ELSE IF(QUAD.EQ.1.OR.QUAD.EQ.3) THEN    ! CENTRAL SIDES
          DT=SQRT((DIX-DOX)**2+(DIZ-DOZ)**2)
          IF(DIZ-DOZ.LT.0.) DT=-DT
          YCOS2=DOX/SQRT(DOX**2+DOZ**2)
          BDL=BY*.03/YCOS2*1.09          ! 1.09 M
          L1=(X-XI)/YCOS2       ! length
          DELX=(Z-ZI) + DOZ/DOX*(XI-X)
        ELSE IF(QUAD.EQ.5.OR.QUAD.EQ.7.OR.QUAD.
     A    EQ.9.OR.QUAD.EQ.11) THEN    ! END SIDES
CCC  FIRST GET MAGNET THICKNESS;
          IF ( QUAD.GE.5.AND.QUAD.LE.8 ) THEN  ! North ends
            CALL MUMAGS(25,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
          ELSEIF (QUAD.GE.9.AND.QUAD.LE.12) THEN ! South ends
            CALL MUMAGS(21,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
          END IF
          THICK= SPAR(3)*2.0
          IF(ABS(X).LT.81.3.AND.ABS(Y).LT.89.9) THEN   ! IN SAMUS
            IF ( Z.GT.0 ) THEN
              CALL GTSMAG(2,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
            ELSE
              CALL GTSMAG(1,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
            END IF
            THICK=SPAR(3)*2.0
          ENDIF
        DT=SQRT((DIX-DOX)**2+(DIZ-DOZ)**2)
        IF(DIX-DOX.LT.0.) DT=-DT
        YCOS2=DOZ/SQRT(DOX**2+DOZ**2)
        BDL=-BY*.03/YCOS2*THICK
        L1=(Z-ZI)/YCOS2       ! length
        DELX=(X-XI) + DOX/DOZ*(ZI-Z)
      ELSE IF(QUAD.EQ.6.OR.QUAD.EQ.8.OR.QUAD.
     A    EQ.10.OR.QUAD.EQ.12) THEN    ! END TOP/BOTTOM
CCC  FIRST GET MAGNET THICKNESS;
        IF ( QUAD.GE.5.AND.QUAD.LE.8 ) THEN  ! North ends
          CALL MUMAGS(25,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
        ELSE IF(QUAD.GE.9.AND.QUAD.LE.12) THEN ! South ends
        CALL MUMAGS(21,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
      END IF
      THICK= SPAR(3)*2.0
      IF(ABS(X).LT.81.3.AND.ABS(Y).LT.89.9) THEN   ! IN SAMUS
        IF ( Z.GT.0 ) THEN
          CALL GTSMAG(2,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
        ELSE
          CALL GTSMAG(1,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
        END IF
        THICK=SPAR(3)*2.0
      ENDIF
      DT=SQRT((DIY-DOY)**2+(DIZ-DOZ)**2)
      IF(DIY-DOY.LT.0.) DT=-DT
      YCOS2=DOZ/SQRT(DOY**2+DOZ**2)
      BDL=BX*.03/YCOS2*THICK
      L1=(Z-ZI)/YCOS2       ! length
      DELX=(Y-YI) + DOY/DOZ*(ZI-Z)
      ENDIF
      ENDIF
C
      IF(DT.EQ.0.) THEN
        P1=999.
      ELSE
        P1=-BDL/DT
      ENDIF
      IF(DELX.EQ.0.) THEN
        P2=999.
      ELSE
        P2=-BDL*L1/DELX
      ENDIF
      BDL=ABS(BDL)
      IF(P1.NE.999..OR.P2.NE.999.) THEN
        IF(P2.EQ.999.) THEN
          P=P1
        ELSE IF(P1.EQ.999.) THEN
          P=P2
        ELSE
C          P=P2             ! use point-line
          P=P1             ! use line-line
        ENDIF
      ENDIF
      IF(P.NE.999.) THEN
        CALL MUELOS(QUAD,ABS(P),X,Y,Z,DIX,DIY,DIZ,DOX,DOY,DOZ,
     A    ELFE,ELCAL)  ! energy loss
CCC  LIMITS
        IF(ELFE.GT.20.) ELFE=20.
        IF(ELCAL.GT.20.) ELCAL=20.
CCC  DIVIDE ELFE BY 2 TO DO 'AVERAGE'
        IF(P.GT.0.) THEN
          P=P+ ELFE/2.+ELCAL     ! ENERGY LOSS
        ELSE
          P=P- ELFE/2.-ELCAL
        ENDIF
      ENDIF
      IF(P.NE.999.) THEN            ! DO RESOLUTION
        T=ABS(ACOS(DIZ))*180./3.14159
        IF(T.GT.90.) T=180.-T
        TR=T*3.14159/180.
        IF(T.GT.45.) THEN
          A=.18*SQRT(SIN(TR))
          B=.002*SIN(TR)
        ELSE IF(T.GT.35..AND.T.LE.45.) THEN
          A=.18*SQRT(SIN(TR))/SQRT(.5)
          B=.004*SIN(TR)
        ELSE IF(T.GT.20..AND.T.LE.35.) THEN
          A=.18*SQRT(COS(TR))/SQRT(1.5)
          B=.0015*COS(TR)
        ELSE
          A=.18*SQRT(COS(TR))/SQRT(1.5)
          B=.001*COS(TR)
        ENDIF
        RES=SQRT(A**2 + (P*B)**2)
        DP=RES*ABS(P)                   ! DON'T DO 1/P
      ENDIF
      RETURN
      END

      SUBROUTINE VEE(TRAK1,TRAK2,PHI0,DPHI0,THE0,DTHE0)
C------------------------------------------------------------------
C
C  Check if the pair (TRAK1,TRAK2) of central tracks makes a vee.
C
C  Daria Zieminska 6-JUL-1990
C  Tom Trippe 12-AUG-1991   add EZRSET, ZIMPACT calls, D0 stds.
C-   Updated   7-NOV-1991   Daria Zieminska  store angles in VEEKIN
C-   Updated  14-DEC-1991   Daria Zieminska  call ZENERGY;
C-                          Require VTX track if the Vee is before the VTX
C-                          volume; added arguments defining the road for
C-                          search of Vees.
C-   Updated  21-SEP-1993   H. Castilla, B. Gomez & O. Ramirez to take into
C-                          account the case when both tracks hit the same tower
C
C------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF/LIST'
      INCLUDE 'D0$INC:VEEKIN.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZISAE.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISV2.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISP2.LINK/LIST'
      INTEGER LZLOC,LISAE
      INTEGER LISV2I,NISV2,IFL
      INTEGER NV,K,K1,K2,K3,IORV,IORP,LISV2,LISP2,IS
CCC
      INTEGER NC1,NC2,NF1,NF2
CCC
      CHARACTER*8 NAME,LABEL
      INTEGER TRAK1,TRAK2,NPRIM,ICALL,IER,IFDCT
      INTEGER LBIT,ON1,ON2
      REAL CONST1,CONST2,ESCALE,PVEE_MIN,P1_MIN,P1,P2,ESUM,ZENERGY
      REAL XZ2,YZ1,XZ1,YZ2,DET,DX0,DY0
      REAL PHI0,DPHI0,THE0,DTHE0
      LOGICAL CAL,CORRECT
      INTEGER LVTX1,LVTX2,LCDC1,LCDC2,LFDC1,LFDC2
      INTEGER ISTAT1(4),LZTRK1,LZTRK2,ISTAT2(4),GZZTRK
      REAL PRIM(3),EPRIM(3),ZPRIM(5),DZPRIM(5),PS(2),RATIO
      REAL PHI1,PHI2,XG1,XG2,YG1,YG2,SG1,SG2,ZG1,ZG2,XV,YV,Z0(2)
      REAL EPHI1,EPHI2,ETHE1,ETHE2,DELPHI,DELPHIMX,DELPHIMX1,DELPHIMX2
      REAL DELTHEMX,DELTHE,ETA1,ETA2,DELETA
      REAL SV1,SV2,THE1,THE2,ZV1,ZV2,DELZMX,DELZMXF,IMPMIN(3)
      REAL XYZ(3),EXYZ(3),DXPRIM,DYPRIM,ZIMPACT,B1,B2
      INTEGER RUN,ID,RUNSAV,IDSAV,LOC,GZISV1,N1,N2,PRUNIT,USUNIT,STATUS
      LOGICAL OK,THETA1_VTX,THETA2_VTX
      SAVE RUNSAV,IDSAV,PRIM
      SAVE ICALL
      DATA RUNSAV,IDSAV/-1,-1/
      DATA ICALL/0/
C------------------------------------------------------------------
C
C Create/set HBOOK directory VEES
C
      CALL DHDIR('VEES_RCP','HBOOK_DIRECTORY',IER,' ')
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('VEES','VEEHIS',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VEES_RCP')
        CALL EZGET('DELZMX',DELZMX,IER)
        CALL EZGET('DELZMXF',DELZMXF,IER)
        CALL EZGET('DELPHIMX1',DELPHIMX1,IER)
        CALL EZGET('DELPHIMX2',DELPHIMX2,IER)
        CALL EZGET('DELTHEMX',DELTHEMX,IER)
        CALL EZGET_rarr('IMPMIN',IMPMIN,IER)
        CALL EZGET_l(  'CALL_ZENERGY', CAL,IER)
        CALL EZGET_l(  'CORRECT_ENERGY', CORRECT,IER)
        CALL EZGET(  'CONST1', CONST1,IER)
        CALL EZGET(  'CONST2', CONST2,IER)
        CALL EZGET(  'ESCALE', ESCALE,IER)
        CALL EZGET(  'PVEE_MIN', PVEE_MIN,IER)
        CALL EZGET(  'P1_MIN', P1_MIN,IER)
        CALL EZRSET
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET_rarr('Z0',Z0,IER)
        CALL EZRSET
        CALL HBOOK1(101,' Impact parameter of VTX tracks',100,0.,1.0,0.)
        CALL HBOOK1(102,' Impact parameter of CDC tracks',100,0.,1.0,0.)
        CALL HBOOK1(103,' Impact parameter of FDC tracks',100,0.,1.0,0.)
        PRUNIT=USUNIT()
        ICALL=1
      END IF
      CALL EVNTID(RUN,ID)
C
C  Get coordinates of the primary vertex
C
      IF(RUN.NE.RUNSAV.OR.ID.NE.IDSAV) THEN
        RUNSAV=RUN
        IDSAV=ID
        CALL VXY_BEAM(RUN,PRIM(1),EPRIM(1),PRIM(2),EPRIM(2),STATUS)
        CALL ZVERTE(NPRIM,ZPRIM,DZPRIM)
C------------------------------------------------------------
C  temporary: use Isajet vertex
C
C        LOC=GZISV1()
C        ZPRIM(1)=Q(LOC+9)
C        DZPRIM(1)=0.2
C        NPRIM=1
C------------------------------------------------------------
        IF (NPRIM.GT.0) THEN
          PRIM(3)=ZPRIM(1)
          EPRIM(3)=DZPRIM(1)
        END IF
      ENDIF
      IF (NPRIM.EQ.0) GO TO 1000
      IF (NPRIM.GT.1) GO TO 1000
      LZTRK1=GZZTRK(TRAK1)       ! Location of the 1-st track bank
      IF (LZTRK1.LE.0) GO TO 1000 ! Bank doesn't exist 
      IF (Q(LZTRK1+7).LT.-0.9) GO TO 1000 ! Bad CAL 
      LZTRK2=GZZTRK(TRAK2)
      IF (LZTRK2.LE.0) GO TO 1000 
      IF (Q(LZTRK2+7).LT.-0.9) GO TO 1000 ! Bad CAL 
      B1=ZIMPACT(LZTRK1,0,N1)
      IF (TRAK2.EQ.TRAK1+1) CALL HFILL(100+N1,B1,0.,1.)
      IF (B1 .LT. IMPMIN(N1)) GO TO 1000
      B2=ZIMPACT(LZTRK2,0,N2)
      IF (B2 .LT. IMPMIN(N2)) GO TO 1000
      LVTX1=0
      LVTX2=0
      LCDC1=0
      LCDC2=0
      LFDC1=0
      LFDC2=0
C
C  Find parameters of 1-st track in r-phi
C
      IF (N1.EQ.1) THEN  ! use the VTX component if available
        LVTX1=LQ(LZTRK1-6)          ! Ref. link to VTX track
        PHI1=Q(LVTX1+6)
        EPHI1=Q(LVTX1+16)
        XG1=Q(LVTX1+7)
        YG1=Q(LVTX1+8)
        ON1=IQ(LVTX1+3)
      ELSE IF(N1.EQ.2) THEN
        LCDC1=LQ(LZTRK1-7)          ! Ref. link to CDC track
        PHI1=Q(LCDC1+6)
        EPHI1=Q(LCDC1+16)
        EPHI1=MAX(EPHI1,0.001)
        XG1=Q(LCDC1+7)
        YG1=Q(LCDC1+8)
        ON1=IQ(LCDC1+3)
      ELSE IF(N1.EQ.3) THEN
        LFDC1=LQ(LZTRK1-8)          ! Ref. link to FDC track
        PHI1=Q(LFDC1+6)
        EPHI1=Q(LFDC1+23)
        EPHI1=MAX(EPHI1,0.001)
        XG1=Q(LFDC1+4)
        YG1=Q(LFDC1+5)
        ON1=IQ(LFDC1+3)
      ELSE 
        GO TO 1000
      END IF
      DELPHI=ABS(PHI1-PHI0)
      IF (DELPHI.GT.PI) DELPHI=TWOPI-DELPHI
      IF (DELPHI.GT.DPHI0) GO TO 1000
C
C  Find parameters of 2-nd track in r-phi
C
      IF (N2.EQ.1) THEN
        LVTX2=LQ(LZTRK2-6)
        PHI2=Q(LVTX2+6)
        EPHI2=Q(LVTX2+16)
        XG2=Q(LVTX2+7)
        YG2=Q(LVTX2+8)
        ON2=IQ(LVTX2+3)
      ELSE IF(N2.EQ.2) THEN
        LCDC2=LQ(LZTRK2-7)
        PHI2=Q(LCDC2+6)
        EPHI2=Q(LCDC2+16)
        EPHI2=MAX(EPHI2,0.001)
        XG2=Q(LCDC2+7)
        YG2=Q(LCDC2+8)
        ON2=IQ(LCDC2+3)
      ELSE IF(N2.EQ.3) THEN
        LFDC2=LQ(LZTRK2-8)
        PHI2=Q(LFDC2+6)
        EPHI2=Q(LFDC2+23)
        EPHI2=MAX(EPHI2,0.001)
        XG2=Q(LFDC2+4)
        YG2=Q(LFDC2+5)
        ON2=IQ(LFDC2+3)
      ELSE 
        GO TO 1000
      END IF
      DELPHI=ABS(PHI2-PHI0)
      IF (DELPHI.GT.PI) DELPHI=TWOPI-DELPHI
      IF (DELPHI.GT.DPHI0) GO TO 1000
C
C  Check if conditions for a vee are satisfied in the r-phi plane
C
      IF (N1.EQ.1.OR.N2.EQ.1) THEN
        DELPHIMX=DELPHIMX1
      ELSE
        DELPHIMX=DELPHIMX2
      END IF
      DELPHI=ABS(PHI1-PHI2)
      IF (DELPHI.GT.PI) DELPHI=TWOPI-DELPHI
      IF (DELPHI.GT.DELPHIMX) GO TO 1000  
      IF (DELPHI.LT.0.001) GO TO 1000 ! tracks parallel in r-phi
      IF ((N1 .EQ. 0) .OR. (N2 .EQ. 0)) GO TO 1000
      NC1=0
      NC2=0
      NF1=0
      NF2=0
      IF (LCDC1.NE.0.AND.LCDC2.NE.0) THEN
        NC1=IQ(LCDC1-5)
        NC2=IQ(LCDC2-5)
      END IF
      IF (LFDC1.NE.0.AND.LFDC2.NE.0) THEN
        NF1=IQ(LFDC1-5)
        NF2=IQ(LFDC2-5)
      END IF
      CALL VEE2D(PHI1,XG1,YG1,ON1,N1,PHI2,XG2,YG2,ON2,N2,XV,YV,OK)
      IF (.NOT.OK) GO TO 1000
      DO 101 LBIT=9,12
        ISTAT1(LBIT-8)=IBITS(IQ(LZTRK1),LBIT,1)
        ISTAT2(LBIT-8)=IBITS(IQ(LZTRK2),LBIT,1)
  101 CONTINUE
C
C  Find ZV1, ZV2: z at (x,Y)=(XV,YV) for the two tracks. 
C  Use outer chambers if possible (theta not well measured in the VTX)
C
      THETA1_VTX=LVTX1.GT.0.AND.LCDC1.EQ.0.AND.LFDC1.EQ.0
      THETA1_VTX=THETA1_VTX.OR.ISTAT1(1).EQ.1
      IF (THETA1_VTX) THEN
        THE1=Q(LVTX1+9)
        ETHE1=Q(LVTX1+18)
        ETHE1=MAX(ETHE1,0.1)
        IF (THE1.GT.0.) THEN
          SG1=Q(LVTX1+10)
          ZG1=Q(LVTX1+11)
          SV1=-SQRT((XV-XG1)**2+(YV-YG1)**2)
          ZV1=ZG1+(SV1-SG1)/TAN(THE1)
        ELSE 
          GO TO 1000
        END IF
      ELSE
        IF(LCDC1.GT.0) THEN
          THE1=Q(LCDC1+9)
          ETHE1=Q(LCDC1+18)
          ETHE1=MAX(ETHE1,0.040)  ! Doreco v11 mean
          IF (THE1.GT.0.) THEN
            ZG1=Q(LCDC1+11)
C           SG1=0 in CDC parametrization 
            SV1=-SQRT((XV-Q(LCDC1+7))**2+(YV-Q(LCDC1+8))**2)
            ZV1=ZG1+SV1/TAN(THE1)
          ELSE 
            GO TO 1000
          END IF
        END IF
        IF(LFDC1.GT.0) THEN
          THE1=Q(LFDC1+22)
          ETHE1=Q(LFDC1+24)
          ETHE1=MAX(ETHE1,0.001)
          IF (THE1.LT.1.57) THEN
            ZG1=Z0(2)
          ELSE
            ZG1=Z0(1)
          END IF
C          IFDCT=IQ(LFDC1-5)
C          CALL FGETZ0(IFDCT,ZG1)  ! crashing on STA data
          ZV1=XV+YV-(Q(LFDC1+4)+Q(LFDC1+5))
          ZV1=ZV1/(Q(LFDC1+7)+Q(LFDC1+8))+ZG1
        END IF
      END IF
      DELTHE=ABS(THE1-THE0)
      IF (DELTHE.GT.DTHE0) GO TO 1000
      THETA2_VTX=LVTX2.GT.0.AND.LCDC2.EQ.0.AND.LFDC2.EQ.0
      THETA2_VTX=THETA2_VTX.OR.ISTAT2(1).EQ.1
      IF (THETA2_VTX) THEN
        THE2=Q(LVTX2+9)
        ETHE2=Q(LVTX2+18)
        ETHE2=MAX(ETHE1,0.1)
        IF (THE2.GT.0.) THEN
          SG2=Q(LVTX2+10)
          ZG2=Q(LVTX2+11)
          SV2=-SQRT((XV-XG2)**2+(YV-YG2)**2)
          ZV2=ZG2+(SV2-SG2)/TAN(THE2)
        ELSE 
          GO TO 1000 
        END IF
      ELSE
        IF(LCDC2.GT.0) THEN 
          THE2=Q(LCDC2+9)
          ETHE2=Q(LCDC2+18)
          ETHE2=MAX(ETHE2,0.040)   ! Doreco v11 mean
          IF (THE2.GT.0.) THEN
            ZG2=Q(LCDC2+11)
C           SG2=0 in CDC parametrization 
            XG2=Q(LCDC2+7)
            YG2=Q(LCDC2+8)
            SV2=-SQRT((XV-XG2)**2+(YV-YG2)**2)
            ZV2=ZG2+SV2/TAN(THE2)
          ELSE 
            GO TO 1000
          END IF
        END IF
        IF(LFDC2.GT.0) THEN
          THE2=Q(LFDC2+22)
          ETHE2=Q(LFDC2+24)
          ETHE2=MAX(ETHE2,0.001)
          IF (THE2.LT.1.57) THEN
            ZG2=Z0(2)
          ELSE
            ZG2=Z0(1)
          END IF
C          IFDCT=IQ(LFDC2-5)
C          CALL FGETZ0(IFDCT,ZG2)
          ZV2=XV+YV-(Q(LFDC2+4)+Q(LFDC2+5))
          ZV2=ZV2/(Q(LFDC2+7)+Q(LFDC2+8))+ZG2
        END IF
      END IF
      DELTHE=ABS(THE2-THE0)
      IF (DELTHE.GT.DTHE0) GO TO 1000
C
C  Compare ZV1, ZV2 
C
      DELTHE=ABS(THE1-THE2)
      IF (DELTHE.GT.DELTHEMX) GO TO 1000  
      IF (DELPHI.LT.0.01.AND.DELTHE.LT.0.05) GO TO 1000
      IF (LFDC1.GT.0.AND.LFDC2.EQ.0.OR.LFDC1.EQ.0.AND.LFDC2.GT.0) THEN
        IF (ABS(ZV1-ZV2).GT.DELZMXF) GO TO 1000 
      END IF
      IF (ABS(ZV1-ZV2).GT.DELZMX) THEN
        IF (LFDC1.GT.0.AND.LFDC2.GT.0.AND.LVTX1.LE.0.AND.LVTX2.LE.0) 
     + THEN
          GO TO 900 
        ELSE
          GO TO 1000
        END IF
      END IF
      XYZ(1)=XV
      XYZ(2)=YV
      XYZ(3)=(ZV1+ZV2)/2.
C
C  Require VTX tracks if in VTX volume (due to low VTX efficiency don't do it)
C
C     IF (SQRT(XV**2+YV**2).LT.3..AND.ABS(XYZ(3)).LT.10.) THEN
C        IF (N1.GT.1.OR.N2.GT.1) GO TO 1000
C      END IF
C
C  errors (temporary)
C
      EXYZ(1)=0.1
      EXYZ(2)=0.1
      EXYZ(3)=ABS(ZV1-ZV2)/2.
      IF (EXYZ(3).LT.1.) EXYZ(3)=1.
  900 CONTINUE
      IF (LFDC1.GT.0.AND.LFDC2.GT.0.AND.LVTX1.LE.0.AND.LVTX2.LE.0) THEN
C
C  Both tracks are FDC tracks:
C
c       DXPRIM=Q(LFDC1+7)-Q(LFDC2+7)
c       DYPRIM=Q(LFDC1+8)-Q(LFDC2+8)
c       IF (ABS(DXPRIM).GT.0.001.AND.ABS(DYPRIM).GT.0.001) THEN
c         ZV1=(Q(LFDC2+4)-Q(LFDC1+4))/DXPRIM
c         ZV2=(Q(LFDC2+5)-Q(LFDC1+5))/DYPRIM
c         IF (ABS(ZV1-ZV2).GT.DELZMX) GO TO 1000
c         XYZ(3)=(ZV1+ZV2)/2.+ZG1
c         XYZ(1)=Q(LFDC1+4)+(XYZ(3)-ZG1)*Q(LFDC1+7)
c         XYZ(2)=Q(LFDC1+5)+(XYZ(3)-ZG1)*Q(LFDC1+8)
c       END IF
C   V. Burtovoy code:
        XZ2   = Q(LFDC2 + 7)
        YZ1   = Q(LFDC1 + 8)
        XZ1   = Q(LFDC1 + 7)
        YZ2   = Q(LFDC2 + 8)
        DET   = XZ2*YZ1 - XZ1*YZ2
        IF (ABS(DET).GT.0.0) THEN
          DX0   = Q(LFDC2+4) - Q(LFDC1+4)
          DY0   = Q(LFDC2+5) - Q(LFDC1+5)
          ZV1   = ZG1 + (XZ2*DY0 - YZ2*DX0)/DET
          ZV2   = ZG2 + (XZ1*DY0 - YZ1*DX0)/DET
          XYZ(3)=(ZV1 + ZV2)/2.
          XYZ(1)= Q(LFDC1 + 4) + (XYZ(3) - ZG1)*XZ1
          XYZ(2)= Q(LFDC1 + 5) + (XYZ(3) - ZG1)*YZ1
          IF (ABS(ZV1-ZV2).GT.DELZMXF) GO TO 1000
        ELSE
          GO TO 1000
        END IF
      END IF
      CALL VZERO(STR,40)
      CALL VZERO(ETR,40)
      STR(2,1)=THE1
      STR(3,1)=PHI1
      STR(2,2)=THE2
      STR(3,2)=PHI2
      ETR(2,1)=ETHE1
      ETR(3,1)=EPHI1
      ETR(2,2)=ETHE2
      ETR(3,2)=EPHI2
      IF (CAL) THEN
        LZTRK1=GZZTRK(TRAK1)
        P1=ZENERGY(LZTRK1,0,IER) ! get track energy from the calorimeter
        IF (P1.GT.0.2.AND.IER.EQ.0) THEN
          IF (CORRECT) P1=P1*(1.+ESCALE/(P1+ESCALE))
          STR(1,1)=P1
          ETR(1,1)=SQRT((CONST1/SQRT(P1))**2+CONST2**2)*P1 ! momentum error
        ELSE
          P1=0.
        END IF
        IF (P1.LT.P1_MIN) GO TO 1000
        LZTRK2=GZZTRK(TRAK2)
        P2=ZENERGY(LZTRK2,0,IER)
        IF (P2.GT.0.2.AND.IER.EQ.0) THEN
          IF (CORRECT) P2=P2*(1.+ESCALE/(P2+ESCALE))
          STR(1,2)=P2
          ETR(1,2)=SQRT((CONST1/SQRT(P2))**2+CONST2**2)*P2
        ELSE
          P2=0.
        END IF
        ESUM=P1+P2
        IF (P1.LT.P1_MIN.OR.P2.LT.P1_MIN) GO TO 1000
        IF (ESUM.LT.PVEE_MIN) GO TO 1000
        ETA1=ALOG(TAN(THE1/2.))
        ETA2=ALOG(TAN(THE2/2.))
        DELETA=ABS(ETA1-ETA2)
        IF (DELPHI.LT.0.1 .AND. DELETA.LT.0.1 .AND. ABS(P1-P2).LT.0.
     &    0001)THEN
          STR(1,1)=0.
          STR(1,2)=0.
          ETR(1,1)=0.
          ETR(1,2)=0.
          STR(1,3)=ESUM/2.
          ETR(1,3)=SQRT((CONST1/SQRT(ESUM))**2+CONST2*2)*ESUM
        END IF
      END IF
      CALL VEE3D(TRAK1,TRAK2,PHI1,PHI2,THE1,THE2,XYZ,EXYZ,PRIM,EPRIM)
 1000 RETURN
      END

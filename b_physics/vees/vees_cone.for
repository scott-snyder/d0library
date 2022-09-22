      SUBROUTINE VEES_CONE(PHI0,DPHI0,THE0,DTHE0)
C------------------------------------------------------------------
C 
C  Find all vee's (K0 or Lambda decays) in given cone; 
C  Store them in banks VERT and PVES.
C 
C  Daria Zieminska 13-DEC-1991
C  Updated 24-MAR-2004 sss - compile with g77.
C                            
C------------------------------------------------------------------
      IMPLICIT NONE 
      INTEGER NTRACK,IZTRK1,IZTRK2,NPRIM,PRUNIT,USUNIT
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZISAE.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISV2.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISP2.LINK/LIST'
      INCLUDE 'D0$INC:PI.DEF/LIST'
      INCLUDE 'D0$INC:VEEKIN.INC/LIST'
      INTEGER ICALL,IER,ICONT(10)
      REAL PHI0,DPHI0,THE0,DTHE0,B1,B2,ZIMPACT,W1,W2,THE2,PHI1,PHI2
      INTEGER NT,N1,N2,NHIT1,NHIT2,I1,I2
      INTEGER NISA1(5,2), NISA2(5,2),WISA1(5),WISA2(5) 
      INTEGER LVTX1,LVTX2,LCDC1,LCDC2,LFDC1,LFDC2,NTRA1,NTRA2 
      INTEGER LZTRK1,LZTRK2,ISTAT2(4),GZZTRK
      INTEGER NUM,LISV2,LISP2
      CHARACTER*8 NAME,LABEL,NAMT
      CHARACTER*8 PART,MI(2)
      INTEGER TRAK1,TRAK2,J1,J2,K1,K2,N,I,LOC,GZISV1
      INTEGER RUN,ID,NISAE(30),NP,NDET(30),NIZT(30),NISAV(15),IFDC
      REAL VEX(15,3),THEVE(15),PHIVE(15),EVXE(15,3),RR,RRR
      REAL ETAE(30),PHIE(30),PHIIE(30),THEIE(30),R1,R2,MOMI(30)
      REAL PDEC,PZDEC,ETADEC,PTDEC,TANTHE1,TANTHE2,XYZI(3),TANTHE
      REAL ETA,ETA1,ETAI(2),THE1,THEI(2),PHII(2),EI(2),ETA2
      REAL ZPRIM(5),DZPRIM(5),ZPRIMI,PXDEC,PYDEC,THEDEC,ATAN2,PHIDEC
      REAL V1(3),DC1(3),V2(3),DC2(3),VX(3),EVX(3),PHI,THE
      REAL PRIM(3),EPRIM(3),THEE(30),ETHE(30),EPHI(30)
      REAL PAR(4),DTDX,DTDY,DPDX,DPDY,COV(4,4),DRDZ
      REAL EPHI1,EPHI2,ETHE1,ETHE2,DXYZ(3),XYZ(3)
      REAL DPHI1,DPHI2,DTHE1,DTHE2,DPHI,DTHE
      INTEGER LVERT1,LVERT2,NP1,NP2,NP3
      LOGICAL CAL,CORRECT,KSKL, LAM, ALAM
      REAL DELPHIMX,DELPHIMX1,DELPHIMX2
      REAL DELTHEMX,DELTHE,DELETA,DELPHI
      REAL CONST1,CONST2,ESCALE,PVEE_MIN,P1_MIN,P1,P2,ESUM,ZENERGY
      REAL PHITRK,THETRK,DMOM1,DMOM2
      SAVE ICALL
      DATA ICALL/0/
      IF (ICALL.EQ.0) THEN
        PRUNIT=USUNIT()
        CALL EZPICK('VEES_RCP')
C        CALL EZGET('DELZMX',DELZMX,IER)
C        CALL EZGET('DELPHIMX1',DELPHIMX1,IER)
C        CALL EZGET('DELPHIMX2',DELPHIMX2,IER)
C        CALL EZGET('DELTHEMX',DELTHEMX,IER)
C        CALL EZGET('IMPMIN',IMPMIN,IER)
        CALL EZGET_l(  'CALL_ZENERGY', CAL,IER)
        CALL EZGET_l(  'CORRECT_ENERGY', CORRECT,IER)
        CALL EZGET(  'CONST1', CONST1,IER)
        CALL EZGET(  'CONST2', CONST2,IER)
        CALL EZGET(  'ESCALE', ESCALE,IER)
        CALL EZGET(  'PVEE_MIN', PVEE_MIN,IER)
        CALL EZGET(  'P1_MIN', P1_MIN,IER)
        CALL EZGET( 'PHITRK', PHITRK, IER )
        CALL EZGET( 'THETRK', THETRK, IER )
        CALL EZRSET
        ICALL=1
        CALL HBOOK1(4,' W1 AND W2 ', 105,0.,105.0,0.)
        CALL HBOOK1(5,' MOM  K0i MES', 100, 0.00, 50.0, 0.)
        CALL HBOOK1(105,' PT K0i MES', 40, 0.00,   8.0, 0.)
        CALL HBOOK1(106,'ETA K0i MES', 100,-8.00,  8.0, 0.)
        CALL HBOOK1(6,' MOM  LAi MES', 50, 0.00,  80.0, 0.)
        CALL HBOOK1(205,' PT LAi MES', 16, 0.00,   4.0, 0.)
        CALL HBOOK1(206,'ETA LAi MES', 100,-8.00,  8.0, 0.)
        CALL HBOOK1(50,'Phi-PhiISA track',1000, -3.2, 3.2, 0.)
        CALL HBOOK1(51,'The-TheISA track',1000, -3.2, 3.2, 0.)
        CALL HBOOK1(52,'Phi-PhiISA Vee',   500, -3.2, 3.2, 0.)
        CALL HBOOK1(53,'The-TheISA Vee',   500, -3.2, 3.2, 0.)
        CALL HBOOK1(54,'EPHI track',  500, 0., 0.1, 0.)
        CALL HBOOK1(55,'ETHE track', 1000, 0., 0.2, 0.)
        CALL HBOOK1(60,'DMOM-CAL', 100, -2.0, 2.0, 0.)
        CALL HBOOK1(71,'Xold - Xi', 200, -100.0, 100.0, 0.)
        CALL HBOOK1(72,'Yold - Yi', 200, -100.0, 100.0, 0.)
        CALL HBOOK1(73,'Zold - Zi', 400, -200.0, 200.0, 0.)
        CALL HBOOK1(81,'Xnew - Xi', 200, -100.0, 100.0, 0.)
        CALL HBOOK1(82,'Ynew - Yi', 200, -100.0, 100.0, 0.)
        CALL HBOOK1(83,'Znew - Zi', 400, -200.0, 200.0, 0.)
      END IF

      CALL EVNTID(RUN,ID)
      CALL ZVERTE(NPRIM,ZPRIM,DZPRIM)
      LOC    = GZISV1()
      ZPRIMI = Q(LOC + 9)
      WRITE(PRUNIT,999) RUN,ID,NPRIM,ZPRIMI,(ZPRIM(I),I=1,NPRIM)
      PRIM(1)=0.
      PRIM(2)=0.
      EPRIM(1)=0.
      EPRIM(2)=0.
      IF (NPRIM.GT.0) THEN
        PRIM(3)  =  ZPRIM(1)
        EPRIM(3) = DZPRIM(1)
      ELSE
        PRIM(3)  =  ZPRIMI
        EPRIM(3) =  0.2
      END IF

      CALL GTZTRH(ICONT)
      NTRACK=ICONT(2)
      IF (NTRACK.LT.2) GO TO 1000
C
C  Double loop over central tracks (ZTRK banks)
C
C      CALL CHDCDA
      CALL ISATID

C      RETURN

      NP = 0
      CALL VZERO_i(NISAE,30)
      CALL VZERO_i(NISAV,15)
      CALL VZERO_i(NDET, 30)
      CALL VZERO_i(NIZT, 30)
      CALL VZERO(ETAE, 30)
      CALL VZERO(PHIE, 30)
      CALL VZERO(PHIIE,30)
      CALL VZERO(MOMI,30)
      CALL VZERO(THEIE,30)
      CALL VZERO(VEX,45)
      CALL VZERO(EVXE,45)
      CALL VZERO(THEVE,15)
      CALL VZERO(PHIVE,15)
      CALL VZERO(STR,40)
      CALL VZERO(ETR,40)

      DO 100 IZTRK1=1,NTRACK-1
        LZTRK1=GZZTRK(IZTRK1)   
        IF(LZTRK1 .LE. 0)        GO TO 100  ! Bank doesn't exist 
        B1 = ZIMPACT(LZTRK1,0,N1)
        IF (N1 .EQ. 1) THEN  ! use the VTX component if available
          LVTX1 = LQ(LZTRK1-6)          ! Ref. link to VTX track
          CALL VTRKAS(LVTX1, NISA1, WISA1, NHIT1, NTRA1)
          IF( NTRA1 .EQ. 0)      GO TO 100
          PHI1=Q(LVTX1+6)
          THE1=Q(LVTX1+9)
	  IF(THE1 .EQ. 0.0)      GO TO 100
          V1(1)=Q(LVTX1 + 7)
          V1(2)=Q(LVTX1 + 8)
          V1(3)=Q(LVTX1 +11) - Q(LVTX1 + 10)/TAN(THE1)
          EPHI1=Q(LVTX1+16)
          IF(EPHI1 .LT. 0.0) EPHI1 = EPHI1 + TWOPI
          ETHE1=Q(LVTX1+18)
        ELSE IF(N1.EQ.2) THEN
          LCDC1=LQ(LZTRK1-7)          ! Ref. link to CDC track
          CALL DTRKAS(LCDC1, NISA1, WISA1, NHIT1, NTRA1)
          IF( NTRA1 .EQ. 0)      GO TO 100
          PHI1=Q(LCDC1+6)
          THE1=Q(LCDC1+9)
	  IF(THE1 .EQ. 0.0)      GO TO 100
          V1(1) = Q(LCDC1 + 7)
          V1(2) = Q(LCDC1 + 8)
          V1(3) = Q(LCDC1 +11)
          EPHI1 = Q(LCDC1 +16)
          IF(EPHI1 .LT. 0.0) EPHI1 = EPHI1 + TWOPI
          ETHE1 = Q(LCDC1 +18)
        ELSE IF(N1.EQ.3) THEN
          LFDC1=LQ(LZTRK1-8)          ! Ref. link to FDC track
          CALL FTRKAS(LFDC1, NISA1, WISA1, NHIT1, NTRA1)
          IF( NTRA1 .EQ. 0)      GO TO 100
          PHI1=Q(LFDC1+6)
          THE1=Q(LFDC1+22)
	  IF(THE1 .EQ. 0.0)      GO TO 100
          V1(1) = Q(LFDC1 + 4)
          V1(2) = Q(LFDC1 + 5)
          IFDC  =IQ(LFDC1 - 5)
          CALL FGETZ0(IFDC, V1(3))
C         EPHI1 = Q(LFDC1 + 23)
          PAR(3) = Q(LFDC1+7)
          PAR(4) = Q(LFDC1+8)
          COV(3,3) = Q(LFDC1+16)
          COV(3,4) = Q(LFDC1+17)
          COV(4,4) = Q(LFDC1+18)
      DPDX = 1./(1.+(PAR(4)/PAR(3))**2) * (-1.*PAR(4)/PAR(3)**2)
      DPDY = 1./(1.+(PAR(4)/PAR(3))**2) * (1./PAR(3))
      EPHI1=    DPDX**2 * COV(3,3) +
     &          DPDY**2 * COV(4,4) +
     &          2.*DPDX*DPDY * COV(3,4)
          IF ( EPHI1 .LT. 0. ) THEN
            EPHI1 = 0.
            WRITE(PRUNIT,312) 
            write (*,312 )
          ENDIF
          EPHI1 = SQRT(EPHI1)
C         ETHE1 = Q(LFDC1+24)
          DRDZ  = SQRT(PAR(3)**2 + PAR(4)**2)
          DTDX = PAR(3)/(DRDZ*(1.+DRDZ**2))
          DTDY = PAR(4)/(DRDZ*(1.+DRDZ**2))
          ETHE1=    DTDX**2 * COV(3,3) +
     &              DTDY**2 * COV(4,4) +
     &            2.*DTDX*DTDY * COV(3,4)
          IF (ETHE1 .LT. 0.) ETHE1 = 0.
          ETHE1 = SQRT(ETHE1)
        ELSE 
          GO TO 100
        END IF
        LVERT1 = LQ(LZTRK1 - 2)
        IF (LVERT1 .GT. 0) THEN 
          V1(1) = Q(LVERT1 + 3)
          V1(2) = Q(LVERT1 + 4)
          V1(3) = Q(LVERT1 + 5)
        END IF
        DC1(1) = SIN(THE1) * COS(PHI1)
        DC1(2) = SIN(THE1) * SIN(PHI1)
        DC1(3) = COS(THE1)
          EPHI1 = EPHI1*PHITRK
          ETHE1 = ETHE1*THETRK

        DO 200 IZTRK2=IZTRK1+1,NTRACK
          LZTRK2 = GZZTRK(IZTRK2)
          IF(LZTRK2 .LE. 0)         GO TO 200 
          B2    = ZIMPACT(LZTRK2,0,N2)
          IF(N2 .EQ. 1) THEN
            LVTX2=LQ(LZTRK2-6)
            CALL VTRKAS(LVTX2, NISA2, WISA2, NHIT2, NTRA2)
            IF( NTRA2 .EQ. 0)       GO TO 200
            PHI2=Q(LVTX2+6)
            THE2=Q(LVTX2+9)
            IF(THE2 .EQ. 0.0)      GO TO 200
            V2(1)=Q(LVTX2 + 7)
            V2(2)=Q(LVTX2 + 8)
            V2(3)=Q(LVTX2 +11) - Q(LVTX2 + 10)/TAN(THE2)
            EPHI2=Q(LVTX2+16)
            IF(EPHI2 .LT. 0.0) EPHI2 = EPHI2 + TWOPI
            ETHE2=Q(LVTX2+18)
          ELSE IF(N2.EQ.2) THEN
            LCDC2=LQ(LZTRK2-7)
            CALL DTRKAS(LCDC2, NISA2, WISA2, NHIT2, NTRA2)
            IF( NTRA2 .EQ. 0)       GO TO 200
            PHI2=Q(LCDC2+6)
            THE2=Q(LCDC2+9)
            IF(THE2 .EQ. 0.0)      GO TO 200
            V2(1) = Q(LCDC2 + 7)
            V2(2) = Q(LCDC2 + 8)
            V2(3) = Q(LCDC2 +11)
            EPHI2=Q(LCDC2+16)
            IF(EPHI2 .LT. 0.0) EPHI2 = EPHI2 + TWOPI
            ETHE2=Q(LCDC2+18)
          ELSE IF(N2.EQ.3) THEN
            LFDC2=LQ(LZTRK2-8)
            CALL FTRKAS(LFDC2, NISA2, WISA2, NHIT2, NTRA2)
            IF( NTRA2 .EQ. 0)       GO TO 200
            PHI2=Q(LFDC2+6)
            THE2=Q(LFDC2+22)
            IF(THE2 .EQ. 0.0)      GO TO 200
            V2(1) = Q(LFDC2 + 4)                 
            V2(2) = Q(LFDC2 + 5)
            IFDC  =IQ(LFDC2 - 5)
            CALL FGETZ0(IFDC, V2(3))
C           EPHI2=Q(LFDC2+23)
            PAR(3) = Q(LFDC2+7)
            PAR(4) = Q(LFDC2+8)
            COV(3,3) = Q(LFDC2+16)
            COV(3,4) = Q(LFDC2+17)
            COV(4,4) = Q(LFDC2+18)
      DPDX = 1./(1.+(PAR(4)/PAR(3))**2) * (-1.*PAR(4)/PAR(3)**2)
      DPDY = 1./(1.+(PAR(4)/PAR(3))**2) * (1./PAR(3))
      EPHI2=    DPDX**2 * COV(3,3) +
     &          DPDY**2 * COV(4,4) +
     &          2.*DPDX*DPDY * COV(3,4)
            IF ( EPHI2 .LT. 0. ) THEN
              EPHI2 = 0.
              WRITE(PRUNIT,314) 
              write (*,314)
            ENDIF
            EPHI2 = SQRT(EPHI2)
C           ETHE2=Q(LFDC2+24)
        DRDZ=SQRT(PAR(3)**2 + PAR(4)**2)
        DTDX = PAR(3)/(DRDZ*(1.+DRDZ**2))
        DTDY = PAR(4)/(DRDZ*(1.+DRDZ**2))
        ETHE2=    DTDX**2 * COV(3,3) +
     &            DTDY**2 * COV(4,4) +
     &            2.*DTDX*DTDY * COV(3,4)
            IF (ETHE2 .LT. 0.) ETHE2 = 0.
            ETHE2 = SQRT(ETHE2)
          ELSE 
            GO TO 200
          END IF
          LVERT2 = LQ(LZTRK2 - 2)
          IF (LVERT2 .GT. 0) THEN 
            V2(1) = Q(LVERT2 + 3)
            V2(2) = Q(LVERT2 + 4)
            V2(3) = Q(LVERT2 + 5)
          END IF
          DC2(1) = SIN(THE2) * COS(PHI2)
          DC2(2) = SIN(THE2) * SIN(PHI2)
          DC2(3) = COS(THE2)
          EPHI2 = EPHI2*PHITRK
          ETHE2 = ETHE2*THETRK

      IF(NTRA1 .GE. 1 .AND. NTRA2 .GE. 1)           THEN 
        DO   79 I1 = 1, NTRA1
          DO 79 I2 = 1, NTRA2
            IF(ABS(NISA1(I1,1)-NISA2(I2,1) ) .EQ. 1 .AND.
     &        NISA1(I1,2) .EQ. NISA2(I2,2) ) THEN 
              W1 = 100.0*FLOAT(WISA1(I1))/FLOAT(NHIT1)
              W2 = 100.0*FLOAT(WISA2(I2))/FLOAT(NHIT2)
              CALL HFILL(4, W1,0.,1.)
              CALL HFILL(4, W2,0.,1.)
              IF( W1 .GE. 50.0 .AND. W2 .GE. 50.0 ) THEN
                CALL ASSA
                NUM   = NUM + 1
                LISV2 = NISA1(I1,2)
                PART  = LABEL(IQ(LISV2+1))
                PXDEC = Q(LISV2 + 2)
                PYDEC = Q(LISV2 + 3)
                PZDEC = Q(LISV2 + 4)
                PDEC  = Q(LISV2 + 5)
                PTDEC = SQRT( PDEC**2 - PZDEC**2 )
                THEDEC= ATAN2(SQRT(PXDEC**2 + PYDEC**2),PZDEC)
                PHIDEC= ATAN2(PYDEC,PXDEC)
                IF(PHIDEC .LT. 0.0) PHIDEC = PHIDEC + TWOPI
                ETADEC= - ALOG( TAN( THEDEC/2. ) ) 
                XYZI(1) = Q(LISV2 + 7)
                XYZI(2) = Q(LISV2 + 8)
                XYZI(3) = Q(LISV2 + 9)
                NT   = 0
                LISP2   = LQ(LISV2-IZISP2)
  2             IF(LISP2 .GT. 0 ) THEN
                  NAMT  = LABEL(IQ(LISP2 + 1))
            KSKL  = (PART .EQ. 'KS'  .OR. PART .EQ. 'KL') .AND.
     &              (NAMT .EQ. 'PI+' .OR. NAMT .EQ. 'PI-')
            LAM   =  PART .EQ.  'L' .AND.
     &              (NAMT .EQ.  'P'  .OR. NAMT .EQ. 'PI-')
            ALAM  =  PART .EQ.  'AL' .AND.
     &              (NAMT .EQ.  'AP' .OR. NAMT .EQ. 'PI+')
                  IF( KSKL .OR. LAM .OR. ALAM ) THEN
                    IF(NISA1(I1,1) .EQ. IQ(LISP2 - 5) ) THEN
                      NT = 1
                    ELSE
                      NT = 2
                    ENDIF  
                    EI(NT)    =  SQRT(Q(LISP2+2)**2 + Q(LISP2+3)**2 +   
     &                                Q(LISP2+4)**2) 
                    MI(NT)    =  NAMT
                    PHII(NT)  =  Q(LISP2 + 7)
                    IF(PHII(NT) .LT. 0.0) PHII(NT) = PHII(NT) + TWOPI
                    THEI(NT)  =  Q(LISP2 + 8)
                    ETAI(NT)  =  Q(LISP2 + 9)
                    LISP2 = LQ(LISP2)
                    GO TO 2
                  ENDIF
                ENDIF
                CALL VERTEX_VEE(PRIM,V1,DC1,V2,DC2,VX,EVX,PHI,THE,IER)
                IF(IER .NE. 0) WRITE(PRUNIT,444) IER
                ETA     = - 10.0
                ETA1    = - 10.0
                ETA2    = - 10.0
                TANTHE  = TAN(  THE/2. ) 
                TANTHE1 = TAN( THE1/2. ) 
                TANTHE2 = TAN( THE2/2. ) 
                IF(TANTHE  .GT. 0.0) ETA     = - ALOG( TANTHE  ) 
                IF(TANTHE1 .GT. 0.0) ETA1    = - ALOG( TANTHE1 ) 
                IF(TANTHE2 .GT. 0.0) ETA2    = - ALOG( TANTHE2 )  
                WRITE(PRUNIT,887) NUM
                WRITE(PRUNIT,888)N1,IZTRK1,NISA1(I1,1),WISA1(I1),NHIT1,
     &          ETA1,ETAI(1),THE1,THEI(1),PHI1,PHII(1),EI(1),MI(1)
                WRITE(PRUNIT,888)N2,IZTRK2,NISA2(I2,1),WISA2(I2),NHIT2,
     &          ETA2,ETAI(2),THE2,THEI(2),PHI2,PHII(2),EI(2),MI(2)
                WRITE(PRUNIT,777) ETA,ETADEC,THE,THEDEC,PHI,PHIDEC,
     &                            PDEC,PART,(XYZI(I),I=1,3)
                WRITE(PRUNIT,778) (VX(I),I=1,3)
                write (*,887) NUM
                write (*,888) N1,IZTRK1,NISA1(I1,1),WISA1(I1),NHIT1,
     &          ETA1,ETAI(1),THE1,THEI(1),PHI1,PHII(1),EI(1),MI(1)
                write (*,888), N2,IZTRK2,NISA2(I2,1),WISA2(I2),NHIT2,
     &          ETA2,ETAI(2),THE2,THEI(2),PHI2,PHII(2),EI(2),MI(2)
                write (*,777) ETA,ETADEC,THE,THEDEC,PHI,PHIDEC,
     &                            PDEC,PART,(XYZI(I),I=1,3)
                write (*,778) (VX(I),I=1,3)
                NP1 = NP + 1
                NP2 = NP + 2
                NP3 = NP/2 + 1
                NDET(NP1)  = N1
                NDET(NP2)  = N2
                NIZT(NP1)  = IZTRK1
                NIZT(NP2)  = IZTRK2
                NISAE(NP1) = NISA1(I1,1)
                NISAE(NP2) = NISA2(I2,1)
                ETAE(NP1)  = ETA1
                ETAE(NP2)  = ETA2
                PHIE(NP1)  = PHI1
                PHIE(NP2)  = PHI2
                MOMI(NP1)  = EI(1)
                MOMI(NP2)  = EI(2)
                PHIIE(NP1) = PHII(1)
                PHIIE(NP2) = PHII(2)
                THEIE(NP1) = THEI(1)
                THEIE(NP2) = THEI(2)
                THEE(NP1)  = THE1
                THEE(NP2)  = THE2
                ETHE(NP1)  = ETHE1
                ETHE(NP2)  = ETHE2
                EPHI(NP1)  = EPHI1
                EPHI(NP2)  = EPHI2
                NISAV(NP3) = NISA1(I1,2)
                VEX(NP3,1) = VX(1) 
                VEX(NP3,2) = VX(2) 
                VEX(NP3,3) = VX(3) 
                EVXE(NP3,1) = EVX(1) 
                EVXE(NP3,2) = EVX(2) 
                EVXE(NP3,3) = EVX(3) 
                THEVE(NP3) = THE 
                PHIVE(NP3) = PHI
                NP         = NP + 2
              ENDIF
            ENDIF
  79    CONTINUE
      ENDIF
  200   CONTINUE
  100 CONTINUE 
      IF(NP .GT. 2) THEN
        DO 300 I1 = 1, NP-2
          IF(NIZT(I1) .GT. 0) THEN
            J1 = I1/2
            IF(I1 .NE. 2*J1) J1 = J1 + 1
	    K1 = 2*J1
            RR = PHIE(K1-1) - PHIIE(K1-1) 
            IF(ABS(RR) .GT. PI) RR = TWOPI - ABS(RR)
            RRR= PHIE(K1)   - PHIIE(K1) 
            IF(ABS(RRR) .GT. PI) RRR = TWOPI - ABS(RRR)
            R1   = (ETAE(K1-1) + ALOG(TAN(THEIE(K1-1)/2.)) )**2 +
     &              RR**2               +   
     &             (ETAE(K1)   + ALOG(TAN(THEIE(K1  )/2.)) )**2 +
     &              RRR**2
            DO 400 I2 = I1+1, NP
              IF(NISAE(I1) .EQ. NISAE(I2) .AND. NIZT(I2) .GT. 0) THEN
                J2 = I2/2
                IF(I2 .NE. 2*J2) J2 = J2 + 1
	        K2 = 2*J2
                RR = PHIE(K2-1) - PHIIE(K2-1)
                IF(ABS(RR) .GT. PI) RR = TWOPI - ABS(RR)
                RRR= PHIE(K2)   - PHIIE(K2) 
                IF(ABS(RRR) .GT. PI) RRR = TWOPI - ABS(RRR)
          R2 = (ETAE(K2-1) + ALOG(TAN(THEIE(K2-1)/2.)) )**2 +
     &          RR**2               +   
     &         (ETAE(K2)   + ALOG(TAN(THEIE(K2  )/2.)) )**2 +
     &          RRR**2
                IF(R1 .LT. R2) THEN
                  NIZT(K2-1)  = -10
                  NIZT(K2  )  = -10
                ELSE
                  NIZT(K1-1)  = -10
                  NIZT(K1  )  = -10
                  GO TO 300
                ENDIF
              ENDIF
  400       CONTINUE 
          ENDIF
  300   CONTINUE 
        WRITE(PRUNIT,334) (NIZT(N), N=1,NP)
        write (*,334) (NIZT(N), N=1,NP)
      ENDIF
      IF(NP .GT. 0) THEN
        DO 500 I1 = 1, NP/2
          J1 = 2*I1
          IF(NIZT(J1) .GT. 0) THEN
            LISV2 = NISAV(I1)
            PART  = LABEL(IQ(LISV2+1))
            PXDEC = Q(LISV2 + 2)
            PYDEC = Q(LISV2 + 3)
            PZDEC = Q(LISV2 + 4)
            PDEC  = Q(LISV2 + 5)
            XYZI(1) = Q(LISV2 + 7)
            XYZI(2) = Q(LISV2 + 8)
            XYZI(3) = Q(LISV2 + 9)
            PTDEC = SQRT( PDEC**2 - PZDEC**2 )
            THEDEC= ATAN2(SQRT(PXDEC**2 + PYDEC**2),PZDEC)
            PHIDEC= ATAN2(PYDEC,PXDEC)
            IF(PHIDEC .LT. 0.0) PHIDEC = PHIDEC + TWOPI
            ETADEC= - ALOG( TAN( THEDEC/2. ) ) 
            IF(PART .EQ. 'KS'  .OR. PART .EQ. 'KL') THEN
              CALL HFILL(  5,  PDEC,0.,1.)
              CALL HFILL(105, PTDEC,0.,1.)
              CALL HFILL(106,ETADEC,0.,1.)
            ENDIF
            IF(PART .EQ.  'L' .OR. PART .EQ.  'AL') THEN
              CALL HFILL(  6,  PDEC,0.,1.)
              CALL HFILL(205, PTDEC,0.,1.)
              CALL HFILL(206,ETADEC,0.,1.)
            ENDIF
            CALL VZERO(STR,40)
            CALL VZERO(ETR,40)
            VX(1) = VEX(I1,1)
            VX(2) = VEX(I1,2)
            VX(3) = VEX(I1,3)
            EVX(1)= EVXE(I1,1)
            EVX(2)= EVXE(I1,2)
            EVX(3)= EVXE(I1,3)
            CALL VSUB(VX,PRIM,DXYZ,3)
            STR(2,1) = THEE(J1-1)
            STR(3,1) = PHIE(J1-1)
            STR(2,2) = THEE(J1)
            STR(3,2) = PHIE(J1)
            ETR(2,1) = ETHE(J1-1)
            ETR(3,1) = EPHI(J1-1)
            ETR(2,2) = ETHE(J1)
            ETR(3,2) = EPHI(J1)
	    DPHI1    = PHIE(J1-1) - PHIIE(J1-1)
            IF(DPHI1 .GT. PI) DPHI1 = TWOPI - DPHI1
            IF(DPHI1 .LT.-PI) DPHI1 =-TWOPI - DPHI1
            IF(ABS(DPHI1) .GT. 0.1)                     GO TO 499
	    DPHI2    = PHIE(J1)   - PHIIE(J1)
            IF(DPHI2 .GT. PI) DPHI2 = TWOPI - DPHI2
            IF(DPHI2 .LT.-PI) DPHI2 =-TWOPI - DPHI2
            IF(ABS(DPHI2) .GT. 0.1)                     GO TO 499
            DTHE1    = THEE(J1-1) - THEIE(J1-1)
            IF(ABS(DTHE1) .GT. 0.1)                     GO TO 499
            DTHE2    = THEE(J1)   - THEIE(J1)
            IF(ABS(DTHE2) .GT. 0.1)                     GO TO 499
            DPHI     = PHIVE(I1)  - PHIDEC
            IF(DPHI .GT. PI) DPHI = TWOPI - DPHI
            IF(DPHI .LT.-PI) DPHI =-TWOPI - DPHI
            IF(ABS(DPHI) .GT. 0.3)                     GO TO 499
            DTHE     = THEVE(I1)  - THEDEC
            IF(ABS(DTHE) .GT. 0.3)                     GO TO 499
            CALL HFILL(50,  DPHI1, 0.,1.)
            CALL HFILL(50,  DPHI2, 0.,1.)
            CALL HFILL(51,  DTHE1, 0.,1.)
            CALL HFILL(51,  DTHE2, 0.,1.)
            CALL HFILL(52,   DPHI, 0.,1.)
            CALL HFILL(53,   DTHE, 0.,1.)
            CALL HFILL(54,  EPHI(J1-1), 0.,1.)
            CALL HFILL(54,  EPHI(J1),   0.,1.)
            CALL HFILL(55,  ETHE(J1-1), 0.,1.)
            CALL HFILL(55,  ETHE(J1),   0.,1.)
C            CALL VEE(NIZT(J1-1),NIZT(J1),
C     &      PHI0,DPHI0,THE0,DTHE0,NDET(J1-1),NDET(J1),XYZ)
            CALL VSUB(XYZ,XYZI,DXYZ,3)
            CALL HFILL(71, DXYZ(1), 0.,1.)
            CALL HFILL(72, DXYZ(2), 0.,1.)
            CALL HFILL(73, DXYZ(3), 0.,1.)
            CALL VSUB(VX,XYZI,DXYZ,3)
            CALL HFILL(81, DXYZ(1), 0.,1.)
            CALL HFILL(82, DXYZ(2), 0.,1.)
            CALL HFILL(83, DXYZ(3), 0.,1.)

C            go to 499

            IF (CAL) THEN
              LZTRK1 = GZZTRK(NIZT(J1-1))
              P1     = ZENERGY(LZTRK1,0,IER)
              IF (P1.GT.0.2.AND.IER.EQ.0) THEN
                IF (CORRECT) P1=P1*(1.+ESCALE/(P1+ESCALE))
                STR(1,1) = P1
                ETR(1,1) = SQRT((CONST1/SQRT(P1))**2+CONST2**2)*P1
              ELSE
                P1=0.
              END IF
C             IF(P1 .LT. P1_MIN)               GO TO 500
              LZTRK2 = GZZTRK(NIZT(J1))
              P2     = ZENERGY(LZTRK2,0,IER) 
              IF (P2.GT.0.2.AND.IER.EQ.0) THEN
                IF (CORRECT) P2=P2*(1.+ESCALE/(P2+ESCALE)) 
                STR(1,2) = P2
                ETR(1,2)=SQRT((CONST1/SQRT(P2))**2+CONST2**2)*P2
              ELSE
                P2=0.
              ENDIF
              ESUM=P1+P2
C             IF(P1.LT.P1_MIN.OR.P2.LT.P1_MIN) GO TO 500
C             IF(ESUM .LT. PVEE_MIN)           GO TO 500
C             DELETA=ABS( ETAE(J1) - ETAE(J1-1) )
C             DELPHI=ABS( PHIE(J1) - PHIE(J1-1) )
C             IF (DELPHI.GT.PI) DELPHI=TWOPI-DELPHI
C              IF (DELPHI.GT.DELPHIMX)          GO TO 500  
C              IF(DELPHI.LT.0.1.AND.DELETA.LT.0.1 .AND. ESUM .GT. 0.)THEN
C              IF (DELPHI.LT.0.1.AND.DELETA.LT.0.1) THEN
C                STR(1,1)=0.
C                STR(1,2)=0.
C                ETR(1,1)=0.
C                ETR(1,2)=0.
C                STR(1,3)=ESUM/2.
C                ETR(1,3)=SQRT((CONST1/SQRT(ESUM))**2+CONST2*2)*ESUM
C              END IF
              DMOM1 = (MOMI(J1-1) - P1)/MOMI(J1-1)
              DMOM2 = (MOMI(J1)   - P2)/MOMI(J1)
              CALL HFILL(60,  DMOM1, 0.,1.)
              CALL HFILL(60,  DMOM2, 0.,1.)
            END IF
C      CALL VEE3D(TRAK1,TRAK2,PHI1,PHI2,THE1,THE2,XYZ,EXYZ,PRIM,EPRIM)
            CALL VEEFIT_NEW(NIZT(J1-1),NIZT(J1),VX,EVX,DXYZ,PHIE(J1-1),
     &      PHIE(J1),THEE(J1-1),THEE(J1),THEVE(I1),PHIVE(I1),LISV2)
 499        CONTINUE
          ENDIF
  500   CONTINUE
      ENDIF
 1000 CONTINUE 
      RETURN
 887  FORMAT(/,15X,'NUM = ',I4,/,/,1X,
     &'   DET TRACK N_ISA W_ISA  NHIT       ETA     ETA_I       THE',
     &'     THE_I       PHI     PHI_I     MOM_I     MASS_I')
 888  FORMAT(1X,5I6,7F10.5,3X,A10)
 901  FORMAT(1X,F6.4,I5,2F9.5,4I7,/)
 999  FORMAT(/,1X,'  RUN =',I5,'  ID =',I5,' NPRIM =',I2,
     &' ZPRIM_I =',F10.5,' ZPRIM =',5F10.5,/,/)
 333  FORMAT(1X,'  &&&&&&   NTRA1 =',I2,'  NTRA2 =',I2,/)
 334  FORMAT(1X,'  NIZT =',20I5)
 777  FORMAT(31X,7F10.5,3X,A10,/,/
     &1X,'    VX        VY        VZ    ',/,3F10.5,
     &'  --->  ISA vertex')
 778  FORMAT(3F10.5,'  --->  new  measured  vertex',/)
 444  FORMAT(1X,'   NO  VERTEX_VEE,  IER =',I5)
 312  FORMAT(1X,' &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&     EPHI1 = 0.0 ')
 314  FORMAT(1X,' &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&     EPHI2 = 0.0 ')
      END       


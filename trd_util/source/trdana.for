      SUBROUTINE TRDANA(IDENT,ITR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Handle TRD energy deposit (computed in TRCFAD)
C-
C-   Inputs  :  IDENT=0 If identified hadron
C-                   =1      ""       electron
C-              ITR  =  TRACK NUMBER
C-   Outputs :
C-   Controls:
C-
C-   Created  26-APR-1989   A. Zylberstejn
C-   Updated   4-MAR-1990   J.Fr. Glicenstein: Flags in TRD_RCP, Likelihood
C-                          Etot/cluster number,5 GeV hadron rejection
C-   Updated  26-FEB-1991   A. Zylberstejn
C-   Updated  14-MAR-1991   A. Zylberstejn   : correction to put ident in the
C-                                             TRDT bank
C-   Updated  29-JAN-1993   Alain PLUQUET   Simplification. New switches for
C-                                          efficiencies.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FADTRD.INC'
      INCLUDE 'D0$INC:FIRSHT.INC'
      INCLUDE 'D0$INC:GCONST.INC'
      INCLUDE 'D0$INC:CLUREC.INC'
      INCLUDE 'D0$INC:geomtr.INC'
      INCLUDE 'D0$INC:TRDEFF.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$INC:TRENER.INC'
      INCLUDE 'D0$INC:TRINTR.INC'
      INCLUDE 'D0$INC:worksp.INC'
      INCLUDE 'D0$INC:zebcom.INC'
      INTEGER IER,LZTRDT,GZTRDT,IERR,LOUT,TRUNIT
      INTEGER LZTRH,GZZTRH,LTTRH,LTHIT,GZTHIT
      INTEGER I,ITR,ICH,IDENT,ISHFT,J,NDFADC,NCLUST(3)
      REAL A,CLIKET,DPHI,ETOTEF,ETO3EF,ETRUEF,ETR3EF,LIKEEF,CLIKEN
      REAL ET(6),THETA,VMAX,LIKNEF
      REAL GPI,NORFAD(NBFAD),MIPTO5G
      LOGICAL DOCLUS,FIRST,DO_HISTO(4),DOPRINT,TRD_DO_PRINT
      LOGICAL EFFC,EFFC_ETOT,EFFC_ETRUNC
      LOGICAL EFFC_E_L,EFFC_E_CL_L
      INTEGER ISHGE, IHSTBK(4),ISWARR(14)
      CHARACTER*3 HSTBOK(4),SWTARR(14),SWTGEN(11)
C      EQUIVALENCE (HSTBOK,IHSTBK),(SWTARR,ISWARR),(SWTGEN,SWHISG)
      INTEGER     ISWHSP(14),ISWHSE(14),ISWHSG(12)
      CHARACTER*3 SWITCH_HISTO(14,2),SWHISG(12)
      CHARACTER*3 C3
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT=TRUNIT()
        CALL EZPICK('TRD_RCP')
        CALL EZGET('ETOTM',ETOTM,IER)
        NDFADC=TVALIN(3)
        DOCLUS=.FALSE.
        CALL EZGET('CLUSTER_RECONS',I,IER)
        CALL UHTOC(I,3,C3,3)
        IF(C3.EQ.'Y' )DOCLUS=.TRUE.
        CALL EZGET('MIP_TO_5_GEV',MIPTO5G,IER)
        CALL EZGET('HSTBOK',IWS,IERR)
        DO I=1,4
          DO_HISTO(I)=.FALSE.
          IF(IWS(I).NE.0)THEN
            CALL UHTOC(IWS(I),3,C3,3)
            DO_HISTO(I)=C3.EQ.'Y'
          END IF
        END DO
        CALL EZGET('EFF_CALCUL',EFFC,IER)
        IF (EFFC) THEN
          CALL EZGET('EFFC_ETOT',EFFC_ETOT,IER)
          CALL EZGET('EFFC_ETRUNC',EFFC_ETRUNC,IER)
          CALL EZGET('EFFC_E_L',EFFC_ETOT,IER)
          CALL EZGET('EFFC_E_CL_L',EFFC_ETOT,IER)
        ELSE
          EFFC_ETOT=.FALSE.
          EFFC_ETRUNC=.FALSE.
          EFFC_E_L=.FALSE.
          EFFC_E_CL_L=.FALSE.
        ENDIF
C        CALL EZGET('PIONS_HISTOS',SWITCH_HISTO(1,1),IER)
        IF(DO_HISTO(1))THEN
          IF(DO_HISTO(2))THEN
            CALL VZERO(IWS,14)
            CALL EZGET('PIONS_HISTOS'    ,IWS ,IER)
            DO I=1,14
              IF(IWS(I).NE.0)THEN
                CALL UHTOC(IWS(I),3,SWITCH_HISTO(I,1),3)
              END IF
            END DO
          END IF
C        CALL EZGET('ELECTRONS_HISTOS',SWITCH_HISTO(1,2),IER)
          IF(DO_HISTO(3))THEN
            CALL VZERO(IWS,14)
            CALL EZGET('ELECTRONS_HISTOS',IWS,IER)
            DO I=1,14
              IF(IWS(I).NE.0)THEN
                CALL UHTOC(IWS(I),3,SWITCH_HISTO(I,2),3)
              END IF
            END DO
          END IF
          LOUT=6
C        CALL EZGET('GENERAL_TRD_HISTOS',SWHISG(1),IER)
          CALL VZERO(IWS,14)
          IF(DO_HISTO(4))THEN
            CALL EZGET('GENERAL_TRD_HISTOS',IWS(1),IER)
            DO I=1,12
              IF(IWS(I).NE.0)THEN
                CALL UHTOC(IWS(I),4,SWHISG(I),4)
              END IF
            END DO
          END IF
        END IF
        CALL EZRSET
      END IF
      DOPRINT=TRD_DO_PRINT()
      IF(DOPRINT)WRITE(LOUT,'(a30,i3,/,a30)')' enter TRDANA track:',
     &    ITR,' ----------------'
C      IF(DOPRINT)WRITE(LOUT,*)'ltrdt',GZTRDT()
      IF(DO_HISTO(1))CALL HCDIR('//PAWC/TRD',' ')
      IF (ABS(STHETA(ITR)).LT.1.) THEN
        THETA=RADDEG*ASIN(STHETA(ITR))
      ELSE
        THETA=90.
      ENDIF
      CALL VFILL(ELEFF,8,1.)
      CALL VZERO(PIREJ,5)
      ISHFT=100*IDENT+FIRSHT
      ISHGE=500      +FIRSHT
      IF (EFFC_ETOT) THEN
        ELEFF(1)=ETOTEF(ETOTAL(1,ITR)*MIPTO5G,THETA,1)
        PIREJ(1)=ETOTEF(ETOTAL(1,ITR)*MIPTO5G,THETA,0)
      ENDIF
      IF(DOPRINT)WRITE(LOUT,*)'before effc_trunc,ltrdt',GZTRDT()
      IF (EFFC_ETRUNC) THEN
        ELEFF(3)=ETRUEF(ETRUNC(1,ITR)*MIPTO5G,THETA,1)
        PIREJ(3)=ETRUEF(ETRUNC(1,ITR)*MIPTO5G,THETA,0)
      ENDIF
      DO ICH=1,3     !LOOP ON THE 3 ANODE PLANES
        ET(ICH)=0
        IF(TRETOT(ICH,ITR).GT.ETOTM) ET(ICH)=TRETOT(ICH,ITR)*MIPTO5G
      ENDDO
      IF(ET(1)+ET(2)+ET(3).LE.0.)RETURN
      LIKET(1,ITR)=-999.
      LIKECL(1,ITR)=-999.
      IF (EFFC_E_L) THEN
        IF(ET(1)*ET(2)*ET(3).GT.0.)THEN
          LIKET(1,ITR) = CLIKET(ET,THETA)
          ELEFF(5)=LIKEEF(LIKET(1,ITR),THETA,1)
          PIREJ(5)=LIKEEF(LIKET(1,ITR),THETA,0)
        END IF
      END IF
      IF(DOPRINT)WRITE(LOUT,*)'before effc_e_l,ltrdt',GZTRDT()
      IF(NTWIRE(1,ITR).EQ.3)THEN
        IF (EFFC_ETOT) THEN
          ELEFF(2)=ETO3EF(ETOTAL(1,ITR)*MIPTO5G,THETA,1)
          PIREJ(2)=ETO3EF(ETOTAL(1,ITR)*MIPTO5G,THETA,0)
        ENDIF
        IF(DOPRINT)WRITE(LOUT,*)'before effc_etot,ltrdt',GZTRDT()
        IF (EFFC_ETRUNC) THEN
          ELEFF(4)=ETR3EF(ETRUNC(1,ITR)*MIPTO5G,THETA,1)
          PIREJ(4)=ETR3EF(ETRUNC(1,ITR)*MIPTO5G,THETA,0)
        ENDIF
      ENDIF
      IDENTR(ITR)=IDENTR(ITR)+IDENT ! changed (28/3/91)
      IF(DOPRINT)WRITE(LOUT,*)' in trdana, before trdtfil,gztrdt',
     &  GZTRDT()
      CALL TRDTFIL(ITR)
      NCREC=0
      LZTRDT=GZTRDT()
      IF(DOPRINT )THEN
        LZTRH=GZZTRH()
        WRITE(LOUT,*)' in trdana,after trdtfil gztrdt',LZTRDT,
     &    ' lztrh',LZTRH,' lttrh',LQ(LZTRH-5),' lthit',GZTHIT()
      END IF
      IF(LZTRDT.LT.20000)THEN
        CALL ERRMSG(' Zebcom bank becoming saturated','TRDANA',
     &    ' No room for TRD banks','w')
      END IF
      IF(LZTRDT.EQ.0)THEN
        CALL ERRMSG('TRD','TRDANA',' No TRDT bank','w')
        GO TO 52
      END IF
      IF(DOPRINT)WRITE(LOUT,*)'ltrdt',GZTRDT()
      DO ICH=1,3
C        CALL VSCALE(FADCTR(1,ICH,1),MIPTO5G,NORFAD,NBFAD)
C        IF(DOCLUS)CALL CLUSTF(NORFAD,NBFAD)
C        IF(NCREC.GT.0)CALL VSCALE(ECLR,1./MIPTO5G,ECLR,NCREC)
C        NCLUST(ICH) = NCREC
        IF(NBHWIR(ICH,ITR).NE.0)           CALL TPRLFIL(ICH,ITR)
      ENDDO
C      IF( EFFC_E_CL_L .AND.
C     &           NCLUST(1)+NCLUST(2)+NCLUST(3).NE.0)THEN
C        DO 50 I = 1 ,  3
C          A = CLIKEN(ET,NCLUST,I,THETA)
C          ELEFF(I+5)=LIKNEF(A,I,THETA,1)
C          PIREJ(I+5)=LIKNEF(LIKECL(1,ITR),I,THETA,0)
C          Q(LZTRDT+6+I)=A
C          IF(I.EQ.1) Q(LZTRDT+19)=ELEFF(I+5)
C   50   CONTINUE
C      END IF
C        CALL PRTPRL(LOUT,LZTRDT,0,'bid',0)
   52 CONTINUE
C  Histograms
C  ---------
      IF(DOPRINT)WRITE(LOUT,*)'ltrdt',GZTRDT()
      IF (.NOT. DO_HISTO(1))GO TO 60
      LZTRDT=GZTRDT()
      IF(LZTRDT.EQ.0)GO TO 60
C
C Plot of raw energy -not corrected for different gains- per layer
C Since the energies may have been multiplied by epicor we have to multiply
C TRETOT by EPICOT to get the un-corrected energy
C
      IF (SWITCH_HISTO(1,IDENT+1).EQ.'Y') THEN
        GPI=1.
        DO ICH=1,6
          J=ICH
          IF(ICH.GT.3)J=ICH-3
          GPI=EPICOR(J)
          IF(NBHWIR(ICH,ITR).NE.0)
     +      CALL HF1(ICH+ISHFT,TRETOT(ICH,ITR)*GPI,1.)! Energy layer ich
          IF(ICH.LE.3 .AND. NBHWIR(ICH,ITR).NE.0)THEN
C  energie anode versus energie cathode
            CALL HFILL(FIRSHT+630+ICH,
     &        TRETOT(ICH,ITR)*GPI,TRETOT(ICH+3,ITR)*GPI,1.)
            DPHI=PHITRA(ICH,ITR)/RADAN(ICH)/(TWOPI/256.)
C  energie versus position dans la cellule
            CALL HFILL(FIRSHT+650+ICH,TRETOT(ICH,ITR)*GPI,DPHI,1.)
          END IF
        END DO
      ENDIF
C Normalized energy per individal layer
      IF (SWITCH_HISTO(2,IDENT+1).EQ.'Y') THEN ! energy per individal layer
        DO ICH=1,6
          CALL HF1(ICH+6+ISHFT,TRETOT(ICH,ITR),1.)
        END DO
      ENDIF
      IF (SWITCH_HISTO(3,IDENT+1).EQ.'Y')THEN ! energy per layer
        DO ICH=1,6
          IF(NBHWIR(ICH,ITR).NE.0)
     +      CALL HF1(13+ISHFT,TRETOT(ICH,ITR),1.)
        END DO
      END IF
C      END IF
      IF (SWITCH_HISTO(4,IDENT+1).EQ.'Y')
     +   CALL HF1(25+ISHFT,ETOTAL(1,ITR),1.) !TOTAL ENERGY
      IF (SWITCH_HISTO(5,IDENT+1).EQ.'Y')
     +   CALL HF1(27+ISHFT,ETRUNC(1,ITR),1.) !TRUNCATED MEAN
      IF (SWITCH_HISTO(6,IDENT+1).EQ.'Y')
     +   CALL HF1(29+ISHFT,LIKET(1,ITR),1.)  !LIKELIHOOD ETOT
      IF (SWITCH_HISTO(7,IDENT+1).EQ.'Y')
     +   CALL HF1(ISHFT+31,ELEFF(1),1.)      ! efficiency E tot
      IF (SWITCH_HISTO(10,IDENT+1).EQ.'Y')
     +   CALL HF1(ISHFT+35,ELEFF(3),1.)      ! efficiency Trunc. mean
      IF (SWITCH_HISTO(11,IDENT+1).EQ.'Y')
     +   CALL HF1(ISHFT+39,ELEFF(5),1.)      ! efficiency Likelihood E tot
C      IF (SWITCH_HISTO(11,IDENT+1).EQ.'Y')
C     +   CALL HF1(ISHFT+41,ELEFF(6),1.)      ! efficiency likel./nb. of clust
      IF (SWITCH_HISTO(12,IDENT+1).EQ.'Y')
     +   CALL HF1(ISHFT+43,ELEFF(7),1.)
      IF (SWITCH_HISTO(13,IDENT+1).EQ.'Y')
     +   CALL HF1(ISHFT+45,ELEFF(8),1.)
   60 CONTINUE
      IF(DOPRINT)WRITE(LOUT,*)'ltrdt',GZTRDT()
  999 CONTINUE
      IF(DOPRINT)WRITE(LOUT,*)' exit trdana ltrdt',GZTRDT()
      IF(DOPRINT)WRITE(LOUT,'(a30,i3,/,a30)')' exit TRDANA track:',
     &    ITR,' ----------------'
      RETURN
 1040 FORMAT(' Track',I2,' tretot',6F7.1,' Et cor',3F7.1)
 1060 FORMAT(' nb. of hit planes',I2,' Total energy',
     &  F7. 1, 'Truncated Energy',F7.1,' Likelihood Etot',G10.4)
 1070 FORMAT(' Effic: Total Energy',G10.4,', Trunc. mean',
     &  G10.4,', Likelih. Etot',G10.4, ' pion rejection =',
     &  3G10.4)
      END

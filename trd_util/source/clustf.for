      SUBROUTINE CLUSTF(YFADC,NENTRY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TRD CLUSTER FINDING
C-          ADAPTED FROM AN OLD ROUTINE USED IN TEST RUN ANALYSIS
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  A LONG TIME AGO IN SACLAY
C-   Updated  25-JUN-1993   A. Zylberstejn
C-   Updated   9-SEP-1994   A. Zylberstejn Completly modified to speed up
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,IA,II,IINF,ILEFT,IM,IMAX,IR,IRIGHT
      INTEGER IUCOMP,IUCOLA,LVMAX
      INTEGER ISUP,NCLUMAX
      INCLUDE 'D0$INC:TRDBGU.INC'
      INTEGER N,NCL,NENTRY
      INTEGER IFOIS,IAUX(128),IDS
      REAL ENER,ESEUI,XLEFT,XRIGHT,XX,ENERGY_THRESH
      REAL Y(128),YFADC(128),YMAX,YREF,YSEUI
      REAL YOFFSET
      LOGICAL FIRST,JPRNT,DESCENTE,DESC
      INCLUDE 'D0$INC:CLUREC.INC/LIST'
      INTEGER LOUT,TRUNIT
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:TGRAV.INC/LIST'
      DATA ESEUI/10./
      DATA YSEUI/0./
      DATA IFOIS/0/
      DATA NCLUMAX/12/
C      DATA ENERGY_THRESH/50./
      DATA YOFFSET/.5/
      DATA FIRST/.TRUE./
C
C  ESEUI = MINIMAL ORDINATE VALUE FOR CENTRAL PEAK
C  YSEUI =THRESHOLD VALUE FOR "PEDESTALS"
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT=TRUNIT()
        ENERGY_THRESH=3.*ESEUI
C        CALL EZPICK('TRD_RCP')
C        CALL EZGET('CLUSTER_THRESHOLD',CLUTHR,IER)
C        IF(IER.EQ.0)ENERGY_THRESH=CLUTHR
C        CALL EZRSET
      END IF
      IFOIS=IFOIS+1
      N=MOD(NENTRY,1000)
      NCREC=0
      NCL=0
      IF(N.LE.4)        GO TO 999
      JPRNT=.FALSE.
      DO I=1,N
        IAUX(I)=1
        Y(I)=YFADC(I)
        IF(Y(I).LE.YSEUI)IAUX(I)=0
      END DO
C  Look for first and last bin above threshold
   20 IINF=IUCOMP(1,IAUX,N)
      ISUP=IUCOLA(1,IAUX,N)
      IINF=MAX0(1,IINF)
      ISUP=MIN0(N-3,ISUP)
      IMAX=LVMAX(Y(IINF),ISUP-IINF+1)+IINF-1 !Search for  max position
C      IF(JPRNT)
C     +  WRITE(LOUT,*)' iinf,isup,imax',IINF,ISUP,IMAX,'y',Y(IMAX),
C     &  'ESEUI',ESEUI
      ILEFT=IMAX
      IRIGHT=IMAX
      IF(Y(IMAX).LE.ESEUI)GO TO 1000
C      IF(IMAX.LT.3) GO TO 1000
      IF(ISUP-IINF.LE.4)GO TO 1000
C      IF(IMAX.EQ.IINF)GO TO 1000
C      IF(IMAX.EQ.ISUP)GO TO 1000
      YMAX=Y(IMAX)
      IM=IMAX
      IF(IMAX-IINF.LT.3)THEN
        ILEFT=IINF
        GO TO 30
      END IF
      II=0
C Search left side of cluster
      IDS=0
   24 DESCENTE=.FALSE.
      DO WHILE(YFADC(IM-1).LE.YFADC(IM)+YOFFSET)
        DESC=.FALSE.
        ILEFT=IM
        IF(IAUX(IM-1).LE.0)GO TO 30
        IF(YFADC(IM-1).GE.YFADC(IM))THEN
          DESC=.FALSE.
        ELSE
          DESC=.TRUE.
          IDS=1
        END IF
        IF(IDS.NE.0 .AND. .NOT.DESC .AND. .NOT.DESCENTE)THEN
          IM=IM+1
          ILEFT=IM
          GO TO 30
        END IF
        IM=IM-1
        ILEFT=IM
        DESCENTE=DESC
        IF(IM.LE.IINF)GO TO 30
      END DO
   30 CONTINUE
      IM=IMAX
      IRIGHT=IM
      IF(IMAX.GE.ISUP-3)THEN
        IRIGHT=ISUP
        GO TO 40
      END IF
C Search right side of cluster
      IDS=0
   34 DESCENTE=.FALSE.
      DO WHILE(YFADC(IM+1).LE.YFADC(IM)+YOFFSET)
        DESC=.FALSE.
        IRIGHT=IM
        IF(IAUX(IM+1).LE.0)GO TO 40
        IF(YFADC(IM+1).GE.YFADC(IM))THEN
C            print*,' im',im,'YFADC(IM+1)',YFADC(IM+1),'YFADC(IM)',
C     &        YFADC(IM),'YFADC(IM+2)',YFADC(IM+2)
          DESC=.FALSE.
C          IF(.not.DESCENTE)GO TO 40
        ELSE
          DESC=.TRUE.
          IDS=1
        END IF
        IF(IDS.NE.0 .AND. .NOT.DESC .AND. .NOT.DESCENTE)THEN
          IM=IM-1
          IRIGHT=IM
          GO TO 40
        END IF
        DESCENTE=DESC
        IM=IM+1
        IRIGHT=IM
        IF(IM.GE.ISUP)GO TO 40
      END DO
   38 CONTINUE
C      if(jprnt)
C     + write(lout,*)' sortie de la boucle right ,im',im,
C     &  'Y(IM+2),Y(IM+1)',Y(IM+2),Y(IM+1)
C      IF(iaux(im+2).eq.0 .or. Y(IM+2).GE.Y(IM+1))GO TO 40
C        II=II+1
C      IM=IM+1
C      print*,' ii,im',ii,im
C      IF(II.EQ.1) GO TO 34
C      go to 34
   40 CONTINUE
      ENER=0.
      II=0
      XX=0.
C      if(jprnt)write(lout,*)' ileft,iright',ileft,iright
      DO I=ILEFT,IRIGHT
        IAUX(I)=0
        Y(I)=0.
        IF(YFADC(I).GT.YSEUI)THEN
          ENER=ENER+YFADC(I)      ! cluster energy
          XX=XX+FLOAT(I)*YFADC(I) ! cluster position
          II=II+1
        END IF
      END DO
C
  122 IF(II.LE.2)GO TO 20 !Clusters with less than 3 bins are not kept
      NCL=NCL+1
      NBINR(NCL)=II
      ECLR(NCL)=ENER
      IGCL(NCL)=1
      IBLFT(NCL)=ILEFT
      IBRGHT(NCL)=IRIGHT
      YSUP(NCL)=YMAX
      XCG(NCL)=XX/ENER
      IBCEN(NCL)=IMAX
      IBMIN=MIN0(IBMIN,ILEFT)
C  ------------
C  Compute FWHM
C  ------------
      YREF=.5*YMAX
      XRIGHT=IRIGHT
      FWHM(NCL)=0.
C  At least one bin on both sides of the max
      IF(IMAX-ILEFT.LT.1)GO TO 139
      IF(IRIGHT-IMAX.LT.1)GO TO 139
      IM=ILEFT
C      if(jprnt)print*,' ileft',ileft
      IF(YFADC(ILEFT).LE.YREF)THEN
        DO WHILE(YFADC(IM).LT.YREF)
C          if(jprnt)print*,' im',im,' y',Yfadc(im),' yref',yref
          IM=IM+1
        END DO
     &
      END IF
      IA=IM-1
      IR=IA+1
 6683 FORMAT(' ILEFT,IMAX,IA,IR',4I4,' YFaDC(IA),YFaDC(IR)',2F5.1,
     &  ' xleft',F5.1)
      IF(YFADC(IR).EQ.YFADC(IA))THEN
        XLEFT=.5*FLOAT(IA+IR)
      ELSE
        XLEFT=FLOAT(IR-IA)*(YREF-YFADC(IA))/(YFADC(IR)-YFADC(IA))
     &      +FLOAT(IA)
      END IF
C      WRITE(LOUT,6683)ILEFT,IMAX,IA,IR,YFADC(IA),YFADC(IR),XLEFT
      IM=IRIGHT
      IF(YFADC(IRIGHT).LE.YREF)THEN
        DO WHILE(YFADC(IM).LT.YREF)
          IM=IM-1
        END DO
      END IF
      IR=IM+1
      IA=IR-1
      IF(YFADC(IR).EQ.YFADC(IA))THEN
        XRIGHT=.5*FLOAT(IA+IR)
      ELSE
        XRIGHT=FLOAT(IR-IA)*(YREF-YFADC(IA))/(YFADC(IR)-YFADC(IA))
     &       +FLOAT(IA)
      END IF
  139 CONTINUE
      FWHM(NCL)=XRIGHT-XLEFT
      IF(JPRNT)THEN
        WRITE(LOUT,6600)NCL,IMAX,YMAX,ILEFT
     +                               ,IRIGHT,ECLR(NCL)
        WRITE(LOUT,6608)XLEFT,XRIGHT,FWHM(NCL),XCG(NCL)
      END IF
 6608 FORMAT(' XLEFT,XRIGHT',2G10.4,' FWHM',G10.4, ' XX',G10.4)
C      IF(jprnt)PRINT 6600,NCL,IMAX,YMAX,ILEFT,IRIGHT,ECLR(NCL)
 6600 FORMAT(' NCL',I3,' IMAX ',I4,' YMAX',F5.1,' ILEFT ',I4
     +,' IRIGHT',I3,' ENERGIE',G10.4)
C  CLUSTERS WITH LESS THAN 3 BINS ARE SUPPRESSED
      IF(NBINR(NCL).LE.2)NCL=NCL-1
      IF(NCL.LT.NCLUMAX)GO TO 20
 1000 CONTINUE
C Clean the cluster sample
      NCREC=0
      DO I=1,NCL
        IF(IGCL(I).NE.0 .AND. ECLR(I).GT.ENERGY_THRESH)THEN
          NCREC=NCREC+1
          ECLR(NCREC)=ECLR(I)
          IGCL(NCREC)=IGCL(I)
          IBLFT(NCREC)=IBLFT(I)
          IBRGHT(NCREC)=IBRGHT(I)
          YSUP(NCREC)=YSUP(I)
          XCG(NCREC)=XCG(I)
          IBCEN(NCREC)=IBCEN(I)
          FWHM(NCREC)=FWHM(I)
        END IF
      END DO
  999 CONTINUE
      RETURN
      END

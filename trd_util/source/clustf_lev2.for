      SUBROUTINE CLUSTF_LEV2(WIRE,LAYER,YFADC,N)
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
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,IA,II,IINF,ILEFT,IM,IMAX,IR,IRIGHT
      INTEGER IUCOMP,IUCOLA,NBIN
      INTEGER IER,IS,ISUP,NEGAL
      INTEGER WIRE,LAYER,TCHNB,YFADC(130)
      INTEGER ESEUIL_ANOD, ESEUIL_CATH
      INTEGER        CLUSTER_TRESH_HIGH,CLUSTER_TRESH_LOW
C      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INTEGER N,NBL1,NBL2,NBL3,NBL4,NCL
      INTEGER ENER,IFOIS,IAUX(128)
      INTEGER PEAK_MIN,BASE_LINE
      REAL YIA,YIR,PEAK_VALUE,YREF
      LOGICAL FIRST,DOPRINT,JPRNT,DESCENTE
      INCLUDE 'D0$INC:CLUREC.INC/LIST'
      LOGICAL PED_SUBSTRACT
      INTEGER LOUT,TRUNIT
C      INCLUDE 'D0$INC:GCFLAG.INC/LIST'
C      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:TGRAV.INC/LIST'
      INCLUDE 'D0$INC:trd_ped.INC'
      DATA IFOIS/0/
      DATA FIRST/.TRUE./
C
C  PEAK_MIN = MINIMAL ORDINATE VALUE FOR CENTRAL PEAK
C  base_line =THRESHOLD VALUE FOR "PEDESTALS"
      IF(FIRST)THEN
        FIRST=.FALSE.
        JPRNT=.FALSE.
        LOUT=6
        LOUT=TRUNIT()
        CALL EZPICK('TRD_lev2')
        PED_SUBSTRACT=.FALSE.
        CALL EZGET('PED_SUBSTRACT',PED_SUBSTRACT,IER)
        CALL EZGET('Cluster_tresh_high',CLUSTER_TRESH_HIGH,IER)
        CALL EZGET('Cluster_tresh_low',CLUSTER_TRESH_LOW,IER)
        CALL EZRSET
        BASE_LINE=CLUSTER_TRESH_LOW
        ESEUIL_ANOD=BASE_LINE+CLUSTER_TRESH_HIGH
        ESEUIL_CATH=BASE_LINE+.5*CLUSTER_TRESH_HIGH
      END IF
      IFOIS=IFOIS+1
      JPRNT=.FALSE.
      IF(IFOIS.LE.20)JPRNT=.TRUE.
C      if(jprnt)write(lout,*)' enter clustf'
      NCL=0
      NCREC=0
      IF(PED_SUBSTRACT)THEN
        BASE_LINE=PED_TRD(TCHNB(WIRE,LAYER))+.5
        ESEUIL_ANOD=BASE_LINE+CLUSTER_TRESH_HIGH
        ESEUIL_CATH=BASE_LINE+.5*CLUSTER_TRESH_HIGH
      END IF
      IF(LAYER.LE.3)THEN
        PEAK_MIN=ESEUIL_ANOD
      ELSE
        PEAK_MIN=ESEUIL_CATH
      END IF
      DO I=1,N
        IAUX(I)=1
        IF(YFADC(I).LE.BASE_LINE)IAUX(I)=0
      END DO
C  Look for first and last bin above threshold
      IINF=3
      ISUP=N-3
      NBIN=ISUP-IINF+1
      IINF=IUCOMP(1,IAUX,N)
      ISUP=IUCOLA(1,IAUX,N)
C      if(jprnt)write(lout,*)
C     &  ' dans clustf,iinf,isup,nbin',iinf,isup,NBIN
   20 CONTINUE
      IF(ISUP-IINF.LE.4)GO TO 1000
      PEAK_VALUE=0.
      IMAX=0
      DO 10 I=IINF+1,ISUP-1 ! Search max.
        IF(IAUX(I).EQ.0 .OR. YFADC(I).LT.PEAK_MIN .OR.
     &     YFADC(I).LT.PEAK_VALUE)GO TO 10
        IMAX=I
        PEAK_VALUE=YFADC(I)
   10 CONTINUE
      IF(IMAX.LT.3) GO TO 1000
      IM=IMAX
      II=0
      IF(JPRNT)WRITE(LOUT,*)'iinf,isup,imax',IINF,ISUP,IMAX,' ymax',
     &  PEAK_VALUE
C Search left side of cluster
      ILEFT=IMAX
   24 DESCENTE=.FALSE.
      DO WHILE(YFADC(IM-1).LE.YFADC(IM))
C        if(jprnt)write(lout,*)'im',im,'yfadc',YFADC(IM-1),YFADC(IM)
C     &    ,' iaux',iaux(im-1),iaux(im)
        IF(IAUX(IM-1).LE.0)GO TO 30
        IF(YFADC(IM-1).EQ.YFADC(IM))THEN
C          if(jprnt)
C     +   write(lout,*)' dans la boucle left,descente ',descente,' im',im
          IF(DESCENTE)GO TO 30
        ELSE
          DESCENTE=.TRUE.
        END IF
        IM=IM-1
        ILEFT=IM
C        if(jprnt)write(lout,*)'im,ileft',im,ileft,' iinf',iinf
        IF(IM.LE.IINF)GO TO 30
      END DO
      IF(YFADC(IM-2).GE.YFADC(IM-1))GO TO 30
      II=II+1
      IM=IM-1
      IF(II.EQ.1) GO TO 24
   30 CONTINUE
C      write(lout,*)' sortie de la boucle left,ileft',ileft,
C     &  'imax',imax
      IF(ILEFT-IINF.LE.1)IINF=ILEFT
      IM=IMAX
      IRIGHT=IM
C Search left side of cluster
   34 DESCENTE=.FALSE.
      DO WHILE(YFADC(IM+1).LE.YFADC(IM))
        IF(IAUX(IM+1).LE.0)GO TO 40
        IF(YFADC(IM+1).EQ.YFADC(IM))THEN
          IF(DESCENTE)GO TO 40
        ELSE
          DESCENTE=.TRUE.
        END IF
        IM=IM+1
        IRIGHT=IM
        IF(IM.GE.ISUP)GO TO 40
      END DO
C      write(lout,*)' sortie de la boucle right ,im',im
      IF(YFADC(IM+2).GE.YFADC(IM+1))GO TO 40
      II=II+1
      IM=IM+1
      IF(II.EQ.1) GO TO 34
   40 CONTINUE
      IF(ISUP-IRIGHT.LE.1)ISUP=IRIGHT
      ENER=0.
      II=0
C      if(jprnt)write(lout,*)' ileft,iright',ileft,iright
C      IF(IRIGHT.LE.ILEFT)call exit
      DO I=ILEFT,IRIGHT
        IAUX(I)=0
        ENER=ENER+YFADC(I)      ! cluster energy
        II=II+1
      END DO
C
  122 IF(II.LE.2)GO TO 20 !Clusters with less than 3 bins are not kept
      NCL=NCL+1
      NBINR(NCL)=II
      ECLR(NCL)=ENER
      IF(PED_SUBSTRACT)
     +  ECLR(NCL)=ECLR(NCL)-FLOAT(II)*PED_TRD(TCHNB(WIRE,LAYER))
C      IGCL(NCL)=1
      IBLFT(NCL)=ILEFT
      IBRGHT(NCL)=IRIGHT
      YSUP(NCL)=PEAK_VALUE
C      XCG(NCL)=IMAX
      IBCEN(NCL)=IMAX
  139 CONTINUE
      IF(JPRNT)THEN
        WRITE(LOUT,6600)NCL,IMAX,PEAK_VALUE,ILEFT
     +                               ,IRIGHT,ECLR(NCL)
      END IF
 6600 FORMAT(' NCL',I3,' IMAX ',I4,' peak_value',F5.1,' ILEFT ',I4
     +,' IRIGHT',I3,' ENERGIE',G10.4)
      GO TO 20
 1000 CONTINUE
      NCREC=NCL
C      if(jprnt)write(lout,*)' exit CLUSTF ncrec',ncrec
      RETURN
      END

      SUBROUTINE TPRLFIL (LAYER,TRACK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill TRD ZEBRA banks TPRL (hanging under TRDT)
C-                         Information on energy deposit on individual layers
C-
C-   Inputs  : TRACK = TRACK NUMBER
C-             LAYER = Layer number (1,2,3)
C-   Outputs :
C-   Controls:
C-
C-   Created  25-FEB-1991   A. Zylberstejn : adapted from TRDFIL
C-   Updated  11-JAN-1993   Alain PLUQUET  : new TPRL structure (version>=2)
C-   Updated  12-FEB-1993   A. Zylberstejn  :add number of hits in bank 21
C-   Updated  27-MAY-1993   A. Zylberstejn   :add local density of hits in
C-                                              bank 22
C-   Updated  23-JUN-1993   Alain PLUQUET  version 2.2 with new bandwidth for
C-                                         cluster energies (PACK/UNPACK_TPRL)
C-   Updated  25-NOV-1993   A. ZYLBERSTEJN  Version 3 with clusters per layer
C-   Updated  30-SEP-1994   Alain PLUQUET  version 4.512 new calibration method
C-                                         (with Yves's routine TRD_CALURAPT)
C-   Updated  31-JAN-1995   Lewis Taylor Goss   include over-all correction
C-                                              factor in REAL_WORD(48)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC: TPRL_VERSION_NB.INC'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
      INCLUDE 'D0$INC:TCLUS_PER_WIRE.INC'
      INCLUDE 'D0$INC:TRENER.INC'
      INCLUDE 'D0$INC:CLUREC.INC'
      INCLUDE 'D0$INC:TRTOBN.INC'
      INCLUDE 'D0$INC:TRDCOR.INC'
      INCLUDE 'D0$INC:TR_INFO_HIT.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:URAN_COR.INC'
      LOGICAL FIRST,EXTENDED,DOPRINT,TRD_DO_PRINT
      INCLUDE 'D0$INC:TRD_NWORD.INC'
      INTEGER NA,NC,NCLA,NCLC,I,LTPRL,IER,JS,KS,LTRDT,GZTRDT
      INTEGER LAYER,TRACK,TRD_COR_STATUS,DENSITY_OF_TRD_HITS,LOUT,TRUNIT
      CHARACTER*24 errmsg1,errmsg2
      CHARACTER*4 car4
      INTEGER NCLUS_CATH,NCLUS_ANOD
      REAL VERSION
      INCLUDE 'D0$INC:worksp.INC'
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        EXTENDED=.FALSE.
        VERSION=TPRL_VERS +FLOAT(NWIRE_PER_LAYER(3))/1000.
        LOUT=TRUNIT()
      END IF
      DOPRINT=TRD_DO_PRINT()
      IWS(4003)=TRACK
      IF(DOPRINT)WRITE(LOUT,2002) TRACK,LAYER
 2002 FORMAT(' Enter TPRLFIL track',I3,' LAYER',I2,/,
     &       ' -------------')
      IF (NBHWIR(LAYER,TRACK).LE.0) GO TO 999
      NA=NBHWIR(LAYER,TRACK)
      NC=NBHWIR(LAYER+3,TRACK)
      NCLA=NTOT_CL(LAYER)
      NCLC=NTOT_CL(LAYER+3)
      IF(DOPRINT)WRITE(LOUT,*)' na,nc,ncla,nclc',NA,NC,NCLA,NCLC,
     &  ' layer',LAYER
      LTPRL=0
      LTRDT=GZTRDT()
C      if(doprint)write(lout,*)'in tprlfil ltrdt',ltrdt
      IF(LTRDT.LE.15 000)THEN
        CALL ERRMSG('Not enough room for TPRL','TPRLFIL',
     &        'Skip TPRL filling ' ,'W')
        GO TO 999
      END IF
      CALL BKTPRL(LTPRL,LAYER)
      IF(DOPRINT)WRITE(LOUT,*)' after bktprl,ltprl',LTPRL
      IF(LTPRL.LE.0)THEN
        CALL ERRMSG(' TPRL not booked','TPRLFIL',' ','W')
        GO TO 999
      END IF
      LTRDT=LQ(LTPRL+1)
C      IF(DOPRINT)WRITE(LOUT,*)' After BKTPRL ltprl',LTPRL,' ltrdt',
C     &  LTRDT,' track', IQ(LTRDT-5),' ltrdt form gztrdt',GZTRDT()
      CALL VZERO(INTEGER_WORD,NWORD)
      CALL VFILL(REAL_WORD,NWORD,0.)
C     Q(LTPRL+1)=2.1  ! After putting hit cells code in integer_word(1) and (2)
      Q(LTPRL+1)=2.2  ! After changing bandwidth for cluster energies
      Q(LTPRL+1)=3.+FLOAT(NWIRE_PER_LAYER(3))/1000. !after nov 93
      Q(LTPRL+2)=TMINIM(LAYER,TRACK)

      IQ(LTPRL+4)=TRD_COR_STATUS(LAYER)
      Q(LTPRL+12)=TRETOT(LAYER,TRACK)
      Q(LTPRL+13)=TRETOT(LAYER+3,TRACK)
      IF(DOPRINT)WRITE(LOUT,*)' layer',LAYER,'energy anode',
     &  Q(LTPRL+12),' E cath.',Q(LTPRL+13),' tminim (in tprlfil)',
     &  q(ltprl+2)
      REAL_WORD(1)=PAD(LAYER,1,2)
      REAL_WORD(2)=PAD(LAYER,1,3)
      REAL_WORD(41)=PHITRA(LAYER,TRACK)
      REAL_WORD(42)=PAD(LAYER,1,8)
      REAL_WORD(43)=PAD(LAYER,1,9)
      REAL_WORD(44)=PAD(LAYER,1,7)
      REAL_WORD(48)=OVER_ALL_COR(LAYER)
      IF(DOPRINT)WRITE(LOUT,*)' layer',LAYER,' real_word(1)',
     &  REAL_WORD(1),
     &  'REAL_WORD(2)',REAL_WORD(2)
      DO I=1,4
        REAL_WORD(3*I)  =PAD(LAYER,I,15)
        REAL_WORD(3*I+1)=PAD(LAYER,I,16)
        REAL_WORD(3*I+2)=PAD(LAYER,I,17)
        REAL_WORD(14+I) =PAD(LAYER,I,4)
        REAL_WORD(20+I) =PAD(LAYER,I,5)
        REAL_WORD(24+I) =PAD(LAYER,I,6)
        REAL_WORD(28+I) =PAD(LAYER,I,1)
        INTEGER_WORD(7+I)=0 ! to be filled later (after loop on tracks)
      ENDDO
      IF(DOPRINT)WRITE(LOUT,*)' real_word',(REAL_WORD(I),I=1,4)
      REAL_WORD(19)=ETOTWI(NBINFO-1,LAYER,TRACK)
      REAL_WORD(20)=ETOTWI(NBINFO  ,LAYER,TRACK)
      IF(DOPRINT)WRITE(LOUT,*)' real_word 19 et 20)',REAL_WORD(19),
     &  REAL_WORD(20)
      INTEGER_WORD(1)=0 ! coded word for hit anode cells
      INTEGER_WORD(2)=0 ! coded word for hit cathode cells
      INTEGER_WORD(3)=0 !spare
      INTEGER_WORD(4)=NA
      INTEGER_WORD(5)=NC
      JS=50
      DO I=1,NA
        JS=JS+1
        INTEGER_WORD(JS)=WIRNBH(I,LAYER,TRACK)-1 ! wire nb.
        REAL_WORD(JS)=ETOTWI(I,LAYER,TRACK)      !energy
        IF(DOPRINT)WRITE(LOUT,*)' layer',LAYER,' anode',
     &    INTEGER_WORD(JS),' energy',REAL_WORD(50+I)
        IF(MULT_CELL(I,LAYER,TRACK).NE.0)CALL SBIT1(INTEGER_WORD(1),I)
      ENDDO
      IF(NC.NE.0)THEN
        DO I=1,NC
          JS=JS+1
          INTEGER_WORD(JS)=WIRNBH(I,LAYER+3,TRACK)-1
          REAL_WORD(JS)=ETOTWI(I,LAYER+3,TRACK)
          IF(MULT_CELL(I,LAYER+3,TRACK).NE.0)CALL SBIT1(INTEGER_WORD(2),
     &      I)
          IF(DOPRINT)WRITE(LOUT,*)' layer',LAYER,' cath.',
     &      INTEGER_WORD(JS),' energy',REAL_WORD(JS)
        ENDDO
      END IF
      IF(NCLA.GE.1)THEN! do not fill cluster banks if no clust on anodes
        IF(DOPRINT)WRITE(LOUT,*)' ncla',NCLA,' ltprl',LTPRL,
     &    ' js initial',JS
        NCLUS_ANOD=0
        IF(JS+NCLA*5.GT.NWORD-10)THEN
          NCLUS_ANOD=(NWORD-20-JS)/5
          WRITE(car4,'(i3)')ncla
          errmsg1='  anode. clusters '//car4
          WRITE(car4,'(i3)')nclus_anod
          errmsg2=' reduced to  '//car4
          CALL errmsg(errmsg1,'tprlfil',errmsg2,'w')
c          PRINT*,' too many anode. clusters, room left for',
c     &                NCLUS_anod,' clusters'
          IF(NCLUS_ANOD.LE.0)THEN
            NTOT_CL(LAYER)=0
            CALL VZERO(NCL_PER_WIRE(1,LAYER),NA)
            NTOT_CL(LAYER+3)=0
            CALL VZERO(NCL_PER_WIRE(1,LAYER+3),NA)
            GO TO 46
          ELSE
            CALL TRD_CLEAN_CLUST(LAYER,TRACK,NCLUS_ANOD)
            NCLA=NCLUS_ANOD
            NTOT_CL(LAYER)=NCLA
          END IF
        END IF
        INTEGER_WORD(6)=NTOT_CL(LAYER)
        DO I=1,NCLA
          JS=JS+1
          REAL_WORD(JS)=ECL_PER_WIRE(I,LAYER)
C          JS=JS+1
          INTEGER_WORD(JS)=LEFT_CL(I,LAYER)
          INTEGER_WORD(JS+1)=CENTER_CL(I,LAYER)
          INTEGER_WORD(JS+2)=RIGHT_CL(I,LAYER)
          INTEGER_WORD(JS+3)=HEIGTH_CL(I,LAYER)+.5
          IF(DOPRINT)WRITE(LOUT,*)' cluster ANODE',I,' en ',JS,
     &      ' position left ',INTEGER_WORD(JS),' center',
     &      INTEGER_WORD(JS+1),INTEGER_WORD(JS+2),INTEGER_WORD(JS+3)
          JS=JS+4
        ENDDO
        IF(DOPRINT)WRITE(LOUT,*)' JS after clust. anodes',JS
        KS=0
        NCLUS_CATH=0
        IF(JS+NCLC*5.GT.NWORD-10)THEN
          NCLUS_CATH=(NWORD-20-JS)/5
          WRITE(car4,'(i3)')nclc
          errmsg1=' cathode clusters '//car4
          WRITE(car4,'(i3)')nclus_cath
          errmsg2=' reduced to  '//car4
          CALL errmsg(errmsg1,'tprlfil',errmsg2,'w')
c          PRINT*,' too many cath. clusters, room left for',
c     &                NCLUS_CATH,' clusters'
          IF(NCLUS_CATH.LE.0)THEN
            NTOT_CL(LAYER+3)=0
            CALL VZERO(NCL_PER_WIRE(1,LAYER+3),NA)
            GO TO 46
          ELSE
            CALL TRD_CLEAN_CLUST(LAYER+3,TRACK,NCLUS_CATH)
            NCLC=NCLUS_CATH
            NTOT_CL(LAYER+3)=NCLC
          END IF
        END IF
        INTEGER_WORD(7)=NTOT_CL(LAYER+3)
        IF(NCLC.GE.1)THEN
          DO I=1,NCLC
            KS=KS+1
            JS=JS+1
            REAL_WORD(JS)=ECL_PER_WIRE(I,LAYER+3)
C            JS=JS+1
            INTEGER_WORD(JS)=LEFT_CL(I,LAYER+3)
            INTEGER_WORD(JS+1)=CENTER_CL(I,LAYER+3)
            INTEGER_WORD(JS+2)=RIGHT_CL(I,LAYER+3)
            INTEGER_WORD(JS+3)=HEIGTH_CL(I,LAYER+3)
            IF(DOPRINT)WRITE(LOUT,*)' cluster cath',I,' en ',JS,
     &        ' position left ',INTEGER_WORD(JS),' center',
     &        INTEGER_WORD(JS+1),INTEGER_WORD(JS+2),INTEGER_WORD(JS+3)
            JS=JS+4
            IF(JS.GT.NWORD-10)THEN
              CALL ERRMSG('Too many unpack words ','TPRLFIL',' ','W')
              INTEGER_WORD(7)=KS
              GO TO 40
            END IF
          ENDDO
   40     CONTINUE
        END IF
      END IF
   46 CONTINUE
      DO I=1,NA
        JS=JS+1
        INTEGER_WORD(JS)=NCL_PER_WIRE(I,LAYER)
        IF(DOPRINT)WRITE(LOUT,*)' js',JS,
     &      ' nb. of anode clust.', INTEGER_WORD(JS)
      ENDDO
      DO I=1,NC
        JS=JS+1
        INTEGER_WORD(JS)=NCL_PER_WIRE(I,LAYER+3)
        IF(DOPRINT)WRITE(LOUT,*)' js',JS,
     &      ' nb. de clust cath.', INTEGER_WORD(JS)
      ENDDO
      IF(doprint)WRITE(lout,*)' layer',layer,' rw(15,16,17,18)',
     &    real_word(15),real_word(16),real_word(17),real_word(18)
      CALL PACK_TPRL(LTPRL,VERSION,REAL_WORD,INTEGER_WORD,IER)
      IF(DOPRINT)WRITE(LOUT,*)' after pack_tprl, gztrdt',GZTRDT()
      IQ(LTPRL+21)=NCODED(LAYER)+1000*NCODED(LAYER+3)
      IF(DOPRINT)WRITE(LOUT,*)' ncoded',NCODED(LAYER),NCODED(LAYER+3)
      IF(NA.GE.1)THEN
        IQ(LTPRL+22)=DENSITY_OF_TRD_HITS(WIRNBH(1,LAYER,TRACK),NA,
     &      LAYER)
      END IF
      IF(doprint)THEN
        WRITE(lout,*)' End of tprlfil,layer',layer,
     &    ' tmin',q(ltprl+2)
        CALL UNPACK_TPRL(LTPRL,VERSION,REAL_WORD,INTEGER_WORD,IER)
      END IF
  999 CONTINUE
c      IF(DOPRINT)WRITE(LOUT,*)' gztrdt',GZTRDT()
      IF(DOPRINT)WRITE(LOUT,2010)
 2010 FORMAT(' Exit TPRLFIL',/' ------------')
      RETURN
      END

      SUBROUTINE TSTPBK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book TRD static parameters banks
C-
C-   Inputs  : none
C-   Outputs : TRD_STPFILE.DAT file
C-
C-   Created  15-OCT-1987  MANSOULIE
C-   Modified 11-MAR-1988     "
C-   Updated  21-JUL-1989  A. Zylberstejn Introduce new tree under TPHY
C-   Updated  17-JUL-1991  Alain PLUQUET  Remove TMAT TWAH TRWA
C-                                        Add TDDA TDMC
C-   Updated  11-MAY-1992  Alain PLUQUET  Add MZFORM
C-   Updated  28-JUN-1993  Alain PLUQUET  Add data from run 1a
C-                                        analysis version 0  (T1A0)
C-   Updated   6-JAN-1994   A. Zylberstejn   : increase the size for ped and
C-   gains for layer 3
C-   Updated  12-OCT-1994   Alain PLUQUET  Add Laurent's canary fit
C-   Updated  10-DEC-1994   JEAN-FRANCOIS LEBRAT  Add likelihood tables
C-   Updated  10-MAY-1995   A. Zylberstejn   Define bank T1B0
C-   Updated  12-SEP-1995   L.T.Goss add EPSL' info. to TLTA (courtesy of JFL)
C-   Updated  16-FEB-1996   Y.Ducros: Tables to compute epsl like quantities
C-                                    for FAKE, Electrons,conversions
C-   Updated  26-FEB-1996   A. ZYLBERSTEJN : Add TCOR
C-   Updated  16-APR-1996   A. Zylberstejn   : add TND1 and TND2
C-   Updated  17-APR-1996   A. Zylberstejn   : increase dimension of bank TDOR
C-                                              to 8000 (instead 2000)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:LTRD.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSTRD.LINK'
      INCLUDE 'D0$LINKS:IZTPDH.LINK'
      INCLUDE 'D0$LINKS:IZTGAI.LINK'
      INCLUDE 'D0$LINKS:IZTGEN.LINK'
      INCLUDE 'D0$LINKS:IZTGEO.LINK'
      INCLUDE 'D0$LINKS:IZTCAN.LINK'
      INCLUDE 'D0$LINKS:IZTELH.LINK'
      INCLUDE 'D0$LINKS:IZTWGH.LINK'
      INCLUDE 'D0$LINKS:IZTROP.LINK'
      INCLUDE 'D0$LINKS:IZTPHY.LINK'
C-------------------------------------------------------------------------------
C     STRUCTURE UNDER TCHA
C-------------------------------------------------------------------------------
      INCLUDE 'D0$LINKS:IZTCHA.LINK'
      INCLUDE 'D0$LINKS:IZTCA1.LINK'
      INCLUDE 'D0$LINKS:IZTCA2.LINK'
      INCLUDE 'D0$LINKS:IZTMAE.LINK'
      INCLUDE 'D0$LINKS:IZTMXE.LINK'
      INCLUDE 'D0$LINKS:IZTMAA.LINK'
      INCLUDE 'D0$LINKS:IZTMAB.LINK'
      INCLUDE 'D0$LINKS:IZTMAC.LINK'
C-------------------------------------------------------------------------------
C     STRUCTURE UNDER TPRO
C-------------------------------------------------------------------------------
      INCLUDE 'D0$LINKS:IZTPRO.LINK'
      INCLUDE 'D0$LINKS:IZTPR1.LINK'
      INCLUDE 'D0$LINKS:IZTPR2.LINK'
      INCLUDE 'D0$LINKS:IZTPET.LINK'
      INCLUDE 'D0$LINKS:IZTPE3.LINK'
      INCLUDE 'D0$LINKS:IZTPTR.LINK'
      INCLUDE 'D0$LINKS:IZTPT3.LINK'
      INCLUDE 'D0$LINKS:IZTPLE.LINK'
      INCLUDE 'D0$LINKS:IZTPEC.LINK'
      INCLUDE 'D0$LINKS:IZTPCA.LINK'
      INCLUDE 'D0$LINKS:IZTPCB.LINK'
      INCLUDE 'D0$LINKS:IZTPCC.LINK'
C-------------------------------------------------------------------------------
C     STRUCTURE UNDER T1A0
C-------------------------------------------------------------------------------
      INCLUDE 'D0$LINKS:IZT1A0.LINK'
      INCLUDE 'D0$LINKS:IZT1B0.LINK'
C-------------------------------------------------------------------------------
C     STRUCTURE UNDER TGEO
C-------------------------------------------------------------------------------
      INCLUDE 'D0$LINKS:IZTMAT.LINK'
      INCLUDE 'D0$LINKS:IZTACH.LINK'
      INCLUDE 'D0$LINKS:IZTDMC.LINK'
      INCLUDE 'D0$LINKS:IZTDDA.LINK'
C-------------------------------------------------------------------------------
C     STRUCTURE UNDER STRD
C-------------------------------------------------------------------------------
      INCLUDE 'D0$LINKS:IZTLIK.LINK'
      INCLUDE 'D0$LINKS:IZTPIO.LINK'
      INCLUDE 'D0$LINKS:IZTPI1.LINK'
      INCLUDE 'D0$LINKS:IZTPI2.LINK'
      INCLUDE 'D0$LINKS:IZTPDE.LINK'
      INCLUDE 'D0$LINKS:IZTELE.LINK'
      INCLUDE 'D0$LINKS:IZTEL1.LINK'
      INCLUDE 'D0$LINKS:IZTEL2.LINK'
      INCLUDE 'D0$LINKS:IZTEDE.LINK'
      INCLUDE 'D0$LINKS:IZTLTA.LINK'
C-------------------------------------------------------------------------------
C     STRUCTURE UNDER TGEN
C-------------------------------------------------------------------------------
      INCLUDE 'D0$LINKS:IZTDEN.LINK'
      INCLUDE 'D0$LINKS:IZTDOR.LINK'
      INCLUDE 'D0$LINKS:IZTDPI.LINK'
      INCLUDE 'D0$LINKS:IZTDFA.LINK'
      INCLUDE 'D0$LINKS:IZTDEL.LINK'
      INCLUDE 'D0$LINKS:IZTDCO.LINK'
C
      INTEGER IOTROP,IOTRAC
      INTEGER IOTPHY,IOTCHA,IOTPRO,IOTCA1,IOTCA2,IOTPR1,IOTPR2
      integer LTND1,LTND2
      INTEGER IOT1A0,IOT1B0
      INTEGER IOTMAE,IOTMXE,IOTMAA,IOTMAB,IOTMAC
      INTEGER IOTPET,IOTPE3,IOTPTR,IOTPT3,IOTPLE,IOTPEC
      INTEGER IOTPCA,IOTPCB,IOTPCC
      INTEGER LTRPD,LTELH,LTREL,LTWGH,LTRWG,LTROP
      INTEGER LTACH,LTRAC,LTCOR
      INTEGER LTDMC,LTDDA,LT1A0,LT1B0
      INTEGER LTDEN,LTDOR,LTDPI,LTDFA,LTDEL,LTDCO
      INTEGER I,LI(2)
      INTEGER IOSTRD,IOTPDH,IOTGAI,IOTGEN,IOTGEO,IOTRPD,IOTELH,
     &        IOTREL,IOTWGH,IOTRWG,IOTACH,IOTDMC,IOTDDA
      INTEGER IXIO,LTPIO,LTPI1,LTPI2,LTELE,LTEL1,LTEL2,LTLTA,LTPDE,
     &  LTEDE
C-------------------------------------------------------------------------------
C     STRUCTURE UNDER TCAN
C-------------------------------------------------------------------------------
      INCLUDE 'D0$LINKS:IZTCY1.LINK'
      INTEGER IOTCAN,IOTCY1,LTCY1
C-------------------------------------------------------------------------------
C     BOOK PHYSICAL QUANTITIES
C-------------------------------------------------------------------------------
      LTRD(1)=1
      CALL MZLINT(IDVSTP,'/LTRD/',LTRD,LTPCC,LT)
C-------------------------------------------------------------------------------
C     BOOK STATIC PARAMETERS TRD TOP BANK STRD AS TOP BANK IN ZEBSTP
C-------------------------------------------------------------------------------
      CALL MZFORM ('STRD','-I',IOSTRD)
      CALL MZBOOK(IDVSTP,LSTRD,LSTPH,1,'STRD',5,5,10,IOSTRD,0)
C-------------------------------------------------------------------------------
C     BOOK HEADER BANKS
C-------------------------------------------------------------------------------
      CALL MZFORM ('TPDH','-I',IOTPDH)
      CALL MZBOOK(IDVSTP,LTPDH,LSTRD,-IZTPDH,'TPDH',7,7,4,IOTPDH,0)
      CALL MZFORM ('TGAI','-I',IOTGAI)
      CALL MZBOOK(IDVSTP,LTGAI,LSTRD,-IZTGAI,'TGAI',7,7,0,IOTGAI,0)
      CALL MZFORM ('TGEN','-I',IOTGEN)
      CALL MZBOOK(IDVSTP,LTGEN,LSTRD,-IZTGEN,'TGEN',5,5,2,IOTGEN,0)
C            CALL DZSURV('APRES MZBOOK DE TGEN',IXSTP,LTGEN)
C      CALL BKTGEN(LTGEN)
      CALL MZFORM ('TGEO','-I',IOTGEO)
      CALL MZBOOK(IDVSTP,LTGEO,LSTRD,-IZTGEO,'TGEO',3,3,2,IOTGEO,0)
      CALL MZFORM ('TCAN','-I',IOTCAN)
      CALL MZBOOK(IDVSTP,LTCAN,LSTRD,-IZTCAN,'TCAN',1,1,2,IOTCAN,0)
C-------------------------------------------------------------------------------
C     BOOK PEDESTAL BANKS
C-------------------------------------------------------------------------------
      CALL MZFORM ('TRPD','-F',IOTRPD)
      DO I=1,6
        IF(I.NE.3)THEN
          CALL MZBOOK(IDVSTP,LTRPD,LTPDH,-I,'TRPD',0,0,256,IOTRPD,0)
        ELSE
          CALL MZBOOK(IDVSTP,LTRPD,LTPDH,-I,'TRPD',0,0,512,IOTRPD,0)
        END IF
      END DO
C-------------------------------------------------------------------------------
C     BOOK ELECTRONICS GAINS BANKS
C-------------------------------------------------------------------------------
      CALL MZFORM ('TELH','-I',IOTELH)
      CALL MZBOOK(IDVSTP,LTELH,LTGAI,-IZTELH,'TELH',6,6,4,IOTELH,0)
      CALL MZFORM ('TREL','-F',IOTREL)
      CALL MZBOOK(IDVSTP,LTREL,LTELH,-1,'TREL',0,0,256,IOTREL,0)
      CALL MZBOOK(IDVSTP,LTREL,LTELH,-2,'TREL',0,0,256,IOTREL,0)
      CALL MZBOOK(IDVSTP,LTREL,LTELH,-3,'TREL',0,0,512,IOTREL,0)
      CALL MZBOOK(IDVSTP,LTREL,LTELH,-4,'TREL',0,0,256,IOTREL,0)
      CALL MZBOOK(IDVSTP,LTREL,LTELH,-5,'TREL',0,0,256,IOTREL,0)
      CALL MZBOOK(IDVSTP,LTREL,LTELH,-6,'TREL',0,0,256,IOTREL,0)
C-------------------------------------------------------------------------------
C     BOOK WIRE GAINS BANKS
C-------------------------------------------------------------------------------
      CALL MZFORM ('TWGH','-I',IOTWGH)
      CALL MZBOOK(IDVSTP,LTWGH,LTGAI,-IZTWGH,'TWGH',3,3,4,IOTWGH,0)
      CALL MZFORM ('TRWG','-F',IOTRWG)
      CALL MZBOOK(IDVSTP,LTRWG,LTWGH,-1,'TRWG',0,0,256,IOTRWG,0)
      CALL MZBOOK(IDVSTP,LTRWG,LTWGH,-2,'TRWG',0,0,256,IOTRWG,0)
      CALL MZBOOK(IDVSTP,LTRWG,LTWGH,-3,'TRWG',0,0,512,IOTRWG,0)
C-------------------------------------------------------------------------------
C     BOOK GENERAL TRD BANKS
C-------------------------------------------------------------------------------
C-------------------------------------------------------------------------------
C     BOOK OPERATING CONDITIONS BANK
C-------------------------------------------------------------------------------
      CALL MZFORM('TROP','2I -F',IOTROP)
      CALL MZBOOK(IDVSTP,LTROP,LTGEN,-IZTROP,'TROP',0,0,104,IOTROP,0)
C-------------------------------------------------------------------------------
C     BOOK BANKS FOR PHYSISCS (UNDER TPHY)
C-------------------------------------------------------------------------------
      CALL MZFORM('TPHY','9I -F',IOTPHY)
      CALL MZBOOK(IDVSTP,LTPHY,LTGEN,-IZTPHY,'TPHY',6,6,10,IOTPHY,0)
      CALL MZFORM('TCHA','9I -F',IOTCHA)
      CALL MZBOOK(IDVSTP,LTCHA,LTPHY,-IZTCHA,'TCHA',3,3,14,IOTCHA,0)
      CALL MZFORM('TPRO','9I -F',IOTPRO)
      CALL MZBOOK(IDVSTP,LTPRO,LTPHY,-IZTPRO,'TPRO',3,3,14,IOTPRO,0)
      CALL MZFORM('T1A0','4F 1I 5F -I',IOT1A0)
      CALL MZFORM('T1B0','4F 1I 5F -I',IOT1B0)
      CALL MZBOOK(IDVSTP,LT1A0,LTPHY,-IZT1A0,'T1A0',0,0,622,IOT1A0,0)
      CALL MZBOOK(IDVSTP,LT1B0,LTPHY,-IZT1B0,'T1B0',0,0,622,IOT1B0,0)
      call BKTND1(LTND1)
      call BKTND2(LTND2)
C      print*,' book t1a0 in',lt1a0,' book t1b0 in',lt1b0
C-------------------------------------------------------------------------------
C     BOOK STRUCTURE UNDER TCHA
C-------------------------------------------------------------------------------
      CALL MZFORM('TCA1','9I -F',IOTCA1)
      CALL MZBOOK(IDVSTP,LTCA1,LTCHA,-IZTCA1,'TCA1',2,2,10,IOTCA1,0)
      CALL MZFORM('TCA2','9I -F',IOTCA2)
      CALL MZBOOK(IDVSTP,LTCA2,LTCHA,-IZTCA2,'TCA2',2,2,10,IOTCA2,0)
      CALL MZFORM('TMXE','9I 1F 1I -F',IOTMXE)
      CALL MZBOOK(IDVSTP,LTMXE,LTCA1,-IZTMXE,'TMXE',0,0,313,IOTMXE,0)
C      CALL MZFORM('TMAE','9I -F',IOTMAE)
C      CALL MZBOOK(IDVSTP,LTMAE,LTCA1,-IZTMAE,'TMAE',3,3,10,IOTMAE,0)
      IF(LTMAE.NE.0)THEN
        CALL MZFORM('TMAA','9I 1F 1I -F',IOTMAA)
        CALL MZBOOK(IDVSTP,LTMAA,LTMAE,-IZTMAA,'TMAA',0,0,3014,IOTMAA,0)
        CALL MZFORM('TMAB','9I 1F 1I -F',IOTMAB)
        CALL MZBOOK(IDVSTP,LTMAB,LTMAE,-IZTMAB,'TMAB',0,0,3014,IOTMAB,0)
        CALL MZFORM('TMAC','9I 1F 1I -F',IOTMAC)
        CALL MZBOOK(IDVSTP,LTMAC,LTMAE,-IZTMAC,'TMAC',0,0,3014,IOTMAC,0)
      END IF
      CALL MZFORM('TMXE','9I -F',IOTMXE)
      CALL MZBOOK(IDVSTP,LTMXE,LTCA2,-IZTMXE,'TMXE',0,0,313,IOTMXE,0)
      CALL MZFORM('TMAE','9I -F',IOTMAE)
      CALL MZBOOK(IDVSTP,LTMAE,LTCA2,-IZTMAE,'TMAE',3,3,10,IOTMAE,0)
      CALL MZFORM('TMAA','9I 1F 1I -F',IOTMAA)
      CALL MZBOOK(IDVSTP,LTMAA,LTMAE,-IZTMAA,'TMAA',0,0,3014,IOTMAA,0)
      CALL MZFORM('TMAB','9I 1F 1I -F',IOTMAB)
      CALL MZBOOK(IDVSTP,LTMAB,LTMAE,-IZTMAB,'TMAB',0,0,3014,IOTMAB,0)
      CALL MZFORM('TMAC','9I 1F 1I -F',IOTMAC)
      CALL MZBOOK(IDVSTP,LTMAC,LTMAE,-IZTMAC,'TMAC',0,0,3014,IOTMAC,0)
C-------------------------------------------------------------------------------
C     BOOK STRUCTURE UNDER TPRO
C-------------------------------------------------------------------------------
      CALL MZFORM('TPR1','9I -F',IOTPR1)
      CALL MZBOOK(IDVSTP,LTPR1,LTPRO,-IZTPR1,'TPR1',7,7,10,IOTPR1,0)
      CALL MZFORM('TPR2','9I -F',IOTPR2)
      CALL MZBOOK(IDVSTP,LTPR2,LTPRO,-IZTPR2,'TPR2',7,7,10,IOTPR2,0)
      CALL MZFORM('TPET','9I 1F 1I -F',IOTPET)
      CALL MZFORM('TPE3','9I 1F 1I -F',IOTPE3)
      CALL MZFORM('TPTR','9I 1F 1I -F',IOTPTR)
      CALL MZFORM('TPT3','9I 1F 1I -F',IOTPT3)
      CALL MZFORM('TPLE','9I 1F 1I -F',IOTPLE)
      CALL MZFORM('TPEC','9I 1F',IOTPEC)
      CALL MZFORM('TPCA','9I 1F 1I -F',IOTPCA)
      CALL MZFORM('TPCB','9I 1F 1I -F',IOTPCB)
      CALL MZFORM('TPCC','9I 1F 1I -F',IOTPCC)
      LI(1)=LTPR1
      LI(2)=LTPR2
      DO I=1,2
        CALL MZBOOK(IDVSTP,LTPET,LI(I),-IZTPET,'TPET',0,0,113,IOTPET,0)
        CALL MZBOOK(IDVSTP,LTPE3,LI(I),-IZTPE3,'TPE3',0,0,113,IOTPE3,0)
        CALL MZBOOK(IDVSTP,LTPTR,LI(I),-IZTPTR,'TPTR',0,0,113,IOTPTR,0)
        CALL MZBOOK(IDVSTP,LTPT3,LI(I),-IZTPT3,'TPT3',0,0,113,IOTPT3,0)
        CALL MZBOOK(IDVSTP,LTPLE,LI(I),-IZTPLE,'TPLE',0,0,113,IOTPLE,0)
C        CALL MZBOOK(IDVSTP,LTPEC,LI(I),-IZTPEC,'TPEC',3,3,10,IOTPEC,0)
C-------------------------------------------------------------------------------
C     BOOK STRUCTURE UNDER TPEC
C-------------------------------------------------------------------------------
        IF(LTPEC.NE.0)THEN
          CALL MZBOOK(IDVSTP,LTPCA,LTPEC,-IZTPCA,'TPCA',0,0,113,IOTPCA,
     &      0)
          CALL MZBOOK(IDVSTP,LTPCB,LTPEC,-IZTPCB,'TPCB',0,0,113,IOTPCB,
     &      0)
          CALL MZBOOK(IDVSTP,LTPCC,LTPEC,-IZTPCC,'TPCC',0,0,113,IOTPCC,
     &      0)
        END IF
      END DO
C-------------------------------------------------------------------------------
C     BOOK GEOMETRY BANKS
C-------------------------------------------------------------------------------
C-------------------------------------------------------------------------------
C     ACTIVE VOLUME GEOMETRY
C-------------------------------------------------------------------------------
      CALL MZFORM ('TACH','-I',IOTACH)
      CALL MZBOOK(IDVSTP,LTACH,LTGEO,-IZTACH,'TACH',3,3,8,IOTACH,0)
      CALL MZFORM('TRAC','9F 3I -F',IOTRAC)
      CALL MZBOOK(IDVSTP,LTRAC,LTACH,-1,'TRAC',0,0,25,IOTRAC,0)
      CALL MZBOOK(IDVSTP,LTRAC,LTACH,-2,'TRAC',0,0,25,IOTRAC,0)
      CALL MZBOOK(IDVSTP,LTRAC,LTACH,-3,'TRAC',0,0,25,IOTRAC,0)
      CALL MZBOOK(IDVSTP,LTACH,LTGEO,-IZTACH,'TACH',3,3,8,IOTACH,0)
      CALL MZBOOK(IDVSTP,LTRAC,LTACH,-1,'TRAC',0,0,25,IOTRAC,0)
      CALL MZBOOK(IDVSTP,LTRAC,LTACH,-2,'TRAC',0,0,25,IOTRAC,0)
      CALL MZBOOK(IDVSTP,LTRAC,LTACH,-3,'TRAC',0,0,25,IOTRAC,0)
C-------------------------------------------------------------------------------
C     CALENDAR BANKS (2 VERSIONS OF TDMC, 1 VERSION OF TDDA)
C-------------------------------------------------------------------------------
      CALL MZFORM ('TDMC','-I',IOTDMC)
      CALL MZBOOK(IDVSTP,LTDMC,LTGEO,-IZTDMC,'TDMC',0,0,5,IOTDMC,0)
      CALL MZBOOK(IDVSTP,LTDMC,LTGEO,-IZTDMC,'TDMC',0,0,5,IOTDMC,0)
      CALL MZFORM ('TDDA','-I',IOTDDA)
      CALL MZBOOK(IDVSTP,LTDDA,LTGEO,-IZTDDA,'TDDA',0,0,5,IOTDDA,0)
C-------------------------------------------------------------------------------
C     BOOK STRUCTURE UNDER TCAN
C-------------------------------------------------------------------------------
      CALL MZFORM ('TCY1','-I',IOTCY1)
      CALL MZBOOK(IDVSTP,LTCY1,LTCAN,-IZTCY1,'TCY1',0,0,4000,IOTCY1,0)
C-------------------------------------------------------------------------------
C     LIKELIHOOD BANKS
C-------------------------------------------------------------------------------
      CALL MZFORM('TLIK','-F',IXIO)        ! Describe Bank format
      CALL MZBOOK(IDVSTP,LTLIK,LTGEN,-IZTLIK,'TLIK',3,3,5,IXIO,0)
      CALL MZFORM('TPIO','-F',IXIO)        ! Describe Bank format
      CALL MZBOOK(IDVSTP,LTPIO,LTLIK,-IZTPIO,'TPIO',3,3,5,IXIO,0)
      CALL MZFORM('TELE','-F',IXIO)        ! Describe Bank format
      CALL MZBOOK(IDVSTP,LTELE,LTLIK,-IZTELE,'TELE',3,3,5,IXIO,0)
      CALL MZFORM('TEL1','-F',IXIO)        ! Describe Bank format
      CALL MZBOOK(IDVSTP,LTEL1,LTELE,-IZTEL1,'TEL1',0,0,310,IXIO,0)
      CALL MZFORM('TEL2','-F',IXIO)        ! Describe Bank format
      CALL MZBOOK(IDVSTP,LTEL2,LTELE,-IZTEL2,'TEL2',0,0,310,IXIO,0)
      CALL MZFORM('TPI1','-F',IXIO)        ! Describe Bank format
      CALL MZBOOK(IDVSTP,LTPI1,LTPIO,-IZTPI1,'TPI1',0,0,310,IXIO,0)
      CALL MZFORM('TPI2','-F',IXIO)        ! Describe Bank format
      CALL MZBOOK(IDVSTP,LTPI2,LTPIO,-IZTPI2,'TPI2',0,0,310,IXIO,0)
      CALL MZFORM('TPDE','-F',IXIO)        ! Describe Bank format
      CALL MZBOOK(IDVSTP,LTPDE,LTPIO,-IZTPDE,'TPDE',0,0,110,IXIO,0)
      CALL MZFORM('TEDE','-F',IXIO)        ! Describe Bank format
      CALL MZBOOK(IDVSTP,LTEDE,LTELE,-IZTEDE,'TEDE',0,0,110,IXIO,0)
      CALL MZFORM('TLTA','-F',IXIO)        ! Describe Bank format
      CALL MZBOOK(IDVSTP,LTLTA,LTLIK,-IZTLTA,'TLTA',0,0,3220,IXIO,0)
C
C-------------------------------------------------------------------------------
C     DENSITY BANKS
C-------------------------------------------------------------------------------
      CALL MZFORM('TDEN','-F',IXIO)        ! Describe Bank format
      CALL MZBOOK(IDVSTP,LTDEN,LTGEN,-IZTDEN,'TDEN',5,5,5,IXIO,0)
      CALL MZFORM('TDOR','-F',IXIO)        ! Describe Bank format
      CALL MZBOOK(IDVSTP,LTDOR,LTDEN,-IZTDOR,'TDOR',0,0,8000,IXIO,0)
      CALL MZFORM('TELE','-F',IXIO)        ! Describe Bank format
      CALL MZBOOK(IDVSTP,LTDPI,LTDEN,-IZTDPI,'TDPI',0,0,820,IXIO,0)
      CALL MZFORM('TEL1','-F',IXIO)        ! Describe Bank format
      CALL MZBOOK(IDVSTP,LTDFA,LTDEN,-IZTDFA,'TDFA',0,0,820,IXIO,0)
      CALL MZFORM('TEL2','-F',IXIO)        ! Describe Bank format
      CALL MZBOOK(IDVSTP,LTDEL,LTDEN,-IZTDEL,'TDEL',0,0,820,IXIO,0)
      CALL MZFORM('TPI1','-F',IXIO)        ! Describe Bank format
      CALL MZBOOK(IDVSTP,LTDCO,LTDEN,-IZTDCO,'TDCO',0,0,820,IXIO,0)
C
  999 RETURN
      END

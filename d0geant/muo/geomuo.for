      LOGICAL FUNCTION GEOMUO()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Defines geometry of the D0 muon system.
C-     The geometry parameters are taken from muon constant Zebra
C-  banks, MGEH and MGEO for PDT modules and MFEH and MFEG for
C-  magnetized iron.
C-
C-     The geometry tree structure is following:
C-
C-    MVOL     D0 mother volume.
C-     I
C-     I--PIPE     beam pipe.
C-     I
C-     I--CMxx     muon PDT module in C layer.     (xx module number)
C-     I   I--CPxx(3)         four deck of muon chamber.
C-     I       I--CCxx(24)    24 cells in a deck.
C-     I
C-     I--BMxx     muon PDT module in B layer.     (xx module number)
C-     I   I--BPxx(3)         four deck of muon chamber.
C-     I       I--BCxx(24)    24 cells in a deck.
C-     I
C-     I--MCSB     central iron slabs            (1:4)
C-     I--MCCR     central iron slabs at corner  (1:4)
C-     I--METD     end iron slabs    (1:4,z>0 AND 5:8,Z<0)
C-     I
C-     I--AMxx    muon PDT module in A layer.     (xx modue number)
C-     I   I--APxx(4)         four deck of muon chamber.
C-     I       I--ACxx(24)    24 cells in a deck.
C-     I
C-     I--MCAL        mother volume for CAL including CEN.
C-         I   I--MCEN    mother volume for CEN.
C-         I   I--....    (many CAL volumes)
C-
C-   Returned value : TRUE
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created     JAN-1987   S.Kunori
C-   Updated     NOV-1988   D.Hedin     modify MUCELL call
C-   Updated  17-JUL-1989   N.A.Graf    Ordered various volumes with GSORD
C-   Updated  18-JUL-1989   N.A.Graf    Include mother volume MMUO.
C-   Updated   4-AUG-1989   Alan M. Jonckheere  Update for PBD
C-   Updated   9-mar-1992   S. Abachi  SD0(3) used to flag creation of
C-                                     MURECO_GSAVE.DAT by modifying this code.
C-   Updated  13-SEP-1992   S.Igarashi  GSPOS -> GSPOSP for Magnets
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:D0LOG.INC/LIST'      ! to get SMUO switch.
C
      INTEGER IM,IP,ICL,I,J,K,IVOL,ISLAB(3),ISTAT
      CHARACTER*4 HS1,HS2,HS3
      CHARACTER*4 VMNAME,VPNAME,VCNAME     !  volume name for module
C                                          !  palne and cell.
      INTEGER     NS1,NS2,NS3
      REAL        SPWORK(3),VOFF
      REAL        SPAR1(3),XPAR1(3),ROTM1(3,3)
      REAL        SPAR2(3),XPAR2(3),ROTM2(3,3)
      REAL        XPLOC(3),XPLAB(3)
      REAL        SPAR3(3),XPAR3(3),ROTM3(3,3)
      REAL        TH1,PH1,TH2,PH2,TH3,PH3
      REAL        TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN
      INTEGER     NBUFF,IBUFF(1)
      INTEGER     GZMMAG
      DATA TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN/.5,2.,.1,.05,.05/
      DATA ISLAB/3*0/
      DATA NBUFF/0/,IBUFF(1)/0/
C
C----------------------------------------------------------------------
C
      GEOMUO = .TRUE.
      IF ( DMUO.LT.1 ) GOTO 999
C
C  define Geant rotation matrix.
C  ================================
C
      CALL GSROTM(2000,90.,0.,90.,90.,0.,0.)    ! no rotation.
C
C  define materials.
C  ==================
C
      CALL GSMATE(26,'IRON TOROID$',55.850,26.00,7.870,1.760,17.100
     +           ,NBUFF,IBUFF)
      CALL GSMATE(27,'IRON TOROID$',55.850,26.00,7.870,1.760,17.100
     +           ,NBUFF,IBUFF)
      CALL GSMATE(28,'IRON TOROID$',55.850,26.00,7.870,1.760,17.100
     +           ,NBUFF,IBUFF)
      CALL GSMATE(29,'IRON TOROID$',55.850,26.00,7.870,1.760,17.100
     +           ,NBUFF,IBUFF)
C
C  define tracking media.
C  ======================
C
      CALL GSTMED(20,'MUONAIR$',15,1,0,0.,TMAXFD,DMAXMS,DEEMAX,
     A  EPSIL,STMIN,0,0)      !  AIR FOR NOW
      CALL GSTMED(21,'PDTALMINUM$',9,1,0,0.,TMAXFD,DMAXMS,DEEMAX,
     A  EPSIL,STMIN,0,0)      !  AIR FOR NOW
      CALL GSTMED(22,'PDTGAS$',15,1,0,0.,TMAXFD,DMAXMS,DEEMAX,
     A  EPSIL,STMIN,0,0)      !  AIR FOR NOW
      CALL GSTMED(26,'CFSLABBASE$',26,0,1,20.,TMAXFD,DMAXMS,DEEMAX,
     A  EPSIL,STMIN,0,0)
      CALL GSTMED(27,'EFTOROID$',27,0,1,20.,TMAXFD,DMAXMS,DEEMAX,
     A  EPSIL,STMIN,0,0)
      CALL GSTMED(28,'CFCORNER$',28,0,1,20.,TMAXFD,DMAXMS,DEEMAX,
     A  EPSIL,STMIN,0,0)
      CALL GSTMED(29,'CFSLAB$',29,0,1,20.,TMAXFD,DMAXMS,DEEMAX,
     A  EPSIL,STMIN,0,0)
C
C     -- reset physics processes in iron toroids...
C
      IF(SD0(3) .EQ. 1.0) GOTO 19   !See below
C
      IF(.FALSE.) THEN
        DO 10 I=26,29
          CALL GSTPAR(I,'CUTGAM',0.2)
          CALL GSTPAR(I,'CUTELE',0.2)
          CALL GSTPAR(I,'CUTNEU',0.2)
          CALL GSTPAR(I,'CUTHAD',0.2)
C       -- 2. gamma stopped, no pair generated...
          CALL GSTPAR(I,'PAIR',2.)
C       -- 2. gamma not stopped, no e- generated...
          CALL GSTPAR(I,'COMP',2.)
C       -- 2. gamma stopped, no e- generated...
          CALL GSTPAR(I,'PHOT',2.)
C       -- 2. stopped, no secondary...
          CALL GSTPAR(I,'PFIS',2.)
C       -- 2. part. not stopped, no secondary generated...
          CALL GSTPAR(I,'DRAY',2.)
C       -- 2. e+ stopped, no gamma generated...
          CALL GSTPAR(I,'ANNI',2.)
C       -- 2. e- not stopped, no gamma generated...
          CALL GSTPAR(I,'BREM',2.)
C       -- 1. hadron stopped...
          CALL GSTPAR(I,'HADR',1.)
          CALL GSTPAR(I,'MUNU',1.)
          CALL GSTPAR(I,'DCAY',1.)
C       -- 2. Landau fluctuation...
          CALL GSTPAR(I,'LOSS',2.)
C       -- 1. Gaussian scattering...
          CALL GSTPAR(I,'MULS',1.)
   10   CONTINUE
      ENDIF
C
   19   CONTINUE
C
C  check the overlap of modules and iron toroids.
C  ==============================================
C
CCC      CALL MSVCHK(ISTAT)
C
C  construct iron troid geometry.
C  ==============================
C
C -- loop over iron slabs...
C
      DO 100 IM=1,28
C     -- fetch geometrical parameters for iron slab...
        CALL MUMAGS(IM,HS1,NS1,SPAR1,XPAR1,ROTM1,NBUFF,IBUFF)
C     -- check if module exists...
        IF(NS1.EQ.0) GO TO 101
C     -- iron slab geometry...
        CALL ROTGEA(ROTM1,TH1,PH1,TH2,PH2,TH3,PH3)
        CALL GSROTM(IM+2400,TH1,PH1,TH2,PH2,TH3,PH3)
C     -- define volume.
        IF(IM.LE.10) THEN
C           -- central slabs.
          ISLAB(1)=ISLAB(1)+1
          IF(ISLAB(1).EQ.1) THEN
            IF(GZMMAG(4).NE.0) THEN
C              CALL GSVOLU('MCSB',HS1,29,SPAR1,NS1,IVOL)
              CALL GSVOLU('MCSB',HS1,29,SPAR1,0,IVOL)
            ELSE
C              CALL GSVOLU('MCSB',HS1,26,SPAR1,NS1,IVOL)!TEMPORARY
              CALL GSVOLU('MCSB',HS1,26,SPAR1,0,IVOL)!TEMPORARY
            ENDIF
          ENDIF
C          CALL GSPOS('MCSB',ISLAB(1),'MMUO'
C     +                ,XPAR1(1),XPAR1(2),XPAR1(3),IM+2400,'ONLY')
          CALL GSPOSP('MCSB',ISLAB(1),'MMUO'
     +                ,XPAR1(1),XPAR1(2),XPAR1(3),IM+2400,'ONLY'
     +                ,SPAR1,NS1)
C
        ELSE IF(IM.GE.11.AND.IM.LE.20) THEN
C           -- corner slabs.
          ISLAB(2)=ISLAB(2)+1
          IF(ISLAB(2).EQ.1) THEN
C            CALL GSVOLU('MCCR',HS1,28,SPAR1,NS1,IVOL)
            CALL GSVOLU('MCCR',HS1,28,SPAR1,0,IVOL)
          ENDIF
C          CALL GSPOS('MCCR',ISLAB(2),'MMUO'
C     +                ,XPAR1(1),XPAR1(2),XPAR1(3),IM+2400,'ONLY')
          CALL GSPOSP('MCCR',ISLAB(2),'MMUO'
     +                ,XPAR1(1),XPAR1(2),XPAR1(3),IM+2400,'ONLY'
     +                ,SPAR1,NS1)
C
C
        ELSE IF(IM.GE.21.AND.IM.LE.30) THEN
C           -- end slabs.
          ISLAB(3)=ISLAB(3)+1
          IF(ISLAB(3).EQ.1) THEN
C            CALL GSVOLU('METD',HS1,27,SPAR1,NS1,IVOL)
            CALL GSVOLU('METD',HS1,27,SPAR1,0,IVOL)
          ENDIF
C          CALL GSPOS('METD',ISLAB(3),'MMUO'
C     +                ,XPAR1(1),XPAR1(2),XPAR1(3),IM+2400,'ONLY')
          CALL GSPOSP('METD',ISLAB(3),'MMUO'
     +                ,XPAR1(1),XPAR1(2),XPAR1(3),IM+2400,'ONLY'
     +                ,SPAR1,NS1)
        ENDIF

  101   CONTINUE
  100 CONTINUE
C
      IF(SD0(3) .EQ. 1.0) GOTO 20      ! CAUTION
C
C     ! SD0(3)=1 Should be chosen only when attempting
C     ! to create the STP file MURECO_GSAVE.DAT for
C     ! mureco package. In this case all PBD modules
C     ! are excluded and GSORD calls are not made.
C-
C  construct muon PDT module geometry.
C  ===================================
C
C -- loop over moules.
C
      DO 200 IM=1,307
        CALL MUMODU(IM,HS1,NS1,SPAR1,XPAR1,ROTM1,NBUFF,IBUFF)
C    -- check if the module exists...
        IF(NS1.EQ.0) GO TO 201
C
C    -- module geometry...
C
        CALL ROTGEA(ROTM1,TH1,PH1,TH2,PH2,TH3,PH3)
        CALL GSROTM(IM+2000,TH1,PH1,TH2,PH2,TH3,PH3)
C       -- get module name in charcter string,
C          AMxx, BMxx, CMxx...
        CALL MUMODC(IM,VMNAME,VPNAME,VCNAME)
C       -- define Geant volume for module...
        CALL GSVOLU(VMNAME,HS1,20,SPAR1,NS1,IVOL)
C       -- place the volume...
C        IF(VMNAME.EQ.'CM05'.OR.
C     +     VMNAME.EQ.'CM06'.OR.
C     +     VMNAME.EQ.'CM15'.OR.
C     +     VMNAME.EQ.'CM16'.OR.
C     +     VMNAME.EQ.'CM35'.OR.
C     +     VMNAME.EQ.'CM36'.OR.
C     +     VMNAME.EQ.'CM45'.OR.
C     +     VMNAME.EQ.'CM46') THEN
C          CALL GSPOS(VMNAME,1,'MVOL',
C     +             XPAR1(1),XPAR1(2),XPAR1(3),IM+2000,'ONLY')
C        ELSE
        CALL GSPOS(VMNAME,1,'MMUO',
     +             XPAR1(1),XPAR1(2),XPAR1(3),IM+2000,'ONLY')
C        ENDIF
C
C     -- loop over planes...
C
        DO 210 IP=1,4
          CALL MUPLAN(IM,IP-1,HS2,NS2,SPAR2,XPAR2,ROTM2
     +                ,NBUFF,IBUFF)
C          -- check if the plane exists...
          IF(NS2.EQ.0) GO TO 211
C       -- define volume for plane for IP=1, assuming all plane
C       -- in a module are same...
          IF(IP.EQ.1) THEN
            CALL GSVOLU(VPNAME,HS2,21,SPAR2,3,IVOL)
          ENDIF
C       -- position of plane in local coordinate system...
          DO 212 I=1,3
            XPLAB(I)=XPAR2(I)-XPAR1(I)
  212     CONTINUE
C       -- rotate to module local system...
          CALL VMATL(ROTM1,XPLAB,XPLOC,3,3)
          CALL GSPOS(VPNAME,IP,VMNAME,XPLOC(1),XPLOC(2),XPLOC(3)
     +               ,2000,'ONLY')
C             -- set planes as unseen...
          IF(SMUO(1).LT.0.5) THEN
            CALL GSATT(VPNAME,'SEEN',0)
          ELSE
            CALL GSATT(VPNAME,'SEEN',1)
          ENDIF
C          -- define cell volume and position it in plane.
C          -- this is done only for first plane assuming internal
C          -- structure are same in all plane...
          IF(IP.EQ.1) THEN
            DO 220 ICL=1,24
              CALL MUCELL(IM,IP-1,ICL-1,HS3,NS3,SPAR3,XPAR3,ROTM3
     +                      ,VOFF,NBUFF,IBUFF)
C             -- check if the cell exists...
              IF(NS3.EQ.0) GO TO 221
C             -- define volume...
              CALL GSVOLU(VCNAME,HS3,22,SPAR3,3,IVOL)
C             -- position of cell into plane...
              DO 222 I=1,3
                XPLAB(I)=XPAR3(I)-XPAR2(I)
  222         CONTINUE
C             -- transform to plane local system...
              CALL VMATL(ROTM1,XPLAB,XPLOC,3,3)
              CALL GSPOS(VCNAME,ICL,VPNAME,
     +                      XPLOC(1),XPLOC(2),XPLOC(3),2000,'ONLY')
C             -- set cells as unseen...
              IF(SMUO(1).LT.1.5) THEN
                CALL GSATT(VCNAME,'SEEN',0)
              ELSE
                CALL GSATT(VCNAME,'SEEN',1)
              ENDIF
  221         CONTINUE
  220       CONTINUE
          ENDIF
C
  211     CONTINUE
  210   CONTINUE
  201   CONTINUE
  200 CONTINUE
C
C
C      Muon System GSORD calls
C
      CALL GSORD('AM10',1)
      CALL GSORD('AP10',2)
      CALL GSORD('AM11',1)
      CALL GSORD('AP11',2)
      CALL GSORD('AM12',1)
      CALL GSORD('AP12',2)
      CALL GSORD('AM13',1)
      CALL GSORD('AP13',2)
      CALL GSORD('AM15',1)
      CALL GSORD('AP15',2)
      CALL GSORD('AM15',1)
      CALL GSORD('AP15',2)
      CALL GSORD('AM16',1)
      CALL GSORD('AP16',2)
C
      CALL GSORD('AM20',1)
      CALL GSORD('AP20',2)
      CALL GSORD('AM21',1)
      CALL GSORD('AP21',2)
      CALL GSORD('AM22',1)
      CALL GSORD('AP22',2)
      CALL GSORD('AM23',1)
      CALL GSORD('AP23',2)
      CALL GSORD('AM25',1)
      CALL GSORD('AP25',2)
      CALL GSORD('AM25',1)
      CALL GSORD('AP25',2)
      CALL GSORD('AM26',1)
      CALL GSORD('AP26',2)
C
      CALL GSORD('AM30',1)
      CALL GSORD('AP30',2)
      CALL GSORD('AM31',1)
      CALL GSORD('AP31',2)
      CALL GSORD('AM32',1)
      CALL GSORD('AP32',2)
      CALL GSORD('AM33',1)
      CALL GSORD('AP33',2)
      CALL GSORD('AM35',1)
      CALL GSORD('AP35',2)
      CALL GSORD('AM35',1)
      CALL GSORD('AP35',2)
      CALL GSORD('AM36',1)
      CALL GSORD('AP36',2)
C
      CALL GSORD('AM61',1)
      CALL GSORD('AP61',2)
      CALL GSORD('AM62',1)
      CALL GSORD('AP62',2)
      CALL GSORD('AM64',1)
      CALL GSORD('AP64',2)
      CALL GSORD('AM65',1)
      CALL GSORD('AP65',2)
      CALL GSORD('AM67',1)
      CALL GSORD('AP67',2)
C
      CALL GSORD('AM91',1)
      CALL GSORD('AP91',2)
      CALL GSORD('AM92',1)
      CALL GSORD('AP92',2)
      CALL GSORD('AM94',1)
      CALL GSORD('AP94',2)
      CALL GSORD('AM95',1)
      CALL GSORD('AP95',2)
      CALL GSORD('AM97',1)
      CALL GSORD('AP97',2)
C
      CALL GSORD('BM00',1)
      CALL GSORD('BP00',2)
      CALL GSORD('BM01',1)
      CALL GSORD('BP01',2)
      CALL GSORD('BM02',1)
      CALL GSORD('BP02',2)
      CALL GSORD('BM03',1)
      CALL GSORD('BP03',2)
      CALL GSORD('BM04',1)
      CALL GSORD('BP04',2)
      CALL GSORD('BM05',1)
      CALL GSORD('BP05',2)
      CALL GSORD('BM06',1)
      CALL GSORD('BP06',2)
      CALL GSORD('BM07',1)
      CALL GSORD('BP07',2)
C
      CALL GSORD('BM10',1)
      CALL GSORD('BP10',2)
      CALL GSORD('BM11',1)
      CALL GSORD('BP11',2)
      CALL GSORD('BM12',1)
      CALL GSORD('BP12',2)
      CALL GSORD('BM13',1)
      CALL GSORD('BP13',2)
      CALL GSORD('BM14',1)
      CALL GSORD('BP14',2)
      CALL GSORD('BM15',1)
      CALL GSORD('BP15',2)
      CALL GSORD('BM16',1)
      CALL GSORD('BP16',2)
      CALL GSORD('BM17',1)
      CALL GSORD('BP17',2)
C
      CALL GSORD('BM20',1)
      CALL GSORD('BP20',2)
      CALL GSORD('BM21',1)
      CALL GSORD('BP21',2)
      CALL GSORD('BM22',1)
      CALL GSORD('BP22',2)
      CALL GSORD('BM23',1)
      CALL GSORD('BP23',2)
      CALL GSORD('BM24',1)
      CALL GSORD('BP24',2)
      CALL GSORD('BM27',1)
      CALL GSORD('BP27',2)
C
      CALL GSORD('BM30',1)
      CALL GSORD('BP30',2)
      CALL GSORD('BM31',1)
      CALL GSORD('BP31',2)
      CALL GSORD('BM32',1)
      CALL GSORD('BP32',2)
      CALL GSORD('BM33',1)
      CALL GSORD('BP33',2)
      CALL GSORD('BM34',1)
      CALL GSORD('BP34',2)
      CALL GSORD('BM35',1)
      CALL GSORD('BP35',2)
      CALL GSORD('BM36',1)
      CALL GSORD('BP36',2)
      CALL GSORD('BM37',1)
      CALL GSORD('BP37',2)
C
      CALL GSORD('BM40',1)
      CALL GSORD('BP40',2)
      CALL GSORD('BM41',1)
      CALL GSORD('BP41',2)
      CALL GSORD('BM42',1)
      CALL GSORD('BP42',2)
      CALL GSORD('BM43',1)
      CALL GSORD('BP43',2)
      CALL GSORD('BM44',1)
      CALL GSORD('BP44',2)
      CALL GSORD('BM45',1)
      CALL GSORD('BP45',2)
      CALL GSORD('BM46',1)
      CALL GSORD('BP46',2)
      CALL GSORD('BM47',1)
      CALL GSORD('BP47',2)
C
      CALL GSORD('BM50',1)
      CALL GSORD('BP50',2)
      CALL GSORD('BM53',1)
      CALL GSORD('BP53',2)
C
      CALL GSORD('BM60',1)
      CALL GSORD('BP60',2)
      CALL GSORD('BM61',1)
      CALL GSORD('BP61',2)
      CALL GSORD('BM62',1)
      CALL GSORD('BP62',2)
      CALL GSORD('BM63',1)
      CALL GSORD('BP63',2)
      CALL GSORD('BM64',1)
      CALL GSORD('BP64',2)
      CALL GSORD('BM65',1)
      CALL GSORD('BP65',2)
      CALL GSORD('BM66',1)
      CALL GSORD('BP66',2)
      CALL GSORD('BM67',1)
      CALL GSORD('BP67',2)
C
      CALL GSORD('BM80',1)
      CALL GSORD('BP80',2)
      CALL GSORD('BM83',1)
      CALL GSORD('BP83',2)
C
      CALL GSORD('BM90',1)
      CALL GSORD('BP90',2)
      CALL GSORD('BM91',1)
      CALL GSORD('BP91',2)
      CALL GSORD('BM92',1)
      CALL GSORD('BP92',2)
      CALL GSORD('BM93',1)
      CALL GSORD('BP93',2)
      CALL GSORD('BM94',1)
      CALL GSORD('BP94',2)
      CALL GSORD('BM95',1)
      CALL GSORD('BP95',2)
      CALL GSORD('BM96',1)
      CALL GSORD('BP96',2)
      CALL GSORD('BM97',1)
      CALL GSORD('BP97',2)
C
      CALL GSORD('CM00',1)
      CALL GSORD('CP00',2)
      CALL GSORD('CM01',1)
      CALL GSORD('CP01',2)
      CALL GSORD('CM02',1)
      CALL GSORD('CP02',2)
      CALL GSORD('CM03',1)
      CALL GSORD('CP03',2)
      CALL GSORD('CM04',1)
      CALL GSORD('CP04',2)
      CALL GSORD('CM05',1)
      CALL GSORD('CP05',2)
      CALL GSORD('CM06',1)
      CALL GSORD('CP06',2)
      CALL GSORD('CM07',1)
      CALL GSORD('CP07',2)
C
      CALL GSORD('CM10',1)
      CALL GSORD('CP10',2)
      CALL GSORD('CM11',1)
      CALL GSORD('CP11',2)
      CALL GSORD('CM12',1)
      CALL GSORD('CP12',2)
      CALL GSORD('CM13',1)
      CALL GSORD('CP13',2)
      CALL GSORD('CM14',1)
      CALL GSORD('CP14',2)
      CALL GSORD('CM15',1)
      CALL GSORD('CP15',2)
      CALL GSORD('CM16',1)
      CALL GSORD('CP16',2)
      CALL GSORD('CM17',1)
      CALL GSORD('CP17',2)
C
      CALL GSORD('CM20',1)
      CALL GSORD('CP20',2)
      CALL GSORD('CM21',1)
      CALL GSORD('CP21',2)
      CALL GSORD('CM22',1)
      CALL GSORD('CP22',2)
      CALL GSORD('CM23',1)
      CALL GSORD('CP23',2)
      CALL GSORD('CM24',1)
      CALL GSORD('CP24',2)
      CALL GSORD('CM27',1)
      CALL GSORD('CP27',2)
C
      CALL GSORD('CM30',1)
      CALL GSORD('CP30',2)
      CALL GSORD('CM31',1)
      CALL GSORD('CP31',2)
      CALL GSORD('CM32',1)
      CALL GSORD('CP32',2)
      CALL GSORD('CM33',1)
      CALL GSORD('CP33',2)
      CALL GSORD('CM34',1)
      CALL GSORD('CP34',2)
      CALL GSORD('CM35',1)
      CALL GSORD('CP35',2)
      CALL GSORD('CM36',1)
      CALL GSORD('CP36',2)
      CALL GSORD('CM37',1)
      CALL GSORD('CP37',2)
C
      CALL GSORD('CM40',1)
      CALL GSORD('CP40',2)
      CALL GSORD('CM41',1)
      CALL GSORD('CP41',2)
      CALL GSORD('CM42',1)
      CALL GSORD('CP42',2)
      CALL GSORD('CM43',1)
      CALL GSORD('CP43',2)
      CALL GSORD('CM44',1)
      CALL GSORD('CP44',2)
      CALL GSORD('CM45',1)
      CALL GSORD('CP45',2)
      CALL GSORD('CM46',1)
      CALL GSORD('CP46',2)
      CALL GSORD('CM47',1)
      CALL GSORD('CP47',2)
C
      CALL GSORD('CM50',1)
      CALL GSORD('CP50',2)
      CALL GSORD('CM51',1)
      CALL GSORD('CP51',2)
      CALL GSORD('CM53',1)
      CALL GSORD('CP53',2)
      CALL GSORD('CM55',1)
      CALL GSORD('CP55',2)
C
      CALL GSORD('CM60',1)
      CALL GSORD('CP60',2)
      CALL GSORD('CM61',1)
      CALL GSORD('CP61',2)
      CALL GSORD('CM62',1)
      CALL GSORD('CP62',2)
      CALL GSORD('CM63',1)
      CALL GSORD('CP63',2)
      CALL GSORD('CM64',1)
      CALL GSORD('CP64',2)
      CALL GSORD('CM65',1)
      CALL GSORD('CP65',2)
      CALL GSORD('CM66',1)
      CALL GSORD('CP66',2)
      CALL GSORD('CM67',1)
      CALL GSORD('CP67',2)
C
      CALL GSORD('CM70',1)
      CALL GSORD('CP70',2)
      CALL GSORD('CM71',1)
      CALL GSORD('CP71',2)
      CALL GSORD('CM72',1)
      CALL GSORD('CP72',2)
      CALL GSORD('CM73',1)
      CALL GSORD('CP73',2)
      CALL GSORD('CM74',1)
      CALL GSORD('CP74',2)
      CALL GSORD('CM75',1)
      CALL GSORD('CP75',2)
      CALL GSORD('CM76',1)
      CALL GSORD('CP76',2)
      CALL GSORD('CM77',1)
      CALL GSORD('CP77',2)
C
      CALL GSORD('CM80',1)
      CALL GSORD('CP80',2)
      CALL GSORD('CM81',1)
      CALL GSORD('CP81',2)
      CALL GSORD('CM83',1)
      CALL GSORD('CP83',2)
      CALL GSORD('CM85',1)
      CALL GSORD('CP85',2)
C
      CALL GSORD('CM90',1)
      CALL GSORD('CP90',2)
      CALL GSORD('CM91',1)
      CALL GSORD('CP91',2)
      CALL GSORD('CM92',1)
      CALL GSORD('CP92',2)
      CALL GSORD('CM93',1)
      CALL GSORD('CP93',2)
      CALL GSORD('CM94',1)
      CALL GSORD('CP94',2)
      CALL GSORD('CM95',1)
      CALL GSORD('CP95',2)
      CALL GSORD('CM96',1)
      CALL GSORD('CP96',2)
      CALL GSORD('CM97',1)
      CALL GSORD('CP97',2)
C
      CALL GSORD('CMA0',1)
      CALL GSORD('CPA0',2)
      CALL GSORD('CMA1',1)
      CALL GSORD('CPA1',2)
      CALL GSORD('CMA2',1)
      CALL GSORD('CPA2',2)
      CALL GSORD('CMA3',1)
      CALL GSORD('CPA3',2)
      CALL GSORD('CMA4',1)
      CALL GSORD('CPA4',2)
      CALL GSORD('CMA5',1)
      CALL GSORD('CPA5',2)
      CALL GSORD('CMA6',1)
      CALL GSORD('CPA6',2)
      CALL GSORD('CMA7',1)
      CALL GSORD('CPA7',2)
C
Ccc      CALL GSORD('MMUO',3)
C
C ****  Setup Digitizations and Sets
      IF ( DMUO.GE.2 ) CALL DETMUO
C
   20 CONTINUE
C
  999 RETURN
      END

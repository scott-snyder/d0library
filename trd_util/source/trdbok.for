      SUBROUTINE TRDBOK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOK TRD HISTOGRAMS
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-OCT-1989   A. Zylberstejn
C-   Updated  20-NOV-1991   A. Zylberstejn  : Introduce booking of histos
C-                                            for verification
C-   Updated  10-MAR-1992   A. Zylberstejn  Normalize all calls to HBOOK1
C-   Updated  22-OCT-1992   A. Zylberstejn
C-   Updated  20-SEP-1993   A. Zylberstejn  Updated for 512 cells in layer 3
C-   Updated   4-JAN-1994   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INTEGER NBLAY,NBTIT,i,ierr
      INTEGER ICH,K,IK,IDSH,IER,KL,FIRST_VER
      REAL VMX
      PARAMETER( NBLAY = 8 )
      PARAMETER( NBTIT = 3 )
      INCLUDE 'D0$INC:FIRSHT.INC'
      CHARACTER*4 IDENT(2)
      CHARACTER*3 HBDIR(1)
      CHARACTER*4 C4
      CHARACTER*26 FILNAM
      CHARACTER*30 TRHNAM
      CHARACTER*50       EFFTYP(8)
      CHARACTER*8 NUMLAY(NBLAY)
      CHARACTER*10 TITRE(NBTIT)
C      INTEGER DO_HISTO(4),IHBDIR(1)
      INTEGER IAUX(14)
      CHARACTER*3 DO_HISTO(4)
C      integer IHBDIR(1)
      CHARACTER*3 SWITCH_HISTO(14,2)
      CHARACTER*4 SWHISG(12)
      INTEGER     ISWHSG(12)
      CHARACTER*10 DIRNAM
      CHARACTER*3 C3
      character*3 car3
C      EQUIVALENCE (IHBDIR(1),HBDIR(1))
C      EQUIVALENCE (ISWHSG,SWHISG)
      LOGICAL FIRST,FLGVAL,REDUCED_SET
      DATA FIRST/.TRUE./
      DATA IDENT/'PION','ELEC'/
      DATA NUMLAY / 'Layer 1','Layer 2','Layer 3',
     &              'Strip 1','Strip 2','Strip 3',
     &              'ANODE  ','CATHODE'/
C      DATA NUMWIR /
C     &  'Wire 0','Wire 1','Wire 2','Wire 3','Wire 4','Wire 5','Wire 6',
C     &  'DL 0 -','DL 0 +','DL 1 -','DL 1 +'/
      DATA TITRE / 'E TOT ','E TRUNCA','LIK ETOT'/
      DATA VMX /0./
C----------------------------------------------------------------------
C
C            +-------------------+
C            |BOOK THE HISTOGRAMS|
C            +-------------------+
C       ICH=1,3 ANODES ICH=4,6 CATHODES   K=0 ANODES,K=1 CATHODES
C 1)PIONS
C  ------
C  FIRSHT+ICH    ENERGY FOR LAYER ICH
C  FIRSHT+6+ICH  NORMALIZED ENERGY
C  FIRSHT+13+K   NORMALIZED ENERGY PER LAYER (IRRESPECTIVE OF LAYER)
C  FIRSHT+25+K   NORMALIZED ENERGY 3 LAYERS
C  FIRSHT+27+K   TRUNCATED ENERGY 3 LAYERS
C  FIRSHT+29+K   LIKELIHOOD TOTAL ENERGY 3 LAYERS
C  FIRSHT+31+K   EFFICIENCY TOTAL ENERGY 3 LAYERS
C  FIRSHT+33+K   EFFICIENCY TOTAL ENERGY 3 LAYERS 3 CHAMBERS HIT
C  FIRSHT+35+K   EFFICIENCY TRUNCATED MEAN 3 LAYERS
C  FIRSHT+37+K   EFFICIENCY TRUNCATED MEAN 3 LAYERS 3 CHAMBERS HIT
C  FIRSHT+39+K   EFFICIENCY LIKELIHOOD TOTAL ENERGY
C  FIRSHT+41+K   EFFICIENCY LIKELIHOOD TOTAL ENERGY/NB. OF CLUSTERS
C                               CLUSTER ENERGY  THRESHOLD 0
C  FIRSHT+43+K   EFFICIENCY LIKELIHOOD TOTAL ENERGY/NB. OF CLUSTERS
C                               CLUSTER ENERGY  THRESHOLD 30
C  FIRSHT+45+K   EFFICIENCY LIKELIHOOD TOTAL ENERGY/NB. OF CLUSTERS
C                               CLUSTER ENERGY  THRESHOLD 60
C 2)ELECTRONS (ADD 100 TO THE ABOVE)
C  ----------
C     FIRSTEL=FIRSHT+100
C  FIRSTEL+ICH    ENERGY FOR LAYER ICH
C  FIRSTEL+6+ICH  NORMALIZED ENERGY
C  FIRSTEL+13+K   NORMALIZED ENERGY PER LAYER (IRRESPECTIVE OF LAYER)
C  FIRSTEL+25+K   NORMALIZED ENERGY 3 LAYERS
C  FIRSTEL+27+K   TRUNCATED ENERGY 3 LAYERS
C  FIRSTEL+29+K   LIKELIHOOD TOTAL ENERGY 3 LAYERS
C  FIRSTEL+31+K   EFFICIENCY TOTAL ENERGY 3 LAYERS
C  FIRSTEL+33+K   EFFICIENCY TOTAL ENERGY 3 LAYERS 3 CHAMBERS HIT
C  FIRSTEL+35+K   EFFICIENCY TRUNCATED MEAN 3 LAYERS
C  FIRSTEL+37+K   EFFICIENCY TRUNCATED MEAN 3 LAYERS 3 CHAMBERS HIT
C  FIRSTEL+39+K   EFFICIENCY LIKELIHOOD TOTAL ENERGY
C  FIRSTEL+41+K   EFFICIENCY LIKELIHOOD TOTAL ENERGY/NB. OF CLUSTERS
C                               CLUSTER ENERGY  THRESHOLD 0
C  FIRSTEL+43+K   EFFICIENCY LIKELIHOOD TOTAL ENERGY/NB. OF CLUSTERS
C                               CLUSTER ENERGY  THRESHOLD 30
C  FIRSTEL+45+K   EFFICIENCY LIKELIHOOD TOTAL ENERGY/NB. OF CLUSTERS
C                               CLUSTER ENERGY  THRESHOLD 60
C
C  GENERAL
C  -------
C    FIRSTG=FIRSHT+500
C  FIRSTG+ich Phiw track- Phi wire chamber ich      (swhisg(1))
C  FIRSTG+4   Clusters energy                       (swhisg(5))
C  FIRSTG+5   Arrival time                          (swhisg(5))
C  FIRSTG+6   Clusters energy versus arrival time   (swhisg(5))
C  FIRSTG+10+ich   Phiw rec- Phi GEANT chamber ich  (swhisg(2))
C  FIRSTG+21   N miss chamber ich                   (swhisg(4))
C  FIRSTG+22   N hit  chamber ich                   (swhisg(4))
C  FIRSTG+30+ich   Wire map chamber ich             (swhisg(3))
C
C Reduced set of histograms for fast checking
C  FIRST_VER=FIRSHT+1000
C  FIRST_VER+6+ICH    Normalized energy all tracks
C  FIRST_VER+39+K     Efficiency likelihood total energy  all tracks
C  FIRST_VER+106+ICH  Normalized energy electrons
C  FIRST_VER+139+K    Efficiency likelihood total energy electrons
C  FIRST_VER+50+K     Number of hits anodes +10*nb. of cathodes on
C                      the track all tracks
C  FIRST_VER+54       Nhit  all tracks
C  FIRST_VER+55       Nmiss  all tracks
C add 100 to the above number for electrons
C
C
C****  Reads in TRD_RCP file
      IF (FIRST) THEN
        CALL VFILL (SWHISG,12,'N')
C      CALL VFILL (DO_HISTO,4,'N')
        REDUCED_SET=FLGVAL('VERIFY')
        CALL EZPICK('TRD_RCP')
        CALL EZGET('HISTO_REDUCED_SET',I,IERR)
c        PRINT*,' in trdbok 2,reduce,i',I,' ierr',IERR
        IF(IERR.EQ.0)THEN
          CALL UHTOC(I,4,C4,4)
          REDUCED_SET=C4.EQ.'Y' .OR. C4.EQ.'y'
     &      .OR. C4.EQ.'YES'
c          PRINT*,' in trdbok 2 i=',I,' c4= ',C4,' reduced_set',
c     &      REDUCED_SET
        END IF
        CALL EZGET('HBOOK_DIRECTORY',K,IER)
        CALL UHTOC(K,5,HBDIR(1),3)
        CALL EZGET('HSTBOK',IAUX(1),IER)
        DO K=1,4
          IF(IAUX(K).NE.0)
     +      CALL UHTOC(IAUX(K),3,DO_HISTO(K),3)
        END DO
        FIRST = .FALSE.
        CALL HCDIR('//PAWC',' ')     !GO TO TOP DIRECTORY
        CALL HMDIR(HBDIR(1),'S')     !CREATE TRD DIRECTORY
      ELSE
        DIRNAM = '//PAWC/'//HBDIR(1)
        CALL HCDIR(DIRNAM ,' ')
      ENDIF
C     -------------------------------
      IF(DO_HISTO(1).NE.'Y')THEN
        GO TO 700
      END IF
C     -------------------------------
        reduced_set=.false.
      IF(DO_HISTO(2).EQ.'Y')THEN
        CALL VZERO(IAUX,14)
        CALL EZGET('PIONS_HISTOS'    ,IAUX ,IER)
        DO K=1,14
          IF(IAUX(K).NE.0)THEN
            CALL UHTOC(IAUX(K),3,SWITCH_HISTO(K,1),3)
          END IF
        END DO
      END IF
      IF(DO_HISTO(3).EQ.'Y')THEN
        CALL VZERO(IAUX,14)
        CALL EZGET('ELECTRONS_HISTOS',IAUX,IER)
        DO K=1,14
          IF(IAUX(K).NE.0)THEN
            CALL UHTOC(IAUX(K),3,SWITCH_HISTO(K,2),3)
          END IF
        END DO
      END IF
      IF(DO_HISTO(4).EQ.'Y')THEN
        CALL VZERO(IAUX,14)
        CALL EZGET('GENERAL_TRD_HISTOS',IAUX(1),IER)
        DO K=1,12
          IF(IAUX(K).NE.0)THEN
            CALL UHTOC(IAUX(K),4,SWHISG(K),4)
          END IF
        END DO
      END IF
C      CALL EZGET('GENERAL_TRD_HISTOS',I,IER)
C      IF (SWTDBG.EQ.1) THEN
C        WRITE(LUDEBG,*)'DIRECTORY NAME =',HBDIR(1)
C      ENDIF
C****
      EFFTYP(1)=' TOTAL ENERGY'
      EFFTYP(2)=' TOTAL ENERGY 3 CHAMBERS HIT'
      EFFTYP(3)=' TRUNCATED MEAN'
      EFFTYP(4)=' TRUNCATED MEAN 3 CHAMBERS HIT'
      EFFTYP(5)=' LIKELIHOOD ETOT'
      EFFTYP(6)=' LIKELIHOOD ETOT/NB. OF CLUSTERS THRESHOLD 0'
      EFFTYP(7)=' LIKELIHOOD ETOT/NB. OF CLUSTERS THRESHOLD 30'
      EFFTYP(8)=' LIKELIHOOD ETOT/NB. OF CLUSTERS THRESHOLD 60'
C
      IK=25                        !ONLY ANODES
      DO 60 K=1,2  !k=1 pions, k=2 electrons
        IF(DO_HISTO(K+1).NE.'Y')GO TO 60
        IDSH=100*(K-1)  ! shift for electrons
        DO 50 ICH=1,6!TOTAL DEPOSITED ENERGY PER LAYER
C          IF(ICH.GE.4 .AND. ICH.LE.6)GO TO 50  !SKIP HISTOS FOR CATHODES
          IF (SWITCH_HISTO(1,K).EQ.'Y') THEN ! RAW ENERGY DEPOSIT PER LAYER
            TRHNAM=TITRE(1)//NUMLAY(ICH)//IDENT(K)
            CALL HBOOK1(FIRSHT+ICH+IDSH, TRHNAM,100,0.,2000.,VMX)
          ENDIF
          IF (SWITCH_HISTO(2,K).EQ.'Y') THEN !CORRECTED ENERGIES
            TRHNAM=' NORMALIZED ENERGY '//NUMLAY(ICH)//IDENT(K)
            CALL HBOOK1(FIRSHT+6+ICH+IDSH,TRHNAM,100,0.,5.,VMX)
          ENDIF
   50   CONTINUE
        IF (SWITCH_HISTO(3,K).EQ.'Y') THEN
          TRHNAM= ' NORMALIZED ENERGY PER LAYER PIONS'//IDENT(K)
          CALL HBOOK1(FIRSHT+13+IDSH,TRHNAM,100,0.,5.,VMX)
        ENDIF
        IF (SWITCH_HISTO(4,K).EQ.'Y') THEN !ETOT
          TRHNAM=TITRE(1)//NUMLAY(IK-18)//IDENT(K)
          CALL HBOOK1(FIRSHT+25+IDSH, TRHNAM,100,0.,15.,VMX)
        ENDIF
        IF (SWITCH_HISTO(5,K).EQ.'Y') THEN !TRUNCATED MEAN
          TRHNAM=TITRE(2)//NUMLAY(IK-18)//IDENT(K)
          CALL HBOOK1(FIRSHT+27+IDSH, TRHNAM,100,0.,15.,VMX)
        ENDIF
        IF (SWITCH_HISTO(6,K).EQ.'Y') THEN !LIKELIHOOD ETOT
          TRHNAM=TITRE(3)//NUMLAY(IK-18)//IDENT(K)
          CALL HBOOK1(FIRSHT+29+IDSH, TRHNAM,100,-10.,10.,VMX)
        ENDIF
        DO KL=1,8  ! EFFICIENCIES
          IF (SWITCH_HISTO(KL+6,K).EQ.'Y') THEN
            CALL HBOOK1(FIRSHT+30+(2*KL-1)+IDSH,
     &        'EFFICIENCY '//EFFTYP(KL)//IDENT(K),100,0.,1.,VMX)
          ENDIF
        END DO
   60 CONTINUE
      IF(DO_HISTO(4).NE.'Y')GO TO 700 ! General histos
      IF (SWHISG(1).EQ.'Y') THEN
        CALL HBOOK1(FIRSHT+500+1,
     &    'ANGULAR DISTANCE  TRACK- WIRE TRD 1(DEG)$',100,-2.,2.,VMX)
        CALL HBOOK1(FIRSHT+500+2,
     &    'ANGULAR DISTANCE  TRACK- WIRE TRD 2(DEG)$',100,-2.,2.,VMX)
        CALL HBOOK1(FIRSHT+500+3,
     &    'ANGULAR DISTANCE  TRACK- WIRE TRD 3(DEG)$',100,-2.,2.,VMX)
C      CALL HBOOK2(firsht+551,'phiw-phitr versus phi layer 1$',
C     &    2,0.,360.,50,-2.,2.,VMX)
C      CALL HBOOK2(firsht+552,'phiw-phitr versus phi layer 2$',
C     &    2,0.,360.,50,-2.,2.,VMX)
C      CALL HBOOK2(firsht+553,'phiw-phitr versus phi layer 3$',
C     &    2,0.,360.,50,-2.,2.,VMX)
C      call hbsliy(firsht+551,2,0,)
C      call hbsliy(firsht+552,2,0,)
C      call hbsliy(firsht+553,2,0,)
        CALL HBOOK1(FIRSHT+525+1,
     &    'DPHI/DPHI CELL TRD 1(CELL BETWEEN -1 AND 1)$',100,-2.,2.,VMX)
        CALL HBOOK1(FIRSHT+525+2,
     &    'DPHI/DPHI CELL TRD 2$',100,-2.,2.,VMX)
        CALL HBOOK1(FIRSHT+525+3,
     &    'DPHI/DPHI CELL TRD 3$',100,-2.,2.,VMX)
      END IF
      IF (SWHISG(2).EQ.'Y') THEN
        CALL HBOOK1(FIRSHT+500+11,
     &    'PHI REC - PHI GEANT TRD 1$',100,-1.5,1.5,VMX)
        CALL HBOOK1(FIRSHT+500+12,
     &    'PHI REC - PHI GEANT TRD 2$',100,-1.5,1.5,VMX)
        CALL HBOOK1(FIRSHT+500+13,
     &    'PHI REC - PHI GEANT TRD 3$',100,-1.5,1.5,VMX)
      ENDIF
      IF (SWHISG(3).EQ.'Y') THEN
        CALL HBOOK1(FIRSHT+530+1,'WIRE MAP  TRD 1$',256,1.,257.,VMX)
        CALL HBOOK1(FIRSHT+530+2,'WIRE MAP  TRD 2$',256,1.,257.,VMX)
        CALL HBOOK1(FIRSHT+530+3,'WIRE MAP  TRD 3$',NWIRE_PER_LAYER(3),
     &    1.,FLOAT(NWIRE_PER_LAYER(3)+1),VMX)
        CALL HBOOK1(FIRSHT+530+4,'WIRE MAP  CATH. TRD 1$',256,1.,257.,
     &    VMX)
        CALL HBOOK1(FIRSHT+530+5,'WIRE MAP  CATH. TRD 2$',256,1.,257.,
     &    VMX)
        CALL HBOOK1(FIRSHT+530+6,'WIRE MAP  CATH. TRD 3$',256,1.,257.,
     &    VMX)
        CALL HBOOK1(FIRSHT+530+7,'MISSING WIRE MAP  TRD 3$',
     &    NWIRE_PER_LAYER(3),1.,FLOAT(NWIRE_PER_LAYER(3)+1),VMX)
        CALL HBOOK1(FIRSHT+500+21,' N MISS $',7,0.,7.,VMX)
        CALL HBOOK1(FIRSHT+500+22,' N HIT $',7,0.,7.,VMX)
        CALL HBOOK1(FIRSHT+500+23,' N PRESENT $',7,0.,7.,VMX)
        CALL HBOOK1(FIRSHT+590+1,'WIRE MAP OFF TRACK TRD 1$',
     &    256,1.,256.,VMX)
        CALL HBOOK1(FIRSHT+590+2,'WIRE MAP OFF TRACK TRD 2$',
     &    256,1.,256.,VMX)
        CALL HBOOK1(FIRSHT+590+3,'WIRE MAP OFF TRACK TRD 3$',
     &    NWIRE_PER_LAYER(3),1.,FLOAT(NWIRE_PER_LAYER(3)),VMX)
      END IF
      IF (SWHISG(4).EQ.'Y') THEN
        CALL HBOOK1(FIRSHT+530+8,'HIT WIRES WHEIG TRD 1$',256,1.,257.,
     &    VMX)
        CALL HBOOK1(FIRSHT+530+9,'HIT WIRES WHEIG TRD 2$',256,1.,257.,
     &    VMX)
        CALL HBOOK1(FIRSHT+540  ,'HIT WIRES WHEIG TRD 3$',
     &    NWIRE_PER_LAYER(3),1.,FLOAT(NWIRE_PER_LAYER(3)), VMX)
        CALL HBOOK1(FIRSHT+550+8,'HIT WIRES       TRD 1$',256,1.,257.,
     &    VMX)
        CALL HBOOK1(FIRSHT+550+9,'HIT WIRES       TRD 2$',256,1.,257.,
     &    VMX)
        CALL HBOOK1(FIRSHT+560  ,'HIT WIRES       TRD 3$',
     &    NWIRE_PER_LAYER(3),1.,FLOAT(NWIRE_PER_LAYER(3)), VMX)
      END IF
      IF (SWHISG(5).EQ.'Y') THEN
        CALL HBOOK1(FIRSHT+500+4,'CLUSTER ENERGIES$', 50,0.,500.,VMX)
        CALL HBOOK1(FIRSHT+500+5,'ARRIVAL TIMES$', 64,0.,64,0.,128.,VMX)
        CALL HBOOK2(FIRSHT+500+6,'CLUSTER ENERGIES VS. ARRIVAL TIME$',
     &    50,0.,500.,64,0.,128.,VMX)
        CALL HBOOK1(FIRSHT+660+1,'CLUSTER ENERGY OFF-TRACK TRD1$',
     &    50,0.,1000.,VMX)
        CALL HBOOK1(FIRSHT+660+2,'CLUSTER ENERGY OFF-TRACK TRD2$',
     &    50,0.,1000.,VMX)
        CALL HBOOK1(FIRSHT+660+3,'CLUSTER ENERGY OFF-TRACK TRD3$',
     &    50,0.,1000.,VMX)
        CALL HBOOK1(FIRSHT+670+1,'CLUSTER POSITION TRD1$',
     &    200,0.,200.,VMX)
        CALL HBOOK1(FIRSHT+670+2,'CLUSTER POSITION TRD2$',
     &    200,0.,200.,VMX)
        CALL HBOOK1(FIRSHT+670+3,'CLUSTER POSITION TRD3$',
     &    200,0.,200.,VMX)
      END IF
C      CALL HBOOK2(4094,'THETA PHI DROPPED CLUSTER$',30,0.,180.,
C     &    64,0.,360.,VMX)
C      CALL HBPRO(4094,0.)
      IF (SWHISG(6).EQ.'Y') THEN! Pedestals
        CALL HBOOK1(FIRSHT+561,' PEDS TRD LAYER 1$',100,4.,12.,VMX)
        CALL HBOOK1(FIRSHT+571,' SIGMA PED. TRD LAYER 1$',100,0.,3.,VMX)
        CALL HBOOK1(FIRSHT+562,' PEDS TRD LAYER2$',100,4.,12.,VMX)
        CALL HBOOK1(FIRSHT+572,' SIGMA PED. TRD LAYER2$',100,0.,3.,VMX)
        CALL HBOOK1(FIRSHT+563,' PEDS TRD LAYER 3$',100,4.,12.,VMX)
        CALL HBOOK1(FIRSHT+573,' SIGMA PED. TRD LAYER 3$',100,0.,3.,VMX)
        CALL HBOOK1(FIRSHT+564,' PEDS TRD LAYER 4$',100,4.,12.,VMX)
        CALL HBOOK1(FIRSHT+574,' SIGMA PED. TRD LAYER 4$',100,0.,3.,VMX)
        CALL HBOOK1(FIRSHT+565,' PEDS TRD LAYER 5$',100,4.,12.,VMX)
        CALL HBOOK1(FIRSHT+575,' SIGMA PED. TRD LAYER 5$',100,0.,3.,VMX)
        CALL HBOOK1(FIRSHT+566,' PEDS TRD LAYER 6$',100,4.,12.,VMX)
        CALL HBOOK1(FIRSHT+576,' SIGMA PED. TRD LAYER 6$',100,0.,3.,VMX)
      END IF
      IF (SWHISG(7).EQ.'Y') THEN! Difference between wires in different layers
        CALL HBOOK1(FIRSHT+600+1,'DIFF WIRE 2-1  $',32,-16.,16.,VMX)
        CALL HBOOK1(FIRSHT+600+2,'DIFF WIRE 3-1  $',32,-16.,16.,VMX)
        CALL HBOOK1(FIRSHT+600+3,'DIFF WIRE 3-2  $',32,-16.,16.,VMX)
        CALL HBOOK1(FIRSHT+610+1,'WIRE TRACK-WIRE CODED ANODE TRD1$',
     &    32,-16.,16.,VMX)
        CALL HBOOK1(FIRSHT+610+2,'WIRE TRACK-WIRE CODED ANODE TRD2$',
     &    32,-16.,16.,VMX)
        CALL HBOOK1(FIRSHT+610+3,'WIRE TRACK-WIRE CODED ANODE TRD3$',
     &    32,-16.,16.,VMX)
C        CALL HBOOK1(FIRSHT+610+4,'wire track-wire coded cathode trd1$',
C     &    32,-16.,16.,VMX)
C        CALL HBOOK1(FIRSHT+610+5,'wire track-wire coded cathode trd2$',
C     &    32,-16.,16.,VMX)
C        CALL HBOOK1(FIRSHT+610+6,'wire track-wire coded cathode trd3$',
C     &    32,-16.,16.,VMX)
      END IF
      IF (SWHISG(8).EQ.'Y') THEN! Timings
        CALL HBOOK2(FIRSHT+580,' TIME DTRGM VERSUS TMIN TRD$',
     &    50,-20,80.,50,0.,100.,VMX)
        CALL HBOOK2(FIRSHT+581,' ENERGY VERSUS TIME DTRGM TRD1$',
     &    50,0.,600.,50,0.,100.,VMX)
        CALL HBOOK2(FIRSHT+582,' ENERGY VERSUS TIME DTRGM TRD2$',
     &    50,0.,600.,50,0.,100.,VMX)
        CALL HBOOK2(FIRSHT+583,' ENERGY VERSUS TIME DTRGM TRD3$',
     &    50,0.,600.,50,0.,100.,VMX)
C      CALL HBOOK1(FIRSHT+540+1,' tmin TRD 1$',100,0.,100.,VMX)
C      CALL HBOOK1(FIRSHT+540+2,' tmin TRD 2$',100,0.,100.,VMX)
C      CALL HBOOK1(FIRSHT+540+3,' tmin TRD 3$',100,0.,100.,VMX)
      END IF
C      CALL HBOOK2(FIRSHT+584,' energy TRD1 versus energy TRD2$',
C     &  50,0.,600.,50,0.,600.,VMX)
C      CALL HBOOK2(FIRSHT+585,' energy TRD2 versus energy TRD3$',
C     &  50,0.,600.,50,0.,600.,VMX)
C        CALL HBOOK1(FIRSHT+620+1,'min -max trd1 $',50,0.,50.,VMX)
C        CALL HBOOK1(FIRSHT+620+2,'min -max trd2 $',50,0.,50.,VMX)
C        CALL HBOOK1(FIRSHT+620+3,'min -max trd3 $',50,0.,50.,VMX)
C        CALL HBOOK1(FIRSHT+620+4,'min -max trd1 $',50,0.,50.,VMX)
C        CALL HBOOK1(FIRSHT+620+5,'min -max trd2 $',50,0.,50.,VMX)
C        CALL HBOOK1(FIRSHT+620+6,'min -max trd3 $',50,0.,50.,VMX)
      IF (SWHISG(9).EQ.'Y') THEN! Cathodes
        CALL HBOOK2(FIRSHT+630+1,' E ANODE VERSUS E CATH. TRD1$',
     &    50,0.,1000.,50,0.,300.,VMX)
        CALL HBOOK2(FIRSHT+630+2,' E ANODE VERSUS E CATH. TR2$',
     &    50,0.,1000.,50,0.,300.,VMX)
        CALL HBOOK2(FIRSHT+630+3,' E ANODE VERSUS E CATH. TRD3$',
     &    50,0.,1000.,50,0.,300.,VMX)
      END IF
      IF (SWHISG(10).EQ.'Y') THEN! dQ/dT
        CALL HBOOK1(FIRSHT+640+1,'DQ/DT TRD1 $',100,0.,100.,VMX)
        CALL HBOOK1(FIRSHT+640+2,'DQ/DT TRD2 $',100,0.,100.,VMX)
        CALL HBOOK1(FIRSHT+640+3,'DQ/DT TRD3 $',100,0.,100.,VMX)
C      call hbook2(firsht+643+1,' dq/dt versus imin. TRD1$',
C     &  50,0.,100.,50,0.,50.,VMX)
C       call hbook2(firsht+643+2,' dq/dt versus imin. TRD2$',
C     &  50,0.,100.,50,0.,50.,VMX)
C        call hbook2(firsht+650+1,' energie versus position dans la
C     +cellule TRD1$', 50,0.,1000.,52,-1.2,1.2,VMX)
C        call hbook2(firsht+650+2,' energie versus position dans la
C     +cellule TRD2$', 50,0.,1000.,52,-1.2,1.2,VMX)
C        call hbook2(firsht+650+3,' energie versus position dans la
C     +cellule TRD3$', 50,0.,1000.,52,-1.2,1.2,VMX)
      END IF
  700 CONTINUE
      CALL EZRSET
C Reduced set of histograms for reconstruction
c      PRINT*,' before booking REDUCED_SET ',REDUCED_SET
      IF(.NOT.REDUCED_SET)GO TO 999
      FIRST_VER=FIRSHT+1000
      DO ICH=1,6
        TRHNAM=' NORMALIZED ENERGY ALL TRACKS '//NUMLAY(ICH)
c        PRINT*,' ich',ICH,' booking de ',FIRST_VER+6+ICH,
c     &    TRHNAM
        CALL HBOOK1(FIRST_VER+6+ICH,
     &    ' NORMALIZED ENERGY ALL TRACKS '//NUMLAY(ICH),100,0.,5.,VMX)
        TRHNAM=' NORMALIZED ENERGY ELECTRONS '//NUMLAY(ICH)
        CALL HBOOK1(FIRST_VER+106+ICH,
     +    ' NORMALIZED ENERGY ELECTRONS '//NUMLAY(ICH),100,0.,5.,VMX)
      END DO
      CALL HBOOK1(FIRST_VER+39,
     &   'LIKELIHOOD EFFICIENCY ANODES ALL TRACKS',100,0.,1.,VMX)
      CALL HBOOK1(FIRST_VER+139,
     &  'LIKELIHOOD EFFICIENCY ANODES ELECTRONS ', 100,0.,1.,VMX)
      CALL HBOOK1(FIRST_VER+50,
     &  '  NUMBER OF HIT LAYERS  ALL TRACKS',14,0.,14.,VMX)
      CALL HBOOK1(FIRST_VER+150,' NUMBER OF HIT LAYERS ELECTRONS',
     &  14,0.,14.,VMX)
      CALL HBOOK1(FIRST_VER+54,' HIT    LAYER NUMBER ALL TRACKS',
     &        6,1.,6.,VMX)
      CALL HBOOK1(FIRST_VER+154,' HIT    LAYER NUMBER ELECTRONS',
     &        6,1.,6.,VMX)
      CALL HBARX(FIRST_VER+54)
      CALL HBARX(FIRST_VER+154)
      CALL HBOOK1(FIRST_VER+55,' MISSING LAYER NUMBER ALL TRACKS',
     &        6,1.,6.,VMX)
      CALL HBOOK1(FIRST_VER+155,' MISSING LAYER NUMBER ELECTRONS',
     &        6,1.,6.,VMX)
  999 RETURN
 1000 FORMAT(2X,A1)
      END

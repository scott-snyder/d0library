      SUBROUTINE TRDBOK_ON
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOK TRH HISTOGRAMS
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  11-JUN-1990   J.Fr.Glicenstein
C-   Modified 23-JUL-1990   Y.Ducros
C-   Updated  12-JAN-1994   A. Zylberstejn
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER I,ICH,K,IK,IER,IJ,LAYER,I2,JSEC,ISECTR,TRIGGER,ILAYR
      INTEGER IHISTO(6)
      REAL BININF,BINSUP
      INTEGER NB,ICAS,CAS(4)
      REAL VMX
      INTEGER NBTIT,NBLAY,NBUNI,ITRIG,MUM,IFO
      PARAMETER( NBTIT = 7 )
      PARAMETER( NBLAY = 3 )
      PARAMETER( NBUNI = 4 )
      INCLUDE 'D0$INC:HTFIRS.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
C
      CHARACTER*80 CHT
      CHARACTER*8 LAYR(NBLAY),CAR8
      CHARACTER*11 UNIT(NBUNI),WORD1(4)
      CHARACTER*10 PHYS(4),NORM
      CHARACTER*11 TITR(NBTIT)
      CHARACTER*60 TRHNAM
      CHARACTER*4 CAR4(2)
      CHARACTER*11 CAR11
      CHARACTER*2 CAR2(16)
      CHARACTER*3 C3
C
      INTEGER KK,LUM,NUM,NUMERO,NX,NWT,IS,HISPREM,IFOIS,LUMP
      INTEGER NBIN(4),HTSECD,NUMTRIG,NCAN(4),NBBIN(4),NBINE,NUMSECT
      REAL BINMIN(4),BINMAX(4),MAXBE,NCAM(4),XMI,XMA,MINBE
      INTEGER LENGTH,IDX
      PARAMETER (IDX = 1)
      CHARACTER*(*) HBKDIR
      PARAMETER (HBKDIR = 'HBOOK_DIRECTORY')
      CHARACTER*3 HBDIR(1)
      LOGICAL TEST
      CHARACTER*3 TIMING
      INTEGER ITIMING,ID
      EQUIVALENCE (NX,IHISTO(1)),(XMI,IHISTO(2))  ,(XMA,IHISTO(3)),
     +         (NBINE,IHISTO(4)),(MINBE,IHISTO(5)),(MAXBE,IHISTO(6))
      CHARACTER*3 SWPHYS(10),SWLAYR(3),SWUNIT(4),SWHIST(10),SWSECT(16),
     #            SWANOD(2),SWPEDES
      INTEGER     ISWPHY(10),ISWLAY(3),ISWUNI(4),ISWHIS(10),ISWSEC(16),
     #            ISWANO(2),IAUX(4)
      LOGICAL FIRST
      DATA LAYR / ' Layer 1',' Layer 2',' Layer 3'/
      DATA UNIT / ' layer     ',
     &            ' Sector    ',
     &            ' Wire      ',
     &            ' trd       '/
      DATA WORD1/ 'Hit layer  ',
     +            'hit sector ',
     +            'hit wire   ',
     +            '           '/
      DATA PHYS / 'All trig. ',' Electron ','  Pion    ','All Tracks'/
      DATA TITR / '   Map of  ','   Energy  ',' Nb. of Ch/',
     &  '**dQ/dt    ',' Tot Energy','  Sigma    ','Fourier a2 '/
      DATA NORM / 'Normalised'/
      DATA CAR4/'Anod','Cath'/
      DATA CAR2/'00','01','02','03','04','05','06','07','08','09',
     &          '10','11','12','13','14','15'/
      DATA NCAN/257,17,2,100/,
     &                            NCAM/257.,17.,2.,100./
      DATA BINMIN/4*0./
      DATA BINMAX/4.,17.,257.,0./
      DATA NBIN/4,17,257,0/
      DATA VMX /0./
      DATA CAS/3,2,1,4/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C            +-------------------+
C            |BOOK THE HISTOGRAMS|
C            +-------------------+
C       ITRIG = 0-9 , ILAYR = 0-3, ISECTR = 0-4
C ITRIG   = 0  no data selection
C ITRIG   = 1  tracks
C ITRIG   = 2  electrons
C ITRIG   = 3  pions
C ITRIG   = 10 Histos for 1 event for same triggers as above
C ILAYR   = 0  chamber 1
C ILAYR   = 1  chamber 2
C ILAYR   = 2  chamber 3
C
C ISECTR  = 0  layer  (Anodes)
C ISECTR  = 2  sector
C ISECTR  = 4  wire
C ISECTR  = 6  all trd
C ISECTR  = 1  layer  (Cathodes)
C ISECTR  = 3  sector
C ISECTR  = 5  wire
C ISECTR  = 7  all trd
C----------------------------------------------------------------------
C  Histo. indexed by HTFIRS+ 100*ILAYR + 1000*ITRIG+ 10*ISECTR    :anode
C                    HTFIRS+ 100*ILAYR + 1000*ITRIG+ 10*(ISECTR+1):cath
C----------------------------------------------------------------------
C****  Reads in TRDHIT_RCP file
      IF (FIRST) THEN
        CALL EZPICK('TRDHIT_RCP')
        CALL EZGET('HBOOK_DIRECTORY',IAUX(1),IER)
        CALL UHTOC(IAUX(1),3,HBDIR(1),3)
        CALL EZGET('NUM_PHYS_SWIT',NUMTRIG,IER)
        CALL EZGET('PHYSICS_SWITCHES',ISWPHY(1),IER)
        CALL EZGET('LAYER_SWITCHES',ISWLAY(1),IER)
        CALL EZGET('UNIT_SWITCHES',ISWUNI(1),IER)
        CALL EZGET('ANODE_SWITCHES',ISWANO(1),IER)
        CALL EZGET('HISTOON_SWITCHES',ISWHIS(1),IER)
        CALL EZGET('NUM_SEC_SWIT',NUMSECT,IER)
        CALL EZGET('SEC_NUM_SWIT',ISWSEC(1),IER)
        CALL EZGET('HISTO_LIMITS',IHISTO(1),IER)
        CALL EZGET('FOURIER_PHASE',ITIMING,IER)
        CALL EZRSET
C  Transform integer in characters
        DO I=1,10
          CALL UHTOC(ISWPHY(I),3,SWPHYS(I),3)
        END DO
        DO I=1,3
          CALL UHTOC(ISWLAY(I),3,SWLAYR(I),3)
        END DO
        DO I=1,4
          CALL UHTOC(ISWUNI(I),3,SWUNIT(I),3)
        END DO
        DO I=1,2
          CALL UHTOC(ISWANO(I),3,SWANOD(I),3)
        END DO
        DO I=1,5
          CALL UHTOC(ISWHIS(I),3,SWHIST(I),3)
        END DO
        DO I=1,16
          CALL UHTOC(ISWSEC(I),3,SWSECT(I),3)
        END DO
        CALL UHTOC(ITIMING,3,TIMING,3)
        CALL HCDIR('//PAWC',' ')  !GO TO TOP DIRECTORY
        CALL HMDIR(HBDIR(1),'S')       !CREATE TRH DIRECTORY
        FIRST = .FALSE.
      ELSE
        CALL HCDIR('//PAWC/'//HBDIR(1),' ')
      ENDIF
      IFOIS=0
      HISPREM=HTFIRS
    1 IFOIS=IFOIS+1
      IF(IFOIS.EQ.2)  HISPREM=HTFIRS+1000*numtrig
      DO 130 TRIGGER = 1,NUMTRIG ! loop on triggers
C        PRINT*,' trigger',TRIGGER,' SWPHYS(TRIGGER) ',SWPHYS(TRIGGER)
        IF(SWPHYS(TRIGGER).NE.'Y')GO TO 130
        ITRIG = TRIGGER - 1
        IFO=0
        DO 120 ICAS=1,4
          I2=CAS(ICAS) ! i2=1:layer,=2:sector,=3:wire,=4:TRD
          IFO=IFO+1
C            PRINT*,' trigger,i2,layer',TRIGGER,I2,LAYER,' SWUNIT(I2)',
C     &        SWUNIT(I2)
          IF(SWUNIT(I2).NE.'Y')GO TO 120
          DO 26 LAYER = 1,3
C          PRINT*,' layer',LAYER,'SWLAYR(LAYER) ',SWLAYR(LAYER)
            IF(SWLAYR(LAYER).NE.'Y')GO TO 26
            ILAYR = LAYER - 1
            DO 18 KK=1,2  ! kk=1 anodes,kk=2 cathodes
C              PRINT*,'  kk',KK, ' SWANOD(KK) ',SWANOD(KK)
              IF(SWANOD(KK).NE.'Y')GO TO 18
              ISECTR = KK - 1 +(I2-1)*2
              NUM = HISPREM + 1000*ITRIG + 100*ILAYR +10*ISECTR
C              PRINT*,' isectr,i2,ifo',ISECTR,I2,IFO,' num',NUM,
C     &          ' SWHIST(1) ',SWHIST(1),'SWHIST(2)',SWHIST(2)
              IF(I2.EQ.1 .AND .IFO.GT.1) GO TO 14
              IF(I2.EQ.4) GO TO 15
              CAR8=LAYR(LAYER)
              IF(I2.EQ.1.AND.IFO.EQ.1)  CAR8='        '
C
              IF(SWHIST(1).EQ.'Y' .AND. IFOIS.EQ.1)THEN !! Hit channel Map
                  LUM=NUM
                IF(IFOIS.EQ.1) THEN
                  TRHNAM= TITR(1)//WORD1(I2)//CAR4(KK)
     &              //CAR8//PHYS(TRIGGER)
                ELSE ! if normalized
                  TRHNAM= NORM//TITR(1)//WORD1(I2)//CAR4(KK)
     &                     //CAR8//PHYS(TRIGGER)
                END IF
                NB=NBIN(I2)
                BININF=BINMIN(I2)
                BINSUP=BINMAX(I2)
                IF(LAYER.EQ.3 .AND. KK.EQ.1)THEN ! anode layer 3
                  IF(I2.EQ.3)THEN
                    NB=NWIRE_PER_LAYER(LAYER)
                  ELSE
                    NB=NWIRE_PER_LAYER(LAYER)/16
                  END IF
                  BININF=1.
                  BINSUP=NB+1
                END IF
c                PRINT2044, LUM+1,TRHNAM,NB,BININF,BINSUP
 2044           FORMAT( I6,A60,I4,2F6.1)
                CALL HBOOK1(LUM+1, TRHNAM,NB,BININF,BINSUP,VMX)
              END IF
C
              IF(SWHIST(2).EQ.'Y')THEN !Channel energy map
                  LUM=NUM
                IF(IFOIS.EQ.1) THEN
                  TRHNAM= TITR(2)//WORD1(I2)//CAR4(KK)
     &              //CAR8//PHYS(TRIGGER)
                ELSE
                  TRHNAM= NORM//TITR(2)//WORD1(I2)//CAR4(KK)
     &                     //CAR8//PHYS(TRIGGER)
                END IF ! if normalized
                NB=NBIN(I2)
                BININF=BINMIN(I2)
                BINSUP=BINMAX(I2)
                IF(LAYER.EQ.3 .AND. KK.EQ.1)THEN ! anode layer 3
                  IF(I2.EQ.3)THEN
                    NB=NWIRE_PER_LAYER(LAYER)
                  ELSE
                    NB=NWIRE_PER_LAYER(LAYER)/16
                  END IF
                  BININF=1.
                  BINSUP=NB+1
                END IF
c                PRINT2044, LUM+2,TRHNAM,NB,BININF,BINSUP
                CALL HBOOK1(LUM+2, TRHNAM,NB,BININF,BINSUP,VMX)
              END IF
C  section suppressed (12-jan-1993)
C              IF(SWHIST(5).EQ.'Y')THEN ! Total energy
C                IF(IFOIS.EQ.1) THEN
C                  TRHNAM= TITR(5)//UNIT(I2)
C     &                           //CAR4(KK)//CAR8//PHYS(TRIGGER)
C                ELSE
C                  TRHNAM= NORM//TITR(5)//UNIT(I2)
C     &                           //CAR4(KK)//CAR8//PHYS(TRIGGER)
C                END IF
C                PRINT2044, LUM+5,TRHNAM,NBINE,MINBE,MAXBE
C                CALL HBOOK1(LUM+5, TRHNAM,NBINE,MINBE,
C     &                      MAXBE,VMX) !Total energy
C              END IF
C                IF(I2.EQ.3) THEN
C                  IF(IFOIS.EQ.1) THEN
C                    TRHNAM=TITR(6)//WORD1(I2)//CAR4(KK)//CAR8//
C     &                       PHYS(TRIGGER)
C                  ELSE
C                    TRHNAM= NORM//TITR(6)//WORD1(I2)//CAR4(KK)
C     &                     //CAR8//PHYS(TRIGGER)
C                  END IF
C                PRINT2044, nUM+6,NBIN(I2),BINMIN(I2),BINMAX(I2)
C                  CALL HBOOK1(NUM+6,TRHNAM,NBIN(I2),BINMIN(I2),
C     &                          BINMAX(I2),VMX)  !Channel energy variance
C                END IF
C              END IF
   14         CONTINUE
              IF(i2.EQ.3)go to 18
              CAR8=LAYR(LAYER)
              IF(I2.EQ.2 .AND. KK.EQ.1) THEN
c                print*,'          sector level'
                DO 19 JSEC=1,NWIRE_PER_LAYER(LAYER)/16
C                  IF(SWSECT(JSEC).NE.'Y') GO TO 19
                  MUM=HISPREM+1000*ITRIG+300*KK+100*ILAYR+JSEC-1
C                  PRINT*,' mum',MUM,' SWHIST(3)', SWHIST(3)
C                             !nb. of hit channels
                  IF(SWHIST(3).EQ.'Y' .AND. IFOIS.EQ.1)THEN
                    IF(IFOIS.EQ.1) THEN
                      TRHNAM= TITR(3)//UNIT(I2)//CAR2(JSEC)
     &                           //CAR4(KK)//CAR8//PHYS(TRIGGER)
                    ELSE
                      TRHNAM= NORM//TITR(3)//UNIT(I2)//CAR2(JSEC)
     &                           //CAR4(KK)//CAR8//PHYS(TRIGGER)
                    END IF
                    NB=17
                    BINSUP=17.
c                    PRINT2044, MUM,TRHNAM,NB,BINMIN(I2),BINSUP
                    CALL HBOOK1(MUM, TRHNAM,NB,BINMIN(I2),BINSUP,VMX)
                  END IF
                  IF(SWHIST(4).EQ.'Y')THEN ! dQ/dT
                    IF(IFOIS.EQ.1) THEN
                      TRHNAM= TITR(4)//UNIT(I2)//CAR2(JSEC)
     &                           //CAR4(KK)//CAR8//PHYS(TRIGGER)
                    ELSE
                      TRHNAM= NORM//TITR(4)//UNIT(I2)//CAR2(JSEC)
     &                           //CAR4(KK)//CAR8//PHYS(TRIGGER)
                    END IF
                    BINSUP=100.
c                    PRINT2044, MUM+20,TRHNAM,NX,BINMIN(I2),BINsup
                    CALL HBOOK1(MUM+20, TRHNAM,NX,BINMIN(I2),
     &                       BINSUP,VMX)
                    IF (SWHIST(6).EQ.'Y' .AND. IFOIS.EQ.1) THEN! Fourier
                                                               ! transform
                      IF(IFOIS.EQ.1) THEN
                        TRHNAM= TITR(7)//UNIT(I2)//CAR2(JSEC)
     &                           //CAR4(KK)//CAR8//PHYS(TRIGGER)
                      ELSE
                        TRHNAM= NORM//TITR(7)//UNIT(I2)//CAR2(JSEC)
     &                           //CAR4(KK)//CAR8//PHYS(TRIGGER)
                      END IF
                      BINSUP=100.
c                      PRINT2044, MUM+60,TRHNAM,NX,BINMIN(I2),BINSUP
                      CALL HBOOK1(MUM+60, TRHNAM,NX,BINMIN(I2),
     &                       BINSUP,VMX)
                    ENDIF
                  END IF
C  Section suppressed (12-Jan-1993)
C                  IF(SWHIST(5).EQ.'Y')THEN ! Total energy
C                    IF(IFOIS.EQ.1) THEN
C                      TRHNAM= TITR(5)//UNIT(I2)//CAR2(JSEC)
C     &                           //CAR4(KK)//CAR8//PHYS(TRIGGER)
C                    ELSE
C                      TRHNAM= NORM//TITR(5)//UNIT(I2)//CAR2(JSEC)
C     &                           //CAR4(KK)//CAR8//PHYS(TRIGGER)
C                    END IF
C                    PRINT'(a6,i5,a8,a60)',' histo',MUM+40,' trhnam',
C     &                TRHNAM
C                    CALL HBOOK1(MUM+40, TRHNAM,NBINE,MINBE,
C     &                      MAXBE,VMX) !Total energy
C                  END IF
   19           CONTINUE
              END IF
   15         CONTINUE
C              IF (I2.NE.4) GOTO 26
              IF(I2.EQ.4.AND.LAYER.GT.1) GO TO 18
              CAR8 = LAYR(LAYER)
              IF (I2.EQ.4) CAR8 = '       '
              IF(SWHIST(3).EQ.'Y' .AND. ifois.EQ.1)THEN !nb. of hit channels
                IF(IFOIS.EQ.1) THEN
                  TRHNAM= TITR(3)//UNIT(I2)//CAR4(KK)
     &                //CAR8//PHYS(TRIGGER)
                ELSE
                  TRHNAM= NORM//TITR(3)//UNIT(I2)//CAR4(KK)//CAR8
     &                    //PHYS(TRIGGER)
                END IF
c                PRINT2044, NUM+3,TRHNAM,NCAN(i2),BINMIN(I2),NCAM(I2)
                CALL HBOOK1(NUM+3, TRHNAM,NCAN(I2),
     &                        BINMIN(I2),NCAM(I2),VMX)
              END IF
              IF(I2.GE.2) GO TO 21
              IF(SWHIST(4).EQ.'Y')THEN ! dQ/dT
                IF(IFOIS.EQ.1) THEN
                  TRHNAM= TITR(4)//UNIT(I2)//CAR4(KK)
     &                //CAR8//PHYS(TRIGGER)
                ELSE
                  TRHNAM= NORM//TITR(4)//UNIT(I2)//CAR4(KK)//CAR8
     &                    //PHYS(TRIGGER)
                END IF
                NB=128
                BINSUP=129.
c                PRINT2044, NUM+4,TRHNAM,NB,BINMIN(I2),BINSUP
                CALL HBOOK1(NUM+4, TRHNAM,NB,BINMIN(I2),
     &                       BINSUP,VMX)
                IF(SWHIST(6).EQ.'Y' .AND. ifois.EQ.1)THEN!dQ/dT Fourier transf.
                  IF(IFOIS.EQ.1) THEN
                    TRHNAM= TITR(7)//UNIT(I2)//CAR4(KK)
     &                  //CAR8//PHYS(TRIGGER)
                  ELSE
                    TRHNAM= NORM//TITR(7)//UNIT(I2)//CAR4(KK)//CAR8
     &                    //PHYS(TRIGGER)
                  END IF
                  BINSUP=100.
c                  PRINT2044, NUM+7,TRHNAM,NX,BINMIN(I2),BINSUP
                  CALL HBOOK1(NUM+7, TRHNAM,NX,BINMIN(I2),
     &                       100.,VMX)
                END IF
              END IF
   21         CONTINUE
              IF(SWHIST(5).EQ.'Y' .AND. ifois.EQ.1)THEN ! Total energy
                IF(IFOIS.EQ.1) THEN
                  TRHNAM= TITR(5)//UNIT(I2)//CAR4(KK)
     &                //CAR8//PHYS(TRIGGER)
                ELSE
                  TRHNAM= NORM//TITR(5)//UNIT(I2)//CAR4(KK)//CAR8
     &                    //PHYS(TRIGGER)
                END IF
c                PRINT2044, NUM+5,TRHNAM,NBINE,MINBE,MAXBE
                CALL HBOOK1(NUM+5, TRHNAM,NBINE,MINBE,
     &                      MAXBE,VMX) !Total energy
              END IF
   18       CONTINUE ! End of loop on anode/cathode
   26     CONTINUE ! end of loop on layers
  120   CONTINUE ! End of loop on unit of detector
  130 CONTINUE   !End of loop on triggers
      IF(IFOIS.EQ.1) GO TO 1
  999 CONTINUE
      IF (TIMING.EQ.'Y') THEN
        ID = HTFIRS + 1000 * (NUMTRIG+1)
        TRHNAM=' Phase plot'
c        PRINT2044, ID,TRHNAM,NX,XMI,XMA
        CALL HBOOK2(ID,'Phase plot',NX,XMI,XMA,20,-3.14,3.14,0.)
      ENDIF
      CALL HBOOK1(1,'DUMMY',NX,XMI,XMA,0.)
C      CALL HBOOK1(100,'Fourier transform',NX,XMI,XMA,0.)
C      IF(IFOIS.GE.0)CALL EXIT
      RETURN
 1000 FORMAT(2X,A1)
      END

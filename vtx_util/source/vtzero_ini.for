      FUNCTION VTZERO_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : initialize variables, book histograms for
C-                         TZERO offline calculation
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  29-MAY-1992       V. D. Elvira
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL VTZERO_INI
      INTEGER LAYER,SECTOR,WIRE,IEND,ID,CR
      INTEGER NLRS,NSCT(3),NWRE,LRS(3),SCTS0(16),SCTS1(32),WRE(8)
      INTEGER IL,IS,IW,IERR,IER
      INTEGER NX,GROUP,NUM,LRCP
C----------------------------------------------------------------------
      REAL IMI,IMA,TRGOFF
C----------------------------------------------------------------------
      LOGICAL ROWST
C----------------------------------------------------------------------
      CHARACTER*40 TITL,CRTIT0,CRTITS,GRTIT0,GRTITS,DSTIT,SSTIT
      CHARACTER*3 CHLAY,CHSEC,CHWIR,CHCRA,CHGRU
      CHARACTER*20 RCPFIL
C----------------------------------------------------------------------
      DATA LRS/0,1,2/
      DATA SCTS0/0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15/
      DATA SCTS1/0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
     &  21,22,23,24,25,26,27,28,29,30,31/
      DATA WRE/0,1,2,3,4,5,6,7/
C----------------------------------------------------------------------
C- Read RCP parameters and switches
C----------------------------------------------------------------------
C
C ****  Read in VTXCOFF_RCP if necessary, then get needed control parameters
C
      RCPFIL = 'VTXCOFF_RCP'
      CALL EZLOC(RCPFIL,LRCP)
      IF ( LRCP .LE. 0 ) THEN
        CALL INRCP (RCPFIL,IER)  ! Read parameter file into an SRCP bank
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG('INRCP ERROR','VTZERO_INI',
     &    'Attempt to read VTXCOFF_RCP failed','F')
        ENDIF
      ENDIF
      CALL EZPICK('VTXCOFF_RCP')
      CALL EZGET_i('NX',NX,IER)
      CALL EZGET('IMI',IMI,IER)
      CALL EZGET('IMA',IMA,IER)
      CALL EZGET_l('ROWST',ROWST,IER)
      CALL EZRSET
C
C ****  Read in VTRAKS_RCP if necessary
C
      CALL VTRINI
      CALL EZPICK('VTRAKS_RCP')
      CALL EZGET('TRGOFF',TRGOFF,IER)
      CALL EZRSET
C----------------------------------------------------------------------
      NLRS=3
      NSCT(1)=16
      NSCT(2)=32
      NSCT(3)=32
      NWRE=8
C----------------------------------------------------------------------
C- Gets electronic address from logical number for each number and books
C- individual channel drif time distribution histograms
C----------------------------------------------------------------------
      DO 1 IL=1,NLRS
        LAYER=LRS(IL)
        DO 2 IS=1,NSCT(IL)
          IF (LAYER.EQ.0) THEN
            SECTOR=SCTS0(IS)
          ELSE
            SECTOR=SCTS1(IS)
          ENDIF
          DO 3 IW=1,NWRE
            WIRE=WRE(IW)
            DO 4 IEND=0,1
              ID=10000+512*LAYER+16*SECTOR+2*WIRE+IEND
              WRITE(CHLAY,100) LAYER
              WRITE(CHSEC,100) SECTOR
              WRITE(CHWIR,100) WIRE
              TITL(1:40) ='DFT. TIME DIST.'//CHLAY//CHSEC//CHWIR
              CALL HBOOK1(ID,TITL,NX,IMI-TRGOFF,IMA-TRGOFF,0.)
              CALL HBARX(ID)
    4       CONTINUE
    3     CONTINUE
    2   CONTINUE
    1 CONTINUE
C----------------------------------------------------------------------
C- Book TZERO and SIGMA group histograms if switch ROWST is TRUE.
C- (Ed Otman's histo structure).
C----------------------------------------------------------------------
      IF (ROWST) THEN
        DO GROUP = 1,10
          WRITE(CHGRU,100) GROUP
          GRTIT0='TZERO VS. CHANNEL for group'//CHGRU
          GRTITS='SIGMA T0 VS. CHANNEL for group'//CHGRU
          CALL HBOOK1(200+10*(GROUP-1),GRTIT0,128,0.5,128.5,0.)
          CALL HBOOK1(200+10*(GROUP-1)+1,GRTITS,128,0.5,128.5,0.)
        ENDDO
      ENDIF
C----------------------------------------------------------------------
C- Book TZERO and SIGMA crate histograms
C----------------------------------------------------------------------
      DO CR=1,10
        NUM=CR*10-7
        WRITE(CHCRA,100) NUM
        CRTIT0='TZERO VS. CHANNEL for crate'//CHCRA
        CRTITS='SIGMA T0 VS. CHANNEL for crate'//CHCRA
        DSTIT='TZERO DISTRIBUTION for crate'//CHCRA
        SSTIT='SIGMA DISTRIBUTION for crate'//CHCRA
        CALL HBOOK1(100+10*(CR-1),CRTIT0,129,127.,256.,0.)
        CALL HBOOK1(100+10*(CR-1)+1,CRTITS,129,127.,256.,0.)
        CALL HBOOK1(100+10*(CR-1)+2,DSTIT,NX,IMI-TRGOFF,IMA-TRGOFF,0.)
        CALL HBOOK1(100+10*(CR-1)+3,SSTIT,50,0.,10.,0.)
      ENDDO
C----------------------------------------------------------------------
      VTZERO_INI=.TRUE.
C----------------------------------------------------------------------
  100 FORMAT(I3)
C----------------------------------------------------------------------
  999 RETURN
      END

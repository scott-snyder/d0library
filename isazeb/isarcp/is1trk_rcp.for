      SUBROUTINE IS1TRK_RCP(PARTICLE_ID,PRIMARY_VERTEX,THREE_MOMENTUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      User supplied subroutine for one track ISAJET event generation
C-      DOES MULTIPLE CELLS. Parameters read from RCP
C-
C-   Outputs :
C-     PARTICLE_ID      = particle ID (use ISAJET id's)
C-     PRIMARY_VERTEX(3)  = x,y,z starting point
C-     THREE_MOMENTUM(3) = px,py,pz of particle
C-
C-
C-   Created   07-NOV-1989   Rajendran Raja
C-                           Uses RCP file to read in parameters of single
C-                           track events
C-
C-   Modified  26-OCT-90     Shahriar Abachi
C-                              Modified to take a histogram (from rcp file)
C-                              and generate a randum momentum spectrum 
C-                              according to that
C-
C-   Modified  15-NOV-90     Shahriar Abachi
C-                              Problems with eta=13,14,36,37 solved.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER PARTICLE_ID
      REAL    THREE_MOMENTUM(*)
      REAL PRIMARY_VERTEX(*)
      REAL VERTEX_Z_SIGMA
      INTEGER IER
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      INTEGER NCELL_MAX
      PARAMETER( NCELL_MAX = 100 )
      INTEGER ETA_PHI_CELL(2,NCELL_MAX)
      INTEGER NMOMC_MAX, NMOMC_MAX2, NMOMC, N_MOM_CELL, NM
      PARAMETER( NMOMC_MAX = 1000 ,NMOMC_MAX2 = 2000 )
      REAL MOMENTUM_SPEC(NMOMC_MAX), MOMSPEC(NMOMC_MAX2)
      REAL NORM, BWID
      INTEGER ICELL,IETA,IPHI,NCELLS
C
      REAL    XS(2,16,NCELL_MAX)
      REAL    YS(2,16,NCELL_MAX)
      REAL    ZS(2,16,NCELL_MAX)
      INTEGER NS(NCELL_MAX),IL
C
      REAL DELTAR(3,3,NCELL_MAX)
C
      REAL    X1(NCELL_MAX)
      REAL    Y1(NCELL_MAX)
      REAL    Z1(NCELL_MAX)
C
      REAL PARTICLE_MOM
      REAL    PHI,SNTH,CSTH,ETA,THETA
C
      INTEGER IFOUND,IPT,IS
      INTEGER ISTART(10),SIDE(10)
      INTEGER IST,IEND,IF1
      INTEGER IOFF,IOFF_ST,IOFF_DEL
      INTEGER J,K,I,JB,LOWEDG,TEMP1,TEMP2
C
      REAL    REND(3) ,RNDM
      REAL    DVEC,GAUSS,DUM
      REAL    VERTEX(3)
C
      REAL    ETALO,ETAHI,DELETA,PHILO,PHIHI,DELPHI
      REAL    XLO,XHI,YLO,YHI,ZLO,ZHI
      REAL    DELTAX,DELTAY,DELTAZ
      CHARACTER*4 CELNAME
      REAL    YDUM
      LOGICAL PRAN
      DATA PRAN /.TRUE./
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('ISARCP_RCP')
C
        CALL EZGETA('ETA_PHI_CELL',0,0,0,NCELLS,IER)
        NCELLS = NCELLS/2               ! 2 entries per cell
        IF(NCELLS.GT.NCELL_MAX)CALL ERRMSG('IS1TRK','ISARCP',
     &    'NUMBER OF CELLS EXCEEDS DIMENSION ALLOWED ','W')
C
        CALL EZGET('ETA_PHI_CELL',ETA_PHI_CELL,IER)
C
        BWID = 0.0
        CALL EZGETA('PARTICLE_MOMENTUM',0,0,0,NMOMC,IER)
        NM = NMOMC
        IF(NMOMC .EQ. 1) PRAN = .FALSE.
        IF(NMOMC .GT. 2 .AND. MOD(NMOMC,2) .EQ. 1) NMOMC = NMOMC + 1
        NMOMC = NMOMC/2
        IF(NMOMC.GT.NMOMC_MAX)CALL ERRMSG('IS1TRK','ISARCP',
     &    'NUMBER OF MOMENTUM CELLS EXCEEDS DIMENSION ALLOWED ','W')
        CALL EZGET('PARTICLE_MOMENTUM',MOMSPEC,IER)
        IF(NM .GT. 2 ) MOMSPEC(NM+1) = 0.0
        IF(PRAN .AND. NMOMC .LT. 2) THEN
          NMOMC = 3
          TEMP1 = MOMSPEC(1)
          TEMP2 = MOMSPEC(2)
          MOMSPEC(1) = TEMP1 - TEMP2
          MOMSPEC(3) = TEMP1
          MOMSPEC(5) = TEMP1 + TEMP2
          MOMSPEC(2) = 1.0
          MOMSPEC(4) = 1.0
          MOMSPEC(6) = 0.0
          BWID = TEMP2
        ENDIF
        IF(.NOT. PRAN) THEN
          GOTO 11
        ENDIF
C
        J = MOMSPEC(1)
        LOWEDG = J
        IF(BWID .LT. 1.0) BWID = 1.0
        BWID = FLOAT(INT(BWID))
        MOMENTUM_SPEC(J) = MOMSPEC(2)
        N_MOM_CELL = 1
        NORM = MOMENTUM_SPEC(J)
        DO I=1,J-1
          MOMENTUM_SPEC(I) = 0.0
        ENDDO
        DO I=3,NMOMC*2
          IF(MOD(I,2) .EQ. 1) THEN
            JB = J
            J = MOMSPEC(I)
          ELSE
            DO K=JB+1,J
              MOMENTUM_SPEC(K) = MOMSPEC(I-2)
              N_MOM_CELL = N_MOM_CELL + 1
              NORM = NORM + MOMENTUM_SPEC(K)
            ENDDO
          ENDIF
        ENDDO
   11   CONTINUE
C
        CALL EZGET('PARTICLE_ID',PARTICLE_ID,IER)
CCC        CALL EZGET('PARTICLE_MOM',PARTICLE_MOM,IER)
        CALL EZGET('PRIMARY_VERTEX',VERTEX,IER)
        CALL EZGET('VERTEX_Z_SIGMA',VERTEX_Z_SIGMA,IER)
        CALL EZGET('CELL_HISTOGRAM_WINDOW_X',DELTAX,IER)
        CALL EZGET('CELL_HISTOGRAM_WINDOW_Y',DELTAY,IER)
        CALL EZGET('CELL_HISTOGRAM_WINDOW_Z',DELTAZ,IER)
C
        IF(IER.NE.0)CALL ERRMSG('IS1TRK','ISAGEN',
     &    'ERROR READING IS1TRK_RCP VARIABLE ','W')
C
        CALL CAISTP('CAL_STPFILE',IER)
        IF(IER.NE.0)CALL ERRMSG('IS1TRK','ISAGEN',
     &    'ERROR READING CAL_STPFILE','W')
C
        DO 100 ICELL = 1 , NCELLS
C
          IL = 1
          IF(ETA_PHI_CELL(1,ICELL) .EQ. 13) IL = 9
          IF(ETA_PHI_CELL(1,ICELL) .EQ. 14) IL = 15
          IF(ETA_PHI_CELL(1,ICELL) .EQ. 36) IL = 11
          IF(ETA_PHI_CELL(1,ICELL) .EQ. 37) IL = 13
          CALL CELVEC(ETA_PHI_CELL(1,ICELL),ETA_PHI_CELL(2,ICELL),IL,
     &        XS(1,1,ICELL),YS(1,1,ICELL),ZS(1,1,ICELL),
     &        NS(ICELL),IER)
          IF(IER.NE.0)CALL ERRMSG('IS1TRK','ISAGEN',
     &    'ERROR IN CELVEC ','W')
C

          X1(ICELL) = XS(1,1,ICELL)
          Y1(ICELL) = YS(1,1,ICELL)
          Z1(ICELL) = ZS(1,1,ICELL)                    ! STARTING POINT OF 1ST VECTOR
          IFOUND = 0
          DO 10 IPT = 1 , 2
            DO 20 IS = 2 , NS(ICELL)
              IF(XS(IPT,IS,ICELL).NE.X1(ICELL))GO TO 20
              IF(YS(IPT,IS,ICELL).NE.Y1(ICELL))GO TO 20
              IF(ZS(IPT,IS,ICELL).NE.Z1(ICELL))GO TO 20
C FOUND AN END POINT SAME AS 1ST VECTOR START.
              IFOUND = IFOUND + 1
              ISTART(IFOUND) = IPT
              SIDE(IFOUND) = IS
   20       CONTINUE
   10     CONTINUE
          IF(IFOUND.NE.2)CALL ERRMSG('ISARCP','IS1TRK',
     &    'CELVEC - BAD CELL SHAPE ','W')
C
          DELTAR(1,1,ICELL)=XS(2,1,ICELL)-XS(1,1,ICELL)
          DELTAR(2,1,ICELL)=YS(2,1,ICELL)-YS(1,1,ICELL)
          DELTAR(3,1,ICELL)=ZS(2,1,ICELL)-ZS(1,1,ICELL)
C
C ****  DELTAR (*,3) ARE THE 3 VECTOR SIDES OF THE CELL STARTING
C ****  AT XS1,YS1,ZS1
C
          DO 30 IFOUND = 1,2
            IF1 = IFOUND+1
            IST = ISTART(IFOUND)
            IS = SIDE(IFOUND)
            IEND = MOD(IST,2) + 1         ! IST STARTS AT XS1,YS1,ZS1
            DELTAR(1,IF1,ICELL)=XS(IEND,IS,ICELL)
     &          - XS(IST,IS,ICELL)
            DELTAR(2,IF1,ICELL)=YS(IEND,IS,ICELL)
     &          - YS(IST,IS,ICELL)
            DELTAR(3,IF1,ICELL)=ZS(IEND,IS,ICELL)
     &          - ZS(IST,IS,ICELL)
C
   30     CONTINUE
  100   CONTINUE
C
C ****  book histograms
C
        CALL HBOOK1(1,'VERTEX Z POSITION ',50,-5*VERTEX_Z_SIGMA,5*
     &    VERTEX_Z_SIGMA,0.)
C
        IOFF_ST = 100
        IOFF = IOFF_ST                  ! Starting IOFF
        IOFF_DEL = 5                    ! Number of hists per cell
C
        DO 150 ICELL = 1 , NCELLS
          WRITE(CELNAME,151)ICELL
  151     FORMAT(I4)
C
          CALL CALETA(ETA_PHI_CELL(1,ICELL),ETALO,DELETA,IER)
          ETALO = ETALO - 5*DELETA
          ETAHI = ETALO + 10*DELETA
          CALL CALPHI(ETA_PHI_CELL(2,ICELL),ETA_PHI_CELL(1,ICELL),
     &      PHILO,DELPHI,IER)
          PHILO = PHILO - 5*DELPHI
          PHIHI = PHILO + 10*DELPHI
          PHILO = PHILO/RADIAN
          PHIHI = PHIHI/RADIAN          ! in degrees
          CALL HBOOK1(IOFF+1,'ETA OF TRACK IN CELL'//CELNAME,
     &    50,ETALO,ETAHI,0.)
          CALL HBOOK1(IOFF+2,'PHI OF TRACK IN CELL'//CELNAME,
     &    50,PHILO,PHIHI,0.)
C
          XLO = X1(ICELL) - DELTAX
          XHI = X1(ICELL) + DELTAX
          YLO = Y1(ICELL) - DELTAY
          YHI = Y1(ICELL) + DELTAY
          ZLO = Z1(ICELL) - DELTAZ
          ZHI = Z1(ICELL) + DELTAZ
C
          CALL HBOOK2(IOFF+3,
     &    'X VS Y OF TRACK END POINT IN CELL'//CELNAME,
     &    50,XLO,XHI,50,YLO,YHI,0.)
          CALL HBOOK2(IOFF+4,
     &    'Z VS Y OF TRACK END POINT IN CELL'//CELNAME,
     &    50,ZLO,ZHI,50,YLO,YHI,0.)
          CALL HBOOK2(IOFF+5,
     &    'X VS Z OF TRACK END POINT IN CELL'//CELNAME,
     &    50,XLO,XHI,50,ZLO,ZHI,0.)
          IOFF = IOFF+IOFF_DEL
  150   CONTINUE
C
        CALL EZRSET
      ENDIF
C
C ****  generate the points at random in the cell.
C
C
C ****  first generate ICELL of cell.
C
      DUM = RNDM(DUM)
      ICELL = NCELLS*DUM + 1
C
      REND(1) = X1(ICELL)
      REND(2) = Y1(ICELL)
      REND(3) = Z1(ICELL)
C
      DO 40 IS = 1 , 3
        DUM = RNDM(DUM)
        REND(1) = REND(1) + DUM*DELTAR(1,IS,ICELL)
        REND(2) = REND(2) + DUM*DELTAR(2,IS,ICELL)
        REND(3) = REND(3) + DUM*DELTAR(3,IS,ICELL)
   40 CONTINUE
C
C ****  REND IS NOW A RANDOM POINT IN THE CELL.
C
      DO 50 IS = 1 , 3
        PRIMARY_VERTEX(IS) = VERTEX(IS)
   50 CONTINUE
C
      CALL NORRAN(GAUSS)
      PRIMARY_VERTEX(3) = PRIMARY_VERTEX(3) + VERTEX_Z_SIGMA*GAUSS
C
      DVEC = 0.
      DO 60 IS = 1 , 3
        DVEC = DVEC + (REND(IS) - PRIMARY_VERTEX(IS))**2
   60 CONTINUE
      DVEC = SQRT(DVEC)
      IF(PRAN) THEN
        CALL ISGETP(MOMENTUM_SPEC,N_MOM_CELL,LOWEDG,BWID,
     &  PARTICLE_MOM)
      ELSE
        PARTICLE_MOM = MOMSPEC(1)
      ENDIF
      DO 70 IS = 1 , 3
        THREE_MOMENTUM(IS) = (REND(IS)-PRIMARY_VERTEX(IS))
     &    *PARTICLE_MOM/DVEC
   70 CONTINUE
C
C ****  now fill histograms
C
      THETA = ATAN2(SQRT(THREE_MOMENTUM(2)**2+THREE_MOMENTUM(1)**2),
     &  THREE_MOMENTUM(3))
      ETA = -ALOG(TAN(THETA*0.5))
      PHI = ATAN2(THREE_MOMENTUM(2),THREE_MOMENTUM(1))
      PHI = PHI/RADIAN
      IF(PHI.LT.0.0)PHI = PHI + 360.    ! 0 TO 360. NOW.
C
      CALL HFILL(1,PRIMARY_VERTEX(3),YDUM,1.0)
      IOFF = IOFF_ST + (ICELL-1)*IOFF_DEL
      CALL HFILL(IOFF+1,ETA,YDUM,1.0)
      CALL HFILL(IOFF+2,PHI,YDUM,1.0)
      CALL HFILL(IOFF+3,REND(1),REND(2),1.0)
      CALL HFILL(IOFF+4,REND(3),REND(2),1.0)
      CALL HFILL(IOFF+5,REND(1),REND(3),1.0)
C
  999 RETURN
      END

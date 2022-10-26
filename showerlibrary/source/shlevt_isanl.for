      SUBROUTINE SHLEVT_ISANL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyze Isajet events using Showerlibrary
C-   for missing pt resolution
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   7-JAN-1990   Rajendran Raja
C-                           BASED ON ANLMUO OF OLD
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBIO.INC'
      INCLUDE 'D0$LINKS:IZISAE.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISV1.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISP1.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMUD1.LINK/LIST'
      INCLUDE 'D0$INC:DATARR.INC/LIST'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER LISAE,LISV1,LISP1,LJET
      EQUIVALENCE (CSTLNK(31),LISAE),(CSTLNK(32),LISV1)
      EQUIVALENCE (CSTLNK(33),LISP1),(CSTLNK(34),LJET)
C
      INTEGER IER
      INTEGER GZISAE,GZISV1
C
      REAL ETNUY,ETNUZ
C
      INTEGER ISTATS,IRTYPE,NEVENT,IJET
C
      INTEGER NCALL,NT
      REAL WT,TRMS1,TRMS2
      REAL T1,T2,DT1,DT2,ADC11,ADC12,ADC21,ADC22
C
      INTEGER I,J,K
      REAL    SET,VETALL(4),VET(4),ET2
      REAL    SET5,SET10
      INTEGER IPID,IPIDG3         ! Isajet and Geant particle ID.
      REAL  PX,PY,PZ,PT,PP,AM     ! momenta and mass of particle.
      REAL  PHI,THETA,ETA         ! phi, theta and eta of particle.
      REAL  EE,EX,EY,EZ,ET        ! Energy vector and Et.
      REAL  SOVRE,THCUT,XX
      INTEGER IIRUN(10),JJRUN
      REAL    WTRUN(10)
      INTEGER IPFL
      REAL    DE
      LOGICAL USE_WEIGHT
C
      DATA NCALL/0/
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      INTEGER NCH
      CHARACTER*80 GLB_TITLE
C
      INTEGER SSUNIT
C----------------------------------------------------------------------
      CALL DHDIR('SHOWERLIBRARY_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('CALORIMETER','SHLEVT',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('SHOWERLIBRARY_RCP')
        CALL EZGET('USE_WEIGHT',USE_WEIGHT,IER)
        CALL EZ_GET_CHARS('GLOBAL_HIST_TITLE',NCH,GLB_TITLE,IER)
        CALL EZRSET
C
        CALL HTITLE(GLB_TITLE)
C
        CALL HBOOK1(1,'NUMBER OF PARTICLES',100,0.,500.,0.)
        CALL HBOOK1(6,'ET2 (MISSING ET SQUARE',100,0.,400.,0.)
        CALL HBOOK1(7,'SET (SCALAR ET)',100,0.,200.,0.)
        CALL HBOOK1(8,'SET (SCALAR ET)',100,0.,1000.,0.)
C
        CALL HBOOK1(9,'SET (SCALAR ET),THETA>5DEG.',100,0.,1000.,0.)
        CALL HBOOK1(10,'SET (SCALAR ET), THETA>10DEG.',100,0.,1000.,0.)
        CALL HBOOK1(12,'SUM OF ALL EX',100,-10.0,10.0,0.)
        CALL HBOOK1(13,'SUM OF ALL EY',100,-10.0,10.0,0.)
        CALL HBOOK1(14,'SUM OF ALL EZ',100,-500.0,500.0,0.)
        CALL HBOOK1(15,'SUM OF ALL ENERGY',100,0.,1000.,0.)
C
        CALL HBOOK1(21,'ET2,  FOR SET=0-10',100,0.,200.,0.)
        CALL HBOOK1(22,'ET2,  FOR SET=10-20',100,0.,200.,0.)
        CALL HBOOK1(23,'ET2,  FOR SET=20-40',100,0.,200.,0.)
        CALL HBOOK1(24,'ET2,  FOR SET=40-80',100,0.,200.,0.)
        CALL HBOOK1(25,'ET2,  FOR SET=80-160',100,0.,200.,0.)
        CALL HBOOK1(26,'ET2,  FOR SET=160-  ',100,0.,200.,0.)
        CALL HBOOK1(31,'SET,  FOR SET=0-10',100,0.,200.,0.)
        CALL HBOOK1(32,'SET,  FOR SET=10-20',100,0.,200.,0.)
        CALL HBOOK1(33,'SET,  FOR SET=20-40',100,0.,200.,0.)
        CALL HBOOK1(34,'SET,  FOR SET=40-80',100,0.,200.,0.)
        CALL HBOOK1(35,'SET,  FOR SET=80-160',100,0.,200.,0.)
        CALL HBOOK1(36,'SET,  FOR SET=160-  ',100,0.,200.,0.)
C
        CALL HBOOK1(27,'RANDOM GAUSSIAN DIST.',100,-5.0,5.0,0.)
        CALL HBOOK1(28,'DE/SQRT(E) FOR E+,E-',100,-1.,1.,0.)
        CALL HBOOK1(29,'DE/SQRT(E) FOR MU+,MU-',100,-1.,1.,0.)
        CALL HBOOK1(30,'DE/SQRT(E) FOR HADRONS',100,-2.,2.,0.)
        CALL HBOOK1(40,'X=ET2/(0.5*SET)',100,0.,10.,0.)
C
C     -- for each particle...
C
        CALL HBOOK1(50,'(GEANT) PARTICLE ID',100,1.,100.,0.)
        CALL HBOOK1(51,'P OF PARTICLE',100,0.,200.,0.)
        CALL HBOOK1(52,'PT OF PARTICLE',100,0.,20.,0.)
        CALL HBOOK1(53,'PZ OF PARTICLE',100,0.,100.,0.)
        CALL HBOOK1(54,'PHI OF PARTICLE',100,0.,400.,0.)
        CALL HBOOK1(55,'THETA OF PARTICLE',100,0.,180.,0.)
        CALL HBOOK1(56,'ETA OF PARTICLE',100,-5.,5.,0.)
        CALL HBOOK1(61,'ET OF PARTICLE',100,0.,20.,0.)
      ENDIF
C
C ****  ANALYZE DATA
C
C  Analyze ISAJET events.
C  ======================
C
      NCALL = NCALL + 1
C
      LISAE=GZISAE()                  ! GET LINK
C
      JTYP=1             !Type (obsolete)
      JRUN=IQ(LISAE+2)      !Run number
      JEVT=IQ(LISAE+3)      !Event number
      NTYP1=1
      NTYP2=1
      IERR=0
      WT = 1.0
C     -- set event weight...
      IF(USE_WEIGHT)WT = Q(LISAE+12)*2.0        ! do only 500 evts
C     -- clear variables...
      SET=0.
      SET5=0.
      SET10=0.
      ERZ=0.
      ERY=0.     !MEASUREMENT ET ERRORS IN UA1 CO-ORDS.
      DO 10 I=1,4
        VET(I)=0.
        VETALL(I)=0.
   10 CONTINUE
C     -- double loops over isv1 and isp1.
      LISV1=GZISV1()
      NT=0                              ! number of tracks.
      DO WHILE (LISV1 .NE. 0)
        LISP1=LQ(LISV1-IZISP1)          ! NO GZISP1!!
        DO WHILE (LISP1 .NE. 0)
          NT=NT+1
C
          IPID=IQ(LISP1+1)
C        -- convert isajet particle id to geant particle id.
          CALL ISAGEA(IPID,IPIDG3)
          PX=Q(LISP1+2)
          PY=Q(LISP1+3)
          PZ=Q(LISP1+4)
          PT=SQRT(PX**2+PY**2)
          PP=Q(LISP1+5)
          AM=Q(LISP1+6)
          PHI=Q(LISP1+7)*57.296      ! phi in degree
          THETA=Q(LISP1+8)*57.296    ! theta in degree
          ETA=Q(LISP1+9)
C
          EE=SQRT(PP**2+AM**2)
c-----skip-----abnormal event-----skip-----abnormal event----------
          IF(pp.lt.0.00001) THEN
            WRITE(SSUNIT(),62) pp
   62       FORMAT('   ******** abnormal event ***********'
     +           ,1pe10.5)
            RETURN
          ENDIF
C---------------smearing energy------------------------------------
  120     CONTINUE
C
          DE=0.
C
          CALL ISA_SMEAR(IPIDG3,EE,DE,IPFL)  ! SMEAR ACCORDING TO ISAJET
C
          IF(IPFL.LE.3)THEN
            SOVRE=DE/SQRT(EE)
            CALL HFILL(27+IPFL,SOVRE,0.,WT)
          ENDIF
C
          EE=EE+DE
          IF(EE.LE.0.0) EE=0.001
C------------------------------------------------------------------
          EX=EE*PX/PP
          EY=EE*PY/PP
          EZ=EE*PZ/PP
C
          ET=SQRT(EX**2+EY**2)
C
C-------------track selection---------------------------------------
CC            IF(THETA.LT.5.) GO TO 190
          THCUT=(2.54*1.5/150.0)*57.296
          IF(THETA.LT.THCUT.OR.THETA.GT.(180.-THCUT)) GO TO 190
C-------------------------------------------------------------------
          VETALL(1)=VETALL(1)+EX
          VETALL(2)=VETALL(2)+EY
          VETALL(3)=VETALL(3)+EZ
          VETALL(4)=VETALL(4)+EE
C
          SET = SET + ET                ! SCALAR ET
          IF(THETA.GT.5.0.AND.THETA.LT.175.)SET5 = SET5 + ET
          IF(THETA.GT.10.0.AND.THETA.LT.170.)SET10 = SET10 + ET
C
C        -- fill histograms for each particle...
C
          CALL HFILL(50,FLOAT(IPIDG3),0.,WT) !   Geant particle ID.
          CALL HFILL(51,PP,0.,WT)
          CALL HFILL(52,PT,0.,WT)
          CALL HFILL(53,PZ,0.,WT)
          CALL HFILL(54,PHI,0.,WT)
          CALL HFILL(55,THETA,0.,WT)
          CALL HFILL(56,ETA,0.,WT)
C
          CALL HFILL(61,ET,0.,WT)
C
  190     CONTINUE      ! jump to here if track be skipped...
C        -- going to next linear link (isp1)...
          LISP1=LQ(LISP1)
        ENDDO
C     -- going to next isv1 linear link...
        LISV1=LQ(LISV1)
      ENDDO
C     -- no more particles.  end of double loops.
C
C        calculte event property...
C       ============================
C
      ET2=VETALL(1)**2+VETALL(2)**2
C
      CALL HFILL(1,FLOAT(NT),0.,WT)   ! number of particles.
      CALL HFILL(6,ET2,0.,WT)         ! missing ET squre.
      CALL HFILL(7,SET,0.,WT)         ! scalar sum of ET.
      CALL HFILL(8,SET,0.,WT)         !     :     (big scale).
      CALL HFILL(9,SET5,0.,WT)        !     :     (th>5 degree)
      CALL HFILL(10,SET10,0.,WT)      !     :     (th>10 degree)
      CALL HFILL(12,VETALL(1),0.,WT)      ! sum of all Ex.
      CALL HFILL(13,VETALL(2),0.,WT)      ! sum of all Ey.
      CALL HFILL(14,VETALL(3),0.,WT)      ! sum of all Ez.
      CALL HFILL(15,VETALL(4),0.,WT)      ! sum of all Energy.
C
      IF(SET.LT.10.) THEN
        CALL HFILL(21,ET2,0.,WT)
        CALL HFILL(31,SET,0.,WT)
      ELSE IF(SET.GT.10.0.AND.SET.LT.20.) THEN
        CALL HFILL(22,ET2,0.,WT)
        CALL HFILL(32,SET,0.,WT)
      ELSE IF(SET.GT.20.0.AND.SET.LT.40.) THEN
        CALL HFILL(23,ET2,0.,WT)
        CALL HFILL(33,SET,0.,WT)
      ELSE IF(SET.GT.40.0.AND.SET.LT.80.) THEN
        CALL HFILL(24,ET2,0.,WT)
        CALL HFILL(34,SET,0.,WT)
      ELSE IF(SET.GT.80.0.AND.SET.LT.160.0) THEN
        CALL HFILL(25,ET2,0.,WT)
        CALL HFILL(35,SET,0.,WT)
      ELSE
        CALL HFILL(26,ET2,0.,WT)
        CALL HFILL(36,SET,0.,WT)
      ENDIF
C
      XX=ET2/(0.5*SET)
      CALL HFILL(40,XX,0.,WT)
C
  999 RETURN
      END


      SUBROUTINE SHLEVT_SHLANL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SHLEVT routine to ananlyze SHOWERLIBRARY
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   4-JAN-1990   Rajendran Raja
C-   Updated  29-JAN-1991   W.G.D.Dharmaratna  Modified for calibration 
C-                                             studies
C-   Updated   3-DEC-1992   W. Dharmaratna   BACK TO DROPPED ENERGY
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:QUEST.INC/LIST'
      INCLUDE 'D0$INC:SHLCON.INC'
      INCLUDE 'D0$INC:SHLDAT.INC'
C      INCLUDE 'D0$INC:PAWC.INC'
C
      INTEGER ICYCLE,NCOUNT
C
      EXTERNAL NZBANK
      INTEGER NZBANK
C
      INTEGER ITRK,NB
      INTEGER I1,I2,I3
      INTEGER IER
      REAL    ETABIN,YDUM
      INTEGER IDNUM,NMBIN2
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      CHARACTER*80 TITLE
      CHARACTER*25 CHV
      CHARACTER*30 CHM
      INTEGER LENT,TRULEN
C
      REAL    FRC_ENR_CAL,SIG_FRC_ENR_CAL,FRAC_EN
      REAL    FRC_ENR_ISA,SIG_FRC_ENR_ISA
      REAL    FRC_ENR_ICD,SIG_FRC_ENR_ICD
      REAL    FRC_ENR_MG1,SIG_FRC_ENR_MG1
      REAL    FRC_ENR_MG2,SIG_FRC_ENR_MG2
      REAL    FRC_ENR_DEAD,SIG_FRC_ENR_DEAD,FRC_DROP
      REAL    CHECK_TOTAL,CHECK_DEAD,CHECK_DROP
C
      INTEGER SSUNIT
      INTEGER NLENGTH
      INTEGER MOMENTUM_LOW_KEY,MOMENTUM_HIGH_KEY
      INTEGER PARTICLE_LOW_KEY,PARTICLE_HIGH_KEY
      INTEGER VERTEX_LOW_KEY,VERTEX_HIGH_KEY
      INTEGER I1D,I11
C
      REAL    ISA_ENERGY,TOT_ENERGY,DROP_ENERGY
      REAL    TOT_CAL_ENERGY,TOT_DEAD_ENERGY,TOT_ICD_ENERGY,
     &  TOT_MG1_ENERGY,TOT_MG2_ENERGY,ENER_MISS(4)
      INTEGER NUM_GCAH,NUM_HITS,PT_GCAH,NUMHITS
      INTEGER ISA_PARTID,GEAPART,KEY_ID
      INTEGER INDX_KEY
C----------------------------------------------------------------------
C      CALL RZCDIR('//SHOWER_LIBRARY',' ')
C
      CALL DHDIR('SHOWERLIBRARY_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('CALORIMETER','SHLEVT',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        ICYCLE = NCYCLE_MAX
        CALL EZPICK('SHOWERLIBRARY_RCP')
        CALL EZGET( 'MOMENTUM_LOW_KEY ',MOMENTUM_LOW_KEY ,IER)
        CALL EZGET('MOMENTUM_HIGH_KEY',MOMENTUM_HIGH_KEY,IER)
        CALL EZGET('PARTICLE_LOW_KEY ',PARTICLE_LOW_KEY ,IER)
        CALL EZGET('PARTICLE_HIGH_KEY ',PARTICLE_HIGH_KEY ,IER)
        CALL EZGET('VERTEX_LOW_KEY',VERTEX_LOW_KEY,IER)
        CALL EZGET('VERTEX_HIGH_KEY',VERTEX_HIGH_KEY,IER)
        CALL EZRSET
C
        CALL HBOOK1(1000,'TRACKS STORED PER BIN',
     +             100,-.5,999.5,0.)
c        CALL HBOOK1(10,'TOT DEAD ENERGY', 100,0.0,400.0,0.0)
c        CALL HBOOK1(20,'TOT CAL ENERGY', 100,0.0,1500.0,0.0)
c        CALL HBOOK1(30,'TOT DROP ENERGY', 100,0.0,100.0,0.0)

        DO I3 = VERTEX_LOW_KEY,VERTEX_HIGH_KEY
          WRITE(CHV,1)VTXBIN(I3),VTXBIN(I3+1)
    1     FORMAT(' VTX = ',F8.2,'-',F8.2)
          DO I1 = MOMENTUM_LOW_KEY,MOMENTUM_HIGH_KEY
            DO I11 = PARTICLE_LOW_KEY,PARTICLE_HIGH_KEY
              WRITE(CHM,2)MOMBIN(I1),MOMBIN(I1+1),PARTAG(I11)
    2         FORMAT(' MOM = ',F8.2,'-',F8.2,2X,A3)
              I1D = I1 + (I11-1)*NMBIN
              IDNUM = 1000 + 100*I3  + I1D
              TITLE = 'TRACKS '//CHV//CHM
              LENT = TRULEN(TITLE)
              CALL HBOOK1
     &        (IDNUM,TITLE(1:LENT),NEBIN,1.,FLOAT(NEBIN+1),0.)
C
              IDNUM = 2000 + 100*I3  + I1D
              TITLE = 'FRAC.ENR.CAL '//CHV//CHM
              LENT = TRULEN(TITLE)
              CALL HBOOK1
     &        (IDNUM,TITLE(1:LENT),NEBIN,1.,FLOAT(NEBIN+1),0.)
C
              IDNUM = 3000 + 100*I3  + I1D
              TITLE = 'SIG.FRAC.ENR.CAL '//CHV//CHM
              LENT = TRULEN(TITLE)
              CALL HBOOK1
     &        (IDNUM,TITLE(1:LENT),NEBIN,1.,FLOAT(NEBIN+1),0.)
C
              IDNUM = 4000 + 100*I3  + I1D
              TITLE = 'FRAC.ENR.DEAD '//CHV//CHM
              LENT = TRULEN(TITLE)
              CALL HBOOK1
     &        (IDNUM,TITLE(1:LENT),NEBIN,1.,FLOAT(NEBIN+1),0.)
C
              IDNUM = 5000 + 100*I3  + I1D
              TITLE = 'SIG.FRAC.ENR.DEAD '//CHV//CHM
              LENT = TRULEN(TITLE)
              CALL HBOOK1
     &        (IDNUM,TITLE(1:LENT),NEBIN,1.,FLOAT(NEBIN+1),0.)
C
              IDNUM = 6000 + 100*I3  + I1D
              TITLE = 'FRAC.ENR.ISA '//CHV//CHM
              LENT = TRULEN(TITLE)
              CALL HBOOK1
     &        (IDNUM,TITLE(1:LENT),NEBIN,1.,FLOAT(NEBIN+1),0.)
C
              IDNUM = 7000 + 100*I3  + I1D
              TITLE = 'SIG.FRAC.ENR.ISA '//CHV//CHM
              LENT = TRULEN(TITLE)
              CALL HBOOK1
     &        (IDNUM,TITLE(1:LENT),NEBIN,1.,FLOAT(NEBIN+1),0.)
C
              IDNUM = 8000 + 100*I3  + I1D
              TITLE = 'FRAC.ENR.ICD '//CHV//CHM
              LENT = TRULEN(TITLE)
              CALL HBOOK1
     &        (IDNUM,TITLE(1:LENT),NEBIN,1.,FLOAT(NEBIN+1),0.)
C
              IDNUM = 9000 + 100*I3  + I1D
              TITLE = 'SIG.FRAC.ENR.ICD '//CHV//CHM
              LENT = TRULEN(TITLE)
              CALL HBOOK1
     &        (IDNUM,TITLE(1:LENT),NEBIN,1.,FLOAT(NEBIN+1),0.)
C
              IDNUM = 10000 + 100*I3  + I1D
              TITLE = 'FRAC.ENR.MG1 '//CHV//CHM
              LENT = TRULEN(TITLE)
              CALL HBOOK1
     &        (IDNUM,TITLE(1:LENT),NEBIN,1.,FLOAT(NEBIN+1),0.)
C
              IDNUM = 11000 + 100*I3  + I1D
              TITLE = 'SIG.FRAC.ENR.MG1 '//CHV//CHM
              LENT = TRULEN(TITLE)
              CALL HBOOK1
     &        (IDNUM,TITLE(1:LENT),NEBIN,1.,FLOAT(NEBIN+1),0.)
C
              IDNUM = 12000 + 100*I3  + I1D
              TITLE = 'FRAC.ENR.MG2 '//CHV//CHM
              LENT = TRULEN(TITLE)
              CALL HBOOK1
     &        (IDNUM,TITLE(1:LENT),NEBIN,1.,FLOAT(NEBIN+1),0.)
C
              IDNUM = 13000 + 100*I3  + I1D
              TITLE = 'SIG.FRAC.ENR.MG2 '//CHV//CHM
              LENT = TRULEN(TITLE)
              CALL HBOOK1
     &        (IDNUM,TITLE(1:LENT),NEBIN,1.,FLOAT(NEBIN+1),0.)
C
              IDNUM = 14000 + 100*I3  + I1D
              TITLE = 'FRAC.DROP ENERGY '//CHV//CHM
              LENT = TRULEN(TITLE)
              CALL HBOOK1
     &        (IDNUM,TITLE(1:LENT),NEBIN,1.,FLOAT(NEBIN+1),0.)

            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
      ITRK=0
      CHECK_TOTAL = 0.0
      CHECK_DEAD = 0.0
      CHECK_DROP = 0.0
C
C ****  loop over all entries in the library
C
      DO 120 I3=VERTEX_LOW_KEY,VERTEX_HIGH_KEY
        DO 110 I2=1,NEBIN
          ETABIN = I2
          DO 100 I1=MOMENTUM_LOW_KEY,MOMENTUM_HIGH_KEY
            DO 105 I11=PARTICLE_LOW_KEY,PARTICLE_HIGH_KEY
              I1D = I1 + (I11-1)*NMBIN
C
C ****  set key vector
C
              KEY(1)=I3
              KEY(2)=I2
              KEY(3)=I1
              KEY(4)= I11               ! PARTICLE ID
              KEY(5)= 1 ! 2 FOR CRACK                 ! PHI SECTOR FOR NOW
C
              ICYCLE=NCYCLE_MAX
C
C ****  read in the shower library array for each key
C
C              CALL RZCDIR('//SHOWER_LIBRARY',' ')
C
C ****  find number of banks in the linear array
C
              NB = CYCLES(INDX_KEY(KEY))
              IF(NB.NE.0)THEN
                FRC_ENR_CAL = 0.
                SIG_FRC_ENR_CAL = 0.
                FRC_ENR_DEAD = 0.
                SIG_FRC_ENR_DEAD = 0.
                FRC_ENR_ISA = 0.
                SIG_FRC_ENR_ISA = 0.
                FRC_ENR_ICD = 0.
                SIG_FRC_ENR_ICD = 0.
                FRC_ENR_MG1 = 0.
                SIG_FRC_ENR_MG1 = 0.
                FRC_ENR_MG2 = 0.
                SIG_FRC_ENR_MG2 = 0.
                FRC_DROP = 0.

C
                DO ICYCLE = 1 , NB
C
                  CALL RZVIN1(SHLB,2*NHMAX,NDATA,KEY,ICYCLE,' ')
C
                  CALL SHLB_STAT(SHLB,ISA_PARTID,ISA_ENERGY,
     &              TOT_ENERGY,TOT_CAL_ENERGY,TOT_DEAD_ENERGY,
     &              TOT_ICD_ENERGY,TOT_MG1_ENERGY,TOT_MG2_ENERGY,
     &              ENER_MISS,NUM_GCAH,NUM_HITS,DROP_ENERGY)
C
                  CALL ISAGEA(ISA_PARTID,GEAPART)      ! CONVERT TO GEANT.
                  CALL GEAN_KEYID(GEAPART,KEY_ID)   ! KEY_ID GIVEN GEAN_PARTID
                  IF(KEY_ID.NE.I11)THEN
                    CALL ERRMSG('SHOWERLIBRARY','SHLEVT_SHLANL',
     &                'SHLB PART ID DIFFERENT FROM THAT ASKED FOR ','W')
                    CALL ERROR
                  ENDIF
C
C ****  CHECK FOR TOTALS
C
                  CHECK_TOTAL = CHECK_TOTAL + TOT_CAL_ENERGY
                  CHECK_DEAD = CHECK_DEAD + TOT_DEAD_ENERGY
                  CHECK_DROP = CHECK_DROP + DROP_ENERGY

                  FRAC_EN = TOT_CAL_ENERGY/TOT_ENERGY ! CAL ENERGY FRACTION
                  FRC_ENR_CAL = FRAC_EN + FRC_ENR_CAL
                  SIG_FRC_ENR_CAL = FRAC_EN*FRAC_EN + SIG_FRC_ENR_CAL
C
                  FRAC_EN = TOT_DEAD_ENERGY/TOT_ENERGY ! DEAD ENERGY FRACTION
                  FRC_ENR_DEAD = FRAC_EN + FRC_ENR_DEAD
                  SIG_FRC_ENR_DEAD = FRAC_EN*FRAC_EN + SIG_FRC_ENR_DEAD
C
                  FRAC_EN = TOT_ENERGY/ISA_ENERGY ! ENTERED ENERGY FRACTION
                  FRC_ENR_ISA = FRAC_EN + FRC_ENR_ISA
                  SIG_FRC_ENR_ISA = FRAC_EN*FRAC_EN + SIG_FRC_ENR_ISA
                 
c                 IF ( FRAC_EN .GT. 1.1) THEN
c                   CALL PRSHLB(SHLB)
c                   NCOUNT = NCOUNT + 1
c                   IF (NCOUNT .GT. 10 ) GOTO 999
c                 ENDIF
C
                  FRAC_EN = TOT_ICD_ENERGY/TOT_ENERGY ! ICD ENERGY FRACTION
                  FRC_ENR_ICD = FRAC_EN + FRC_ENR_ICD
                  SIG_FRC_ENR_ICD = FRAC_EN*FRAC_EN + SIG_FRC_ENR_ICD
C
                  FRAC_EN = TOT_MG1_ENERGY/TOT_ENERGY ! MG1 ENERGY FRACTION
                  FRC_ENR_MG1 = FRAC_EN + FRC_ENR_MG1
                  SIG_FRC_ENR_MG1 = FRAC_EN*FRAC_EN + SIG_FRC_ENR_MG1
C
                  FRAC_EN = TOT_MG2_ENERGY/TOT_ENERGY ! MG2 ENERGY FRACTION
                  FRC_ENR_MG2 = FRAC_EN + FRC_ENR_MG2
                  SIG_FRC_ENR_MG2 = FRAC_EN*FRAC_EN + SIG_FRC_ENR_MG2
C
                  FRAC_EN = DROP_ENERGY/ISA_ENERGY ! MG2 ENERGY FRACTION
                  FRC_DROP = FRAC_EN + FRC_DROP

                  
                ENDDO
C
                FRC_ENR_CAL = FRC_ENR_CAL/NB
                SIG_FRC_ENR_CAL = SIG_FRC_ENR_CAL/NB
                SIG_FRC_ENR_CAL = SQRT(SIG_FRC_ENR_CAL - FRC_ENR_CAL**2)
C
                FRC_ENR_DEAD = FRC_ENR_DEAD/NB
                SIG_FRC_ENR_DEAD = SIG_FRC_ENR_DEAD/NB
                SIG_FRC_ENR_DEAD =
     &          SQRT(SIG_FRC_ENR_DEAD - FRC_ENR_DEAD**2)
C
                FRC_ENR_ISA = FRC_ENR_ISA/NB
                SIG_FRC_ENR_ISA = SIG_FRC_ENR_ISA/NB
c                SIG_FRC_ENR_ISA = SQRT(SIG_FRC_ENR_ISA - FRC_ENR_ISA**2)
C
                FRC_ENR_ICD = FRC_ENR_ICD/NB
                SIG_FRC_ENR_ICD = SIG_FRC_ENR_ICD/NB
                SIG_FRC_ENR_ICD = SQRT(SIG_FRC_ENR_ICD - FRC_ENR_ICD**2)
C
                FRC_ENR_MG1 = FRC_ENR_MG1/NB
                SIG_FRC_ENR_MG1 = SIG_FRC_ENR_MG1/NB
                SIG_FRC_ENR_MG1 = SQRT(SIG_FRC_ENR_MG1 - FRC_ENR_MG1**2)
C
                FRC_ENR_MG2 = FRC_ENR_MG2/NB
                SIG_FRC_ENR_MG2 = SIG_FRC_ENR_MG2/NB
                SIG_FRC_ENR_MG2 = SQRT(SIG_FRC_ENR_MG2 - FRC_ENR_MG2**2)
C
                FRC_DROP = FRC_DROP/NB

                IDNUM = 1000 + 100*I3  + I1D
                CALL HFILL(IDNUM,ETABIN,YDUM,FLOAT(NB))  ! TRACK NUMBER
C
                IDNUM = 2000 + 100*I3  + I1D
                CALL HFILL(IDNUM,ETABIN,YDUM,FRC_ENR_CAL)  ! FRAC CAL ENERGY
C
                IDNUM = 3000 + 100*I3  + I1D
                CALL HFILL(IDNUM,ETABIN,YDUM,SIG_FRC_ENR_CAL)  ! FRAC CAL ENERGY
C
                IDNUM = 4000 + 100*I3  + I1D
                CALL HFILL(IDNUM,ETABIN,YDUM,FRC_ENR_DEAD)  ! FRAC DEAD ENERGY
C
                IDNUM = 5000 + 100*I3  + I1D
                CALL HFILL(IDNUM,ETABIN,YDUM,SIG_FRC_ENR_DEAD)  ! FRAC DEAD ENERC
C
                IDNUM = 6000 + 100*I3  + I1D
                CALL HFILL(IDNUM,ETABIN,YDUM,FRC_ENR_ISA)  ! FRAC CAL ENERGY
C
                IDNUM = 7000 + 100*I3  + I1D
                CALL HFILL(IDNUM,ETABIN,YDUM,SIG_FRC_ENR_ISA)  ! FRAC CAL ENERGY
C
                IDNUM = 8000 + 100*I3  + I1D
                CALL HFILL(IDNUM,ETABIN,YDUM,FRC_ENR_ICD)  ! FRAC CAL ENERGY
C
                IDNUM = 9000 + 100*I3  + I1D
                CALL HFILL(IDNUM,ETABIN,YDUM,SIG_FRC_ENR_ICD)  ! FRAC CAL ENERGY
C
                IDNUM = 10000 + 100*I3  + I1D
                CALL HFILL(IDNUM,ETABIN,YDUM,FRC_ENR_MG1)  ! FRAC CAL ENERGY
C
                IDNUM = 11000 + 100*I3  + I1D
                CALL HFILL(IDNUM,ETABIN,YDUM,SIG_FRC_ENR_MG1)  ! FRAC CAL ENERGY
C
                IDNUM = 12000 + 100*I3  + I1D
                CALL HFILL(IDNUM,ETABIN,YDUM,FRC_ENR_MG2)  ! FRAC CAL ENERGY
C
                IDNUM = 13000 + 100*I3  + I1D
                CALL HFILL(IDNUM,ETABIN,YDUM,SIG_FRC_ENR_MG2)  ! FRAC CAL ENERGY
C
                IDNUM = 14000 + 100*I3  + I1D
                CALL HFILL(IDNUM,ETABIN,YDUM,FRC_DROP)! FRAC CAL ENERGY

              ENDIF
              ITRK=ITRK+NB
              CALL HFILL(1000,FLOAT(NB),0.,1.0)
  105       CONTINUE
  100     CONTINUE
  110   CONTINUE
  120 CONTINUE
      WRITE(SSUNIT(),*)' SHLEVT_SHLANL:Number of tracks analyzed ',
     &  ITRK,' number in library ',NCYCLES
c      CALL HFILL(10,CHECK_TOTAL/1000.0,0.,1.0)
c      CALL HFILL(20,CHECK_DEAD/1000.0,0.,1.0)
c      CALL HFILL(30,CHECK_DROP/1000.0,0.,1.0)
c      WRITE(15,*) ' CHECK_TOTAL/1000.0',CHECK_TOTAL/1000
c      WRITE(15,*) ' CHECK_DEAD/1000.0',CHECK_DEAD/1000
c      WRITE(15,*) ' CHECK_DROP/1000.0',CHECK_DROP/1000
         
  999 RETURN
      END

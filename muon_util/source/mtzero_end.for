       SUBROUTINE MTZERO_END
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates the correction to T0.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   4-OCT-1991   Cecilia E Gerber
C-   Updated  22-JAN-1992   C.G. Add gaussian fit.
C    DH 3/92 use standard summary for output
C    DH 3/92 make corrections to SMUO/MTIM bank
C    DH update first+third words
C    Updated 2/94 D.Wood Add ntuple and include cable delay tuning
C    Updated 3/94 R.Markeloff Compatible with IFAST>1 time-to-distance
C    MF 7/94 Changed name from COR_TZERO
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER NTDIM_MOD
      PARAMETER(NTDIM_MOD=16)
      REAL TRES, VEL, CTE, D_TZERO, HSUM, CONTENT
      REAL SIGX0, SIGMA, AV,SD,CC,CHI2,SIG(3),TCOARSE,CORRECT
      INTEGER MODNUM, MUNMOD3,NMODU,ID,I,DUM,MODULE,CALIB_T,NEV
      INTEGER LMTIM,GZMTIM,SSUNIT,UNIT,IA,ERR,N,NC,NT1,NT2,IER
      REAL AVET1,AVET2,AVESL1,AVESL2,A,F1,F2,F3,F4,HI
      INTEGER RUN,ITIME,JMAX,JBIN,J,LSUM,JSUM,JD
      CHARACTER*26 TIME
      CHARACTER*8 STIME,UPDATE
      REAL AVET1_OLD,AVET2_OLD,AVESL1_OLD,AVESL2_OLD
      REAL RESOL_OLD,RESOL_NEW,DELAY_OLD,DELAY_NEW
      REAL PNT(NTDIM_MOD)
      CHARACTER*6 CTAB(NTDIM_MOD)
      INTEGER JDATE,JTIME
      LOGICAL OK
C
      DATA CTAB/'MODULE','T1OLD ','T1NEW ','T2OLD ','T2NEW ',
     &          'SLOPE1','SLOPE2','DELOLD','DELNEW','RESOLD',
     &          'RESNEW','SIGX0 ','NHRAW ','NHCOR ','N3MISS',
     &          'NX0   '/
C==========================================================================
C     Calculates the correction to T0
C==========================================================================
      CALL EZGET('CALIB_T',CALIB_T,IER)
C  get time/date and convert to string and 64 bit VAX standard
      CALL MUJUNK(1,RUN,ITIME,JTIME,JDATE,F1,F2,F3,F4)
      CALL OFTSTR(ITIME,TIME)
      TRES = .37                    ! TRES (nsec/tdc counts)
      VEL = 0.005                   ! VEL (cm/nsec)
      CTE = TRES * VEL              ! CTE (cm/tdc counts)
      UNIT=SSUNIT()
      WRITE(UNIT,*) '****                             *****'
      WRITE(UNIT,*) '         COR_TZERO CALCULATIONS    '
      WRITE(UNIT,*) '*                                    *'
      WRITE(UNIT,*) ' RUN ',RUN, ' TIME ',TIME
      IF(CALIB_T.EQ.2) WRITE(UNIT,*) ' Do fine T0 adjustment to MTIM'
      IF(CALIB_T.EQ.3) WRITE(UNIT,*) ' Do fine T0 adjustment to MTIM'
      IF(CALIB_T.EQ.4) WRITE(UNIT,*) ' Do coarse T0 adjustment to MTIM'
      WRITE(UNIT,*) '*                                    *'
      WRITE(UNIT,*) '* use 3-miss plots for fine adjustment '
      WRITE(UNIT,*)
     A  'MODULE  ENTRIES RESOL_NEW COR_T0_SIGMA COR_T0  TCOARSE',
     A  '  AVET01  AVET02 UPDATE?'
C
      CALL HOUTPU(10)
      CALL HBOOKN(201,' Tzero calib ntuple',NTDIM_MOD,' ',1000,CTAB)
      CALL HBOOK1(20000,' Sigmas of 3-miss (drift) for all modules ',
     A 40,0.,1.,0.)
      NMODU = MUNMOD3(0,DUM)         ! number of modules
      DO  I = 1,NMODU               ! loop over modules
        CALL VZERO(PNT,NTDIM_MOD)
        UPDATE=' NO '
        MODNUM = MUNMOD3(1,I)
        ID = 20000 + MODNUM
        CONTENT = HSUM(ID)          ! see how many entries
        IF (CONTENT.GT.50.) THEN
CCC   LOOK AT RAW TIME HIST, FIND PEAK BIN
          JMAX=0
          JD=24000+MODNUM
          DO J=15,38
C            LSUM=HI(JD,J)+HI(JD,J-1)+HI(JD,J-2)+HI(JD,J-3)
C            JSUM=HI(JD,J+1)+HI(JD,J+2)+HI(JD,J+3)+HI(JD,J+4)
            LSUM=HI(JD,J)+HI(JD,J-1)+HI(JD,J-2)
            JSUM=HI(JD,J+1)+HI(JD,J+2)+HI(JD,J+3)
            IF(LSUM-JSUM.GT.JMAX) THEN
              JMAX=LSUM-JSUM
              JBIN=J
            ENDIF
          ENDDO
          TCOARSE=100.*JBIN+75.
C    look at x0 and 3 miss distributions
C
C    Units for Histo 26000+MODNUM are now TDC counts, not cm
C
          NEV=HSUM(26000+MODNUM)
          CALL HFITGA(26000+MODNUM,CC,AV,SD,CHI2,112,SIG)  ! T0
c          D_TZERO = AV/CTE        ! D_TZERO ( tdc counts)
          D_TZERO = AV             ! D_TZERO ( tdc counts)
          SIGX0=SD
          CALL HFITGA(25000+MODNUM,CC,AV,SD,CHI2,112,SIG)  ! 3 MISS
          SIGMA = SD                  ! Deviation's sigma (cm)
          CALL HFILL(20000,SIGMA,0.,1.)
          RESOL_NEW = SIGMA*10000.*SQRT(2./3.)  ! resolution in microns
          AVET1=0.
          AVET2=0.
          AVESL1=0.
          AVESL2=0.
C
C    update MTIM bank for 'good' modules (which isn't as yet defined)
C    do only if CALIB_T = 2,3,4. do fine for 2,3. do coarse for 4
C    probably want to impose some cuts here and have a printout
          IF(CALIB_T.EQ.4) THEN     ! do coarse corrections
            LMTIM=GZMTIM(MODNUM)
            IF(LMTIM.NE.0) THEN
              UPDATE=' YES '
C get old paramters
              AVET1_OLD = C(LMTIM+13)
              AVET2_OLD = C(LMTIM+14)
              AVESL1_OLD = C(LMTIM+15)
              AVESL2_OLD = C(LMTIM+16)
              DELAY_OLD = IC(LMTIM+12)
              RESOL_OLD = IC(LMTIM+3)
C update header words
              IC(LMTIM+1)=101              ! flag using tracks
              IC(LMTIM+3)=SIGMA*10000.*SQRT(2./3.)    ! quality
              IC(LMTIM+7)=JDATE        ! time/date
              IC(LMTIM+8)=JTIME        ! time/date
              NC=IC(LMTIM+10)/2   ! number of channels
              NT1=0
              NT2=0
              DO N=1,NC
                IF(C(LMTIM+12+8*N).GE.0.) THEN   ! TIME 1
                  AVET1=AVET1+C(LMTIM+9+8*N)
                  AVESL1=AVESL1+C(LMTIM+10+8*N)
                  NT1=NT1+1
                ENDIF
                IF(C(LMTIM+16+8*N).GE.0.) THEN   ! TIME 2
                  AVET2=AVET2+C(LMTIM+13+8*N)
                  AVESL2=AVESL2+C(LMTIM+14+8*N)
                  NT2=NT2+1
                ENDIF
              ENDDO
              IF(NT1.GT.0) THEN
                AVET1=AVET1/NT1
                AVESL1=AVESL1/NT1
              ENDIF
              IF(NT2.GT.0) THEN
                AVET2=AVET2/NT2
                AVESL2=AVESL2/NT2
              ELSE
                AVET2=AVET1
                AVESL2=AVESL1
              ENDIF
              CORRECT=AVET1-TCOARSE
              C(LMTIM+13)=TCOARSE               ! AVERAGE T01
              C(LMTIM+14)=AVET2-CORRECT         ! AVERAGE T02
              C(LMTIM+15)=AVESL1
              C(LMTIM+16)=AVESL2
C update cable delay using new tzero measurement
              DELAY_NEW = DELAY_OLD - CORRECT*AVESL1
              IC(LMTIM+12)=DELAY_NEW
              AVET1=TCOARSE
              AVET2=AVET2-CORRECT
              DO N=1,NC
                IF(C(LMTIM+12+8*N).GE.0.) THEN   ! TIME 1
                  C(LMTIM+9+8*N)=C(LMTIM+9+8*N)-CORRECT
                ENDIF
                IF(C(LMTIM+16+8*N).GE.0.) THEN   ! TIME 2
                  C(LMTIM+13+8*N)=C(LMTIM+13+8*N)-CORRECT
                ENDIF
              ENDDO
            ENDIF
          ENDIF
          IF(CALIB_T.EQ.2.OR.CALIB_T.EQ.3) THEN     ! do fine corrections
            LMTIM=GZMTIM(MODNUM)
            IF(LMTIM.NE.0.AND.NEV.GE.20) THEN
              UPDATE=' YES '
C get old paramters
              AVET1_OLD = C(LMTIM+13)
              AVET2_OLD = C(LMTIM+14)
              AVESL1_OLD = C(LMTIM+15)
              AVESL2_OLD = C(LMTIM+16)
              DELAY_OLD = IC(LMTIM+12)
              RESOL_OLD = IC(LMTIM+3)
C update header words
              IC(LMTIM+1)=101              ! flag using tracks
              IC(LMTIM+3)=SIGMA*10000.*SQRT(2./3.)    ! quality
              IC(LMTIM+7)=JDATE        ! time/date
              IC(LMTIM+8)=JTIME        ! time/date
              NC=IC(LMTIM+10)/2   ! number of channels
              AVET1=0.
              NT1=0
              AVET2=0.
              NT2=0
              AVESL1=0.
              AVESL2=0.
              DO N=1,NC
                IF(C(LMTIM+12+8*N).GE.0.) THEN   ! TIME 1
                  C(LMTIM+9+8*N)=C(LMTIM+9+8*N)+D_TZERO
                  AVET1=AVET1+C(LMTIM+9+8*N)
                  AVESL1=AVESL1+C(LMTIM+10+8*N)
                  NT1=NT1+1
                ENDIF
                IF(C(LMTIM+16+8*N).GE.0.) THEN   ! TIME 2
                  C(LMTIM+13+8*N)=C(LMTIM+13+8*N)+D_TZERO
                  AVET2=AVET2+C(LMTIM+13+8*N)
                  AVESL2=AVESL2+C(LMTIM+14+8*N)
                  NT2=NT2+1
                ENDIF
              ENDDO
              IF(NT1.GT.0) THEN
                AVET1=AVET1/NT1
                AVESL1=AVESL1/NT1
              ENDIF
              IF(NT2.GT.0) THEN
                AVET2=AVET2/NT2
                AVESL2=AVESL2/NT2
              ELSE
                AVET2=AVET1
                AVESL2=AVESL1
              ENDIF
              C(LMTIM+13)=AVET1               ! AVERAGE T01
              C(LMTIM+14)=AVET2               ! AVERAGE T02
              C(LMTIM+15)=AVESL1               ! AVERAGE SLT01
              C(LMTIM+16)=AVESL2               ! AVERAGE SLT02
C update cable delay using new tzero measurement
              DELAY_NEW = DELAY_OLD + D_TZERO*AVESL1_OLD
              IC(LMTIM+12)=DELAY_NEW
C   HISTOGRAM GOOD CHANNELS
C         IA=AVET1
C         A=IA-100.
C         CALL HBOOK1(28000+MODNUM,' T01 VALUES ',40,A,A+200.,0.)
C         IA=AVET2
C         A=IA-100.
C         CALL HBOOK1(28400+MODNUM,' T02 VALUES ',40,A,A+200.,0.)
C         A=AVESL1-.1
C         CALL HBOOK1(29000+MODNUM,' SLOPE T01 VALUES ',40,A,A+.2,0.)
C         A=AVESL2-.1
C         CALL HBOOK1(29400+MODNUM,' SLOPE T02 VALUES ',40,A,A+.2,0.)
C              DO N=1,NC
C                IF(C(LMTIM+12+8*N).GE.0.) THEN   ! TIME 1
C                  CALL HFILL(28000+MODNUM,C(LMTIM+ 9+8*N),0.,1.)
C                  CALL HFILL(29000+MODNUM,C(LMTIM+10+8*N),0.,1.)
C                ENDIF
C                IF(C(LMTIM+16+8*N).GE.0.) THEN   ! TIME 2
C                  CALL HFILL(28400+MODNUM,C(LMTIM+13+8*N),0.,1.)
C                  CALL HFILL(29400+MODNUM,C(LMTIM+14+8*N),0.,1.)
C                ENDIF
C              ENDDO
            ENDIF
          ENDIF
          WRITE(UNIT,800) MODNUM,NEV,RESOL_NEW,SIGX0,D_TZERO,TCOARSE,
     A      AVET1,AVET2,UPDATE
  800     FORMAT(I5,2X,I6,2X,F9.2,1X,F9.3,4X,F7.1,2X,F7.1,3X,
     &    2F7.0,2X,A8)
          PNT(2) = AVET1_OLD
          PNT(3) = AVET1
          PNT(4) = AVET2_OLD
          PNT(5) = AVET2
          PNT(6) = AVESL1
          PNT(7) = AVESL2
          PNT(8) = DELAY_OLD
          PNT(9) = DELAY_NEW
          PNT(10) = RESOL_OLD
          PNT(11) = RESOL_NEW
          PNT(12) = SIGX0
        ELSE
          WRITE(UNIT,*) MODNUM, 
     A    '  LESS THAN 50 ENTRIES ON TRACKS FOR THIS MODULE'
        ENDIF
C fill ntuple
        PNT(1) = MODNUM
        PNT(13) = HSUM(24000+MODNUM)  ! raw times
        PNT(14) = HSUM(23000+MODNUM)  ! corrected times
        PNT(15) = HSUM(25000+MODNUM)  ! 3-miss sets
        PNT(16) = HSUM(26000+MODNUM)  ! x0 sets
        CALL HFN(201,PNT)
      ENDDO
C
      CALL HOUTPU(UNIT)
  900 FORMAT(2X, E14.7)
  999 RETURN
      END

      SUBROUTINE XYZFIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find position and slope of the beam
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   5-OCT-1992   Alexandre Zinchenko
C-   Updated  16-DEC-1992   A. Zinchenko - initialize PARs from RCP, new
C-                          name and format of output ASCII file with results
C-   Updated  21-JAN-1993   A. Zinchenko - some cleaning, write beam posi-
C-                          tion into file for all runs
C-   Updated  08-MAR-1993   A. Zinchenko - add time, some service
C-   Updated  22-APR-1993   A. Zinchenko - new fit for beam position and
C-                          slope (fit of constrained histogram area)
C-   Updated  20-MAR-2004   sss - use idate2k instead of idate.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      COMMON /QUEST/IQUEST(100)
      INTEGER IQUEST
      INCLUDE 'D0$INC:XYVCOM.INC'
      EXTERNAL XYSFUN, XYGAUZ, XYVGA2
      DOUBLE PRECISION XYGAUZ
      REAL XYVGA2, DX, DZ, AIMIN, AIMAX, Z
      LOGICAL PROJ, FIRST, OK, EZERROR, VERTXZ, ZFIT, HEXIST, FEXAM
      INTEGER IER, NBXY, NBZ, NBZ2, IVZ, INTU, IXY, IPEAK, IPEAK1
      INTEGER LUN, LVMAX, IDH, ITER, NRUN, RUNNO, LUN1, J
      INTEGER NRUNO, LUNST, NSTORE, MONTH, JDATE, IYEAR, NOENT
      INTEGER IMIN, IMAX, NDIM, IX, IZ
      PARAMETER (NDIM = 10000)
      REAL XYSFUN, SIG(6), XMIN(4,2), XMAX(4,2), COV(21)
      REAL ST(6), PMI(6), PMA(6), ZLIM, C, AV, SD, CHI2, VMAX
      REAL PKMIN, BEAMXY(11), P(2), PARR(12), DIM(NDIM)
      EQUIVALENCE (PAR(7), P(1))
      CHARACTER TITLEX*40, TITLEY*40, FILNAM*24
      COMPLEX*8  XYVERT_DAT, XYVERT_TIM, DDATEB, DTIMEB
      CHARACTER*8  CDATE, CTIME, DATEB, TIMEB
      EQUIVALENCE (DDATEB, DATEB), (DTIMEB, TIMEB)
      DATA ST /10., 0.01, 0.01, 10., 0.01, 0.01/
      DATA PMI /0., -1., 0., 0., -1., 0./
      DATA PMA /10000., 1., 1., 10000., 1., 1./
      DATA PROJ /.FALSE./, FIRST /.TRUE./
      DATA TITLEX /'X coordinate, cm from '/
      DATA TITLEY /'Y coordinate, cm from '/
      DATA FILNAM /'USR$OUT:XYVERT_00000.DAT'/
      DATA NRUNO /-1/, CDATE /'  /  /  '/
C
      IF (FIRST) THEN
        CALL EZPICK('XYVERT_RCP')
        CALL EZGET('VERTEX_Z_EXIST',VERTXZ,IER)
        CALL EZGET('HIST_LIM_LOW',XMIN,IER)
        CALL EZGET('HIST_LIM_HIG',XMAX,IER)
        CALL EZGET('BIN_NUMB_XY',NBXY,IER)
        CALL EZGET('BIN_NUMB_Z',NBZ,IER)
        CALL EZGET('BIN_NUMB_Z2',NBZ2,IER)
        CALL EZGET('ZLIMIT',ZLIM,IER)
        CALL EZGET('XYPLANEO',XYBO,IER)
        CALL EZGET('PEAKMIN',PKMIN,IER)
        CALL EZGET('PARAMS',PAR,IER)
        CALL EZGET('EXAMINE',FEXAM,IER)
        IF (IER.NE.0) CALL ERRMSG(' Error reading RCP-file',
     &     'XYZFIT',' Exit','F')
        CALL EZRSET
        IF (NBXY*NBZ2.GT.NDIM) CALL ERRMSG('Small array size',
     &     'XYZFIT',' NDIM.LT.NBXY*NBZ2','F')
        IF (.NOT.FEXAM) FILNAM(1:8) = '        ' ! 8 spaces
        CALL UCOPY(XYBO,XYBN,2)
        CALL UCOPY(PAR,PARR,12)
        IVZ = 5 ! Zvert
        DZ = 2.*ZLIM/NBZ2
      ENDIF
      CALL TIME(CTIME)
      WRITE(*,*) ' Beg time:',CTIME
      NRUN = RUNNO() ! get run number
      WRITE(FILNAM(16:20),'(I5.5)') NRUN
      LUN = -1
      CALL GTUNIT(602,LUN1,IER)
      CALL D0OPEN(LUN1,'BEAM_POSITION','FA',OK) ! open output file
      IF (.NOT.OK) CALL D0OPEN(LUN1,'BEAM_POSITION','FO',OK)
C **** Read store number
      CALL GTUNIT(603,LUNST,IER)
      CALL D0OPEN(LUNST,'online:[coor_exec]current_accel_store.dat',
     &            'FI',OK) ! open file with store number
      NSTORE = 0
      IF (OK) THEN
        READ(LUNST,*) NSTORE
        CLOSE (UNIT=LUNST)
      ENDIF
      CALL RLUNIT(603,LUNST,IER)
C
      CALL HCDIR('//AAABBB',' ')
      CALL DHDIR('XYVERT_RCP','HBOOK_DIRECTORY',IER,' ')
      IF (FIRST) CALL HBOOK1(9,'Z-coordinate, cm',NBZ,-ZLIM,ZLIM)
C
C ****  Loop over ntuples
C
      DO 10 INTU = 1, 4
        IF (HEXIST(INTU)) THEN
          CALL HNOENT(INTU,NOENT)
          IF (NOENT.EQ.0) CALL ERRMSG('EMPTY NTUPLE','XYZFIT',
     &       'NO ENTRIES IN NTUPLE','W')
        ELSE
          CALL ERRMSG('NO NTUPLE','XYZFIT',
     &                'NTUPLE DOES NOT EXIST','W')
        ENDIF        
C
C ****  Find Z0 and SIGz - mean and sigma of interaction region in Z
C
        CALL VZERO(BEAMXY,11)
        ITER = 0
        ZFIT = .TRUE.
        IF (VERTXZ) THEN
          CALL HRESET(9,' ')
          CALL HPROJ1(9,INTU,-10,XYSFUN,1,1E+6,IVZ)
          CALL HUNPAK(9,DIM,' ',0)
          IF (VMAX(DIM,NBZ).LT.5.*PKMIN) THEN
            ZFIT = .FALSE.
            GO TO 13
          ENDIF
          CALL HFITGA(9,C,AV,SD,CHI2,100,SIG)
          IF (C.GT.10.*VMAX(DIM,NBZ)) THEN
            ZFIT = .FALSE.
            GO TO 13
          ENDIF
          PAR(1) = C
          PAR(2) = AV
          PAR(3) = SD
          PAR(4) = SD
          CALL HFITS(9,XYGAUZ,4,PAR,CHI2,102,SIG)
          ZMEAN = PAR(2)
          ZSIG1 = PAR(3)
          ZSIG2 = PAR(4)
        ENDIF
C
C **** Book histograms
C
   13   IF (INTU.LT.4) THEN
          TITLEX(23:28) = 'layer '
          WRITE(TITLEX(29:29),'(I1.1)') INTU-1
          TITLEY(23:28) = 'layer '
          WRITE(TITLEY(29:29),'(I1.1)') INTU-1
        ELSE
          TITLEX(23:29) = 'tracks '
          TITLEY(23:29) = 'tracks '
        ENDIF
        IF (FIRST) THEN
          CALL HBOOK1(INTU*10+1,TITLEX,NBXY,XMIN(INTU,1),
     &                XMAX(INTU,1))
          CALL HBOOK1(INTU*10+2,TITLEY,NBXY,XMIN(INTU,2),
     &                XMAX(INTU,2))
        ENDIF
        IF (VERTXZ) THEN
          CALL HBOOK2(51,'Z vs X in VTX',NBXY,
     &         XMIN(INTU,1),XMAX(INTU,1),NBZ2,-ZLIM,ZLIM,0.)
          CALL HBOOK2(52,'Z vs Y in VTX',NBXY,
     &         XMIN(INTU,2),XMAX(INTU,2),NBZ2,-ZLIM,ZLIM,0.)
        ENDIF
   11   DO 20 IXY = 2, 1, -1
          IDH = INTU*10 + IXY
          CALL HRESET(IDH,' ')
          IF (VERTXZ) CALL HRESET(50+IXY,' ')
C
C ****  Project Ntuple
C
          CALL HPROJ11(IDH,INTU,IXY,XYSFUN,1,1E+6,IXY)
          IF (VERTXZ.AND.ZFIT) CALL HPROJ22(50+IXY,INTU,
     &                         10+IXY,XYSFUN,1,1E+6,IXY,IVZ)
          PROJ = .FALSE.
C
C ****  Fit to Gaussian (with weights equal to 1)
C
          CALL HUNPAK(IDH,DIM,' ',0)
          IF (VMAX(DIM,NBXY).LT.PKMIN) GO TO 20 ! no fitting for histogram
                                                ! with low statistics
          PARR(1) = VMAX(DIM,NBXY)*2./3. ! peak value of first Gaussian
          PARR(2) = LVMAX(DIM,NBXY)*(XMAX(INTU,IXY)-XMIN(INTU,IXY))
     &               /NBXY + XMIN(INTU,IXY)  ! mean value
          PARR(4) = PARR(1)/2.   ! peak value of second Gaussian
          PARR(5) = PARR(2)*2./3. ! mean value
          IF (PARR(2).LE.0.) THEN
            PMI(2) = -1.
            PMA(2) = 0.
            ST(2) = -0.01
            PMI(5) = 1.2*PARR(2)
            PMA(5) = 0.
            ST(5) = 0.01
          ELSE
            PMI(2) = 0.
            PMA(2) = 1.
            ST(2) = 0.01
            PMI(5) = 0.
            PMA(5) = 1.2*PARR(2)
            ST(5) = -0.01
          ENDIF
          CALL XYHLIM(IDH,IMIN,IMAX)
          IQUEST(11) = IMIN
          IQUEST(12) = IMAX
          CALL HFITHN(IDH,'G','QRWB',3,PARR(1),
     &               ST,PMI,PMA,SIG,CHI2)
          IPEAK = 2
          IPEAK1 = 5
          IF (ABS(PARR(IPEAK)-XYBN(IXY)).GT.0.01) THEN
            XYBN(IXY) = PARR(IPEAK)
            IF (IXY.EQ.1) PROJ = .TRUE.
          ENDIF
          IF (LUN.LT.0) THEN
            CALL GTUNIT(601,LUN,IER)
            CALL D0OPEN(LUN,FILNAM,'FO',OK)
          ENDIF
          IF (IXY.EQ.1) THEN
            WRITE(LUN,*) TITLEX
          ELSE
            WRITE(LUN,*) TITLEY
          ENDIF
          WRITE(LUN,1001) INTU,PROJ,PARR(IPEAK-1),
     &                    PARR(IPEAK),PARR(IPEAK+1)
          BEAMXY(IXY*3-2) = PARR(IPEAK-1)
          BEAMXY(IXY*3-1) = PARR(IPEAK)
          IF (PARR(IPEAK-1).GT.1.E-3) BEAMXY(IXY*3) = 
     &        PARR(IPEAK+1)/SQRT(PARR(IPEAK-1))
C
C ****  Determine slope of beam: fit 2-dimentional histogram to
C ****  Gaussian with mean=mean(Z)
C
          IF (VERTXZ.AND.ZFIT) THEN
            P(1) = 0.0005
            P(2) = PARR(8)
            PAR(1) = PARR(1)
            PAR(2) = PARR(2)
            PAR(3) = PARR(3)
            IQUEST(11) = IMIN
            IQUEST(12) = IMAX
            CALL HFITH(50+IXY,XYVGA2,'QR',2,P(1),
     &                 ST,PMI,PMA,SIG,CHI2)
            DX = (XMAX(INTU,IXY)-XMIN(INTU,IXY))/NBXY
            CALL HUNPAK(50+IXY,DIM,' ',0)
            DO 100 IZ = 1, NBZ2
              Z = -ZLIM + DZ*(IZ-0.5)
              AIMIN = IMIN + P(1)*(Z-ZMEAN)/DX
              AIMAX = AIMIN - IMIN + IMAX
              DO 100 IX = 1, NBXY
                IF (IX.LT.INT(AIMIN-0.5).OR.IX.GT.INT(AIMAX+0.5))
     &             DIM(IX+(IZ-1)*NBXY) = 0.
  100       CONTINUE
            CALL HPAK(50+IXY,DIM)
            CALL HFITH(50+IXY,XYVGA2,'Q',2,P(1),
     &                 ST,PMI,PMA,SIG,CHI2)
            WRITE(LUN,1000) ZMEAN,ZSIG1,ZSIG2
            WRITE(LUN,1002) P(1)*1000., SIG(1)*1000.
            BEAMXY(7+(IXY-1)*2) = P(1)*1000.
            BEAMXY(8+(IXY-1)*2) = SIG(1)*1000.
            BEAMXY(11) = ZMEAN
          ENDIF
          WRITE(LUN,1003) 
   20   CONTINUE
        IF (PROJ) THEN
          ITER = ITER + 1
          IF (ITER.LE.3) THEN
            GO TO 11 ! reproject NTUPLE
          ELSE
            WRITE (LUN,*) ' Number of iterations exceeds maximum'
          ENDIF
        ENDIF
        IF (VERTXZ) THEN
          CALL HDELET(51)
          CALL HDELET(52)
        ENDIF
        IF (INTU.EQ.4.AND.
     &      .NOT.(NRUN.EQ.NRUNO.AND.BEAMXY(2).LT.1.E-3
     &      .AND.BEAMXY(5).LT.1.E-3)) THEN
          CALL IDATE2k(MONTH,JDATE,IYEAR)
          WRITE(CDATE(1:2),'(I2.2)') MONTH
          WRITE(CDATE(4:5),'(I2.2)') JDATE
          WRITE(CDATE(7:8),'(I2.2)') IYEAR
          CALL TIME(CTIME)
          DDATEB = XYVERT_DAT()
          DTIMEB = XYVERT_TIM()
          WRITE(LUN1,1005) NRUN, NSTORE, INTU-1, DATEB, TIMEB(1:5),
     &                     CDATE, CTIME(1:5)
          WRITE(LUN1,1015) (BEAMXY(J), J = 1,6)
          IF (VERTXZ.AND.ZFIT) THEN
            WRITE(LUN1,1006) BEAMXY(11), BEAMXY(7), BEAMXY(8)
            WRITE(LUN1,1007) BEAMXY(9), BEAMXY(10)
          ENDIF
        ENDIF
   10 CONTINUE
      WRITE(*,*) ' End time:',CTIME
      FIRST = .FALSE.
      NRUNO = NRUN
      IF (LUN.GT.0) THEN
        CLOSE (UNIT=LUN)
        CALL RLUNIT(601,LUN,IER)
      ENDIF
      CLOSE (UNIT=LUN1)
      CALL RLUNIT(602,LUN1,IER)
C----------------------------------------------------------------------
 1000 FORMAT(' Zmean, Sigma1, Sigma2: ',3E15.6)
 1001 FORMAT(I4,I4,'   Gaussian parameters: peak value = ',E15.6,/
     &       10X,'                      mean value = ',E15.6,/
     &       10X,'                      sigma      = ',E15.6)
 1002 FORMAT(10x,' Beam slope (mrad) = ',E15.6,'+-',E15.6)
 1003 FORMAT(1X,71(1H*))
 1005 FORMAT(' Run',I6,', Store',I5,', Lay',I1,', Beg. time ',A8,
     &       1X,A5,', End time ',A8,1X,A5)
 1015 FORMAT(' Ax= ',F6.1,', Xb=',E11.4,'+-',E10.4,', Ay= ',F6.1,
     &       ', Yb=',E11.4,'+-',E10.4)
 1006 FORMAT(' Z0 (cm)=',E11.4,', dX/dZ (mrad)=',E11.4,'+-',E10.4)
 1007 FORMAT(22X,'dY/dZ (mrad)=',E11.4,'+-',E10.4)
  999 RETURN
      END

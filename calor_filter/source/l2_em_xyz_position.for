      SUBROUTINE L2_EM_XYZ_POSITION(IETA,IPHI,ILYR,EM3,E_EM,
     &  DET_ETA,DET_PHI,XYZ_CLUS,SIG_ETA,SIG_PHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : return position of the hit for track matching
C-
C-   Inputs  : IETA,IPHI,ILYR  physics coordinates of peak EM3 cell
C-             EM3(-2:2,-2:2) 3x3 EM3 cells around the peak
C-             E_EM   total Energy of cluster in EM layers
C-   Outputs : DET_ETA detector-based eta of the hit
C-             DET_PHI detector-based phi of the hit
C-             XYZ_CLUS(3) x,y,z of cluster centroid (based on EM3)
C-             SIG_PHI  estimated error in phi
C-             SIG_ETA  estimated error in eta
C-   Controls: IETA controls how error calculated
C-
C-   Created  29-FEB-1992   James T. Linnemann
C-   Updated 22-JUL-1992   Sal Fahey - added fine positioning algorithm
C-                                      from CM3POS (N. Graf)
C-   Updated   1-AUG-1992   James T. Linnemann   use CELXYZ
C-   Updated  20-AUG-1992   Natalie Roe  Tune log weights
C-   Updated  22-AUG-1992   James T. Linnemann get weights from routine
C-   Updated   5-Sep-1992   do not calculate mean phi (branch point!)
C-   Updated   3-Nov-1993   Natalie Roe add radial offset for ECEM eta
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'       ! zeBrA mAin store /ZEBCOM/
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$INC:CL2_RINGS.INC'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF'
      INTEGER IETA,IPHI,ILYR,I,J,SIZE,EBIN,ETABIN
      PARAMETER(SIZE=2)
      REAL EM3(-SIZE:SIZE,-SIZE:SIZE)   !indices are ETA,PHI in EM3 space
      REAL DET_ETA,DET_PHI,CL2_SNTH,PHI_CELL,E_EM,SIG_ETA,SIG_PHI
      REAL ETCLUS,ECLUS,ETCELL,ECELL,PHI_CLUS,Z_CLUS,
     &  X(3),XCL_ETA(3),XCL_PHI(3),XYZ_CLUS(3)
      REAL WT,SUMWT_PHI,SUMWT_ETA,R,DET_THETA,RATIO_LOG
      REAL SIG_ETA_CM,SIG_PHI_CM,DIST
      REAL W0_PHI,W0_ETA    ! W0 for phi, eta directions
      REAL    ECEM_OFFSET_THETA, ECEM_OFFSET_LN_E0
      REAL    TAN_TH, DEL_TANTH
      INTEGER SIZE_POSITION ! how much of EM3 array to use for position calc
      INTEGER GZCAEP,L2CAEP,ETA,PHI,LYR,NR,IER
      INTEGER ETALO,ETAHI,PHILO(2),PHIHI(2),NPHI,IBOUND,IPOINT
      INTEGER ETA3,PHI3,IE3,IP3,JE3,JP3   !coordinates and offsets in EM3 space
      DATA ECEM_OFFSET_THETA/ 4.28E-4 /
      DATA ECEM_OFFSET_LN_E0/ -11.064 /
C
C For error message routine
C
      CHARACTER*80 NOWERRMESS           ! Error message
      CHARACTER*16 NOWSMESS             ! short description
C
C----------------------------------------------------------------------
      CALL L2_EM_LOGW_CONSTS(E_EM,IETA,W0_PHI,W0_ETA,SIZE_POSITION,
     &    SIG_ETA_CM,SIG_PHI_CM)
C...the relative coordinate array has already been made
      ETCLUS = 0.
      DO I = -SIZE_POSITION,SIZE_POSITION
        DO J = -SIZE_POSITION,SIZE_POSITION
          IF( EM3(I,J).GT.0) ETCLUS = ETCLUS + EM3(I,J) !Array is Et(Zv=0) NOT E
        ENDDO
      ENDDO
C
      ECLUS = ETCLUS
C...this line may be overkill
      ECLUS = ETCLUS/CL2_SNTH(IETA,IPHI,ILYR,0.0)  !BACK TO E
C
C
C...now loop over neighors in absolute coordinates so can use CELXYZ
C
C...it was judged easier to rebuild this loop than correctly calculate absolute
C   coordinate from relative coordinates.  Time is not a big issue.
C
C...get boundaries of a ring around the candidate for calculating floor sums
C...  ring is size = 1 i.e. 3x3 Readout Towers.  Unpacking guaranteed in EM3_MAX
      CALL CL2_RING22(IETA,IPHI,1,ETALO,ETAHI,PHILO,PHIHI,NPHI)
C
C...calculate coordinates in EM3 space 5x5 (-2:2) around peak cell
      ETA3 = 2*L2_JETA(IETA) + DETA3(ILYR) !eta in EM3 space of peak
      PHI3 = 2*IPHI  + DPHI3(ILYR)
      L2CAEP = GZCAEP()
      NR = IQ(L2CAEP+2)
      SUMWT_PHI=0
      SUMWT_ETA=0
C
C...calculating a mean 3-vector takes alignment into account safely
      DO I = 1,3
        XCL_PHI(I) = 0
        XCL_ETA(I) = 0
      ENDDO

      DO ETA = ETALO,ETAHI
        IE3 = 2*L2_JETA(ETA) - ETA3 !relative coord in em3 space before em3 corr
        DO IBOUND = 1,NPHI
          DO PHI = PHILO(IBOUND),PHIHI(IBOUND)
            IP3 = 2*PHI - PHI3      !relative coord in em3 space before em3 corr
            DO LYR = LYEM3A,LYEM3D
              IPOINT = (PTCAEP2(LYR,PHI,ETA)-1)*NR
              IF (IPOINT.GE.0) THEN
                ETCELL = Q(L2CAEP+IPOINT+5)
                IF (ETCELL.GT.0) THEN
                  JE3 = IE3 + DETA3(LYR)  !relative coordinate in EM3
                  IF (IABS(JE3).LE.SIZE_POSITION) THEN
                    JP3 = L3_DPHI(IP3 + DPHI3(LYR)) !relative coord in EM3
                    IF (IABS(JP3).LE.SIZE_POSITION) THEN
C
C...this next may be overkill; it recalculates ratios of E instead of ET
C                      ECELL = ETCELL/CL2_SNTH(ETA,PHI,LYR,0.0)  !BACK TO E
                      CALL CELXYZ(ETA,PHI,LYR,X(1),X(2),X(3),IER)
                      ECELL =ETCELL*SQRT(1.0+X(3)**2/(X(1)**2+X(2)**2))
                      RATIO_LOG = LOG(ECELL/ECLUS)
C
C...use different weighting for phi, z
C
                      WT = W0_PHI + RATIO_LOG
                      IF (WT.GT.0) THEN
                        DO I = 1,3
                          XCL_PHI(I) = XCL_PHI(I) + WT*X(I)
                        ENDDO
                        SUMWT_PHI = SUMWT_PHI + WT
                      ENDIF
                      WT = W0_ETA  + RATIO_LOG
                      IF (WT.GT.0) THEN
                        DO I = 1,3
                          XCL_ETA(I) = XCL_ETA(I) + WT*X(I)
                        ENDDO
                        SUMWT_ETA = SUMWT_ETA + WT
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      IF (SUMWT_PHI.LE.0) THEN
        CALL CELXYZ(IETA,IPHI,ILYR,XCL_PHI(1),XCL_PHI(2),XCL_PHI(3),IER)
      ELSE
        DO I = 1,3
          XCL_PHI(I) = XCL_PHI(I)/SUMWT_PHI
        ENDDO
      ENDIF
      IF (SUMWT_ETA.LE.0) THEN
        CALL CELXYZ(IETA,IPHI,ILYR,XCL_ETA(1),XCL_ETA(2),XCL_ETA(3),IER)
      ELSE
        DO I = 1,3
          XCL_ETA(I) = XCL_ETA(I)/SUMWT_ETA
        ENDDO
      ENDIF
      PHI_CLUS = ATAN2(XCL_PHI(2),XCL_PHI(1))
      IF (PHI_CLUS.LE.0) PHI_CLUS = PHI_CLUS + TWOPI
      R = SQRT(XCL_ETA(1)**2 + XCL_ETA(2)**2)
      Z_CLUS = XCL_ETA(3)
      XYZ_CLUS(1)=R*COS(PHI_CLUS)
      XYZ_CLUS(2)=R*SIN(PHI_CLUS)
      XYZ_CLUS(3)=Z_CLUS
      DET_PHI = PHI_CLUS
      DET_THETA = ATAN2(R,Z_CLUS)
      DET_ETA = -LOG(TAN(DET_THETA/2.))
      SIG_PHI = SIG_PHI_CM/R
      DIST = SQRT(R**2 + XYZ_CLUS(3)**2)
      SIG_ETA = SIG_ETA_CM/DIST           !CC
c correct for radial offset in ECEM
      IF (ABS(IETA).GE.14) THEN
        SIG_ETA = ABS(SIG_ETA*XYZ_CLUS(3)/R)   !EC
        TAN_TH    = TAN(DET_THETA)
c correct for offset IF ECLUS not zero
        IF (ECLUS.GT.0)  THEN
          DEL_TANTH = TAN_TH * ECEM_OFFSET_THETA
     &                * ( LOG(ECLUS) - ECEM_OFFSET_LN_E0 )
          TAN_TH    = TAN_TH - DEL_TANTH
        ENDIF
        DET_THETA = ATAN(TAN_TH)
        IF ( TAN_TH.LT.0. )  DET_THETA = DET_THETA + PI
        DET_ETA = -LOG(TAN(DET_THETA/2.))
      ENDIF
  999 RETURN
      END

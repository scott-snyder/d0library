      SUBROUTINE L2_CDC_SECTORS(APHI,DPHI) 
C--------------------------------------------------------------------
C  Find sectors of CDC chamber lying within a road 
C               
C  Input:  (retrievable from L2EM bank for each EM candidate in event
C
C        APHI = Phi position of EM shower center
C        DPHI = Width of PHI road (in radians)
C
C        Needs a MC_FLAG passed or set somehow if run OFFLINE on MC
C  
C  Output: passed via Common Block SECTLST (see D0$INC:SECTLIST.INC)
C
C        NROAD           Number of cells (layer,sector) within road
C        LAYLST(1:NROAD) Layer  position of nth hit cell
C        SECLST(1:NROAD) Sector position of nth hit cell
C        NUMCRT          Number of crates serving these hit cells
C        CRTLST          List of crate identifiers (0,1,2...5)
C
C  07-OCT-1993   Dan Claes      A stripped version of L2_FNDSEC.FOR
C--------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'                             
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$INC:PI.DEF/LIST'                             
      INCLUDE 'D0$INC:SECTLIST.INC/LIST'
C--------------------------------------------------------------------
      INTEGER IDCRATE,  I,  IP,  L_TRY,  LAYER
      INTEGER N_MAX,  N_MIN,  SECTOR,  ILYR,  ISCTR
      REAL    APHI,  CENT,  DPHI,  MAX_PHI(2),  MIN_PHI(2)
      REAL    OFFSET, PHIHI, PHILO, PHIMAX, PHIMIN, WIDSEC
C
      LOGICAL MC_FLAG         ! If ever run on MC studies will
C                             ! need a mechanism for passing
      DATA MC_FLAG /.FALSE./  ! or setting this flag.
C----------------------------------------------------------------------
C The following (360/64=5.625 also retrievable from DRFT bank,
C but obviously stable enough to hardcode.
      WIDSEC = RADIAN * 360/64  ! Cell half width
      CENT = WIDSEC             ! Cell center, Layer 0, Sector 0
C
      NROAD = 0
      DO I=0,5
        CRTLST(I)=.FALSE.
      ENDDO
C
      PHIMIN = APHI - DPHI
      PHIMAX = APHI + DPHI
C
      DO 100 LAYER = 3,0,-1              ! Layers 0,2 and 1,3 same staggering
C
        IF (MC_FLAG) THEN
C
C Monte Carlo (GEANT has the design orientation of the CDC - with the cell
C              wall between Sector 0,31 of layer 0,2 at PHI=0 and the sense
C              wire plane of Sector  31 of layer 1,3 at PHI=0)
C
          OFFSET = MOD(LAYER,2)*WIDSEC
          PHILO = PHIMIN - OFFSET       ! Correct for half cell (design)
          PHIHI = PHIMAX - OFFSET       ! staggering of layers 0  and  2
        ELSE
C
C Data (e.g. COSMIC run) Chamber mounted with half-cell rotation
C
          OFFSET = MOD(LAYER+1,2)*WIDSEC
          PHILO = PHIMIN + OFFSET       ! Correct for negative half cell
          PHIHI = PHIMAX + OFFSET       ! staggering of layers 0  and  2
        ENDIF
C
C  the input (Calorimeter or muon) PHI has been defined between 0 - TWOPI
C  so PHIMIN could be negative or PHIMAX > twopi
C
        MIN_PHI(1) = PHILO
        MAX_PHI(1) = PHIHI
        L_try = 1
        IF (PHIHI .GT. TWOPI) THEN    
C
          IF (PHILO.LT.TWOPI) THEN    ! Break PHI range into two pieces
            MAX_PHI(2) = PHIHI - TWOPI
            MIN_PHI(2) = 0.
            MAX_PHI(1) = TWOPI
            L_try=2                   ! The staggering correction can
          ELSE                        ! make both PHIHI,PHILO > TWOPI
            MAX_PHI(1) = PHIHI - TWOPI
            MIN_PHI(1) = PHILO - TWOPI
          ENDIF
        ELSEIF (PHILO .LT. 0) THEN    
            MIN_PHI(1) = PHILO + TWOPI  ! Staggering correction can
            IF (PHIHI .LT. 0) THEN      ! make both PHIHI,PHILO < 0
              MAX_PHI(1) = PHIHI + TWOPI
            ELSE
              MAX_PHI(1) = TWOPI      ! Break PHI range into two pieces
              MAX_PHI(2) = PHIHI
              MIN_PHI(2) = 0
              L_try = 2
            ENDIF
        ENDIF
        DO IP = 1, L_try
          N_min = ((MIN_PHI(IP) - CENT)/WIDSEC-1)/2. + 1
          N_max = ((MAX_PHI(IP) - CENT)/WIDSEC + 1)/2.
          IF (N_min.EQ.-1) N_min = 0
          IF (N_max.EQ.32) N_max = 31
          DO SECTOR = N_min, N_max
            NROAD = NROAD + 1                 
            LAYLST(NROAD) = LAYER           ! Assumes we are unpacking
            SECLST(NROAD) = SECTOR          ! all four layers  0,1,2,3
            IDCRATE = IAND(SECTOR,1)
            IF (SECTOR.GT.8 .AND. SECTOR.LT.25) IDCRATE = 3 - IDCRATE
C
C   Patch for RECDD2 unpacking which applies (Cochran's) mixed up crate map
C
            IF (MC_FLAG) THEN
              IF (IDCRATE.EQ.0) THEN
                IDCRATE = 1
              ELSEIF (IDCRATE.EQ.1) THEN
                IDCRATE = 0
              ENDIF
            ENDIF
C
            CRTLST(IDCRATE) = .TRUE.
C
C Delay lines need to be unpacked and mapped out as well
C
            ILYR = IAND(LAYER,1)  ! Check even/odd layer
            ISCTR = ILYR + SECTOR
            IF (ISCTR.GT.7 .AND. ISCTR.LT.24) THEN
              IDCRATE = 4
            ELSE
              IDCRATE = 5
            ENDIF
            CRTLST(IDCRATE) = .TRUE.
          ENDDO
        ENDDO
C
  100 CONTINUE
C
  999 RETURN  
      END        

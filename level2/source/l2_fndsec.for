      SUBROUTINE L2_FNDSEC(ROAD) 
C--------------------------------------------------------------------
C  Find sectors in CDC chamber along a road 
C               
C  Input:  
C        PHIMIN,PHIMAX = road parameters
C  
C  Output: passed via Common Block SECTLST
C
C        NROAD           Number of cells (layer,sector) within road
C        LAYLST(1:NROAD) Layer  position of nth hit cell
C        SECLST(1:NROAD) Sector position of nth hit cell
C        NUMCRT          Number of crates serving these hit cells
C        CRTLST          List of crate identifiers (0,1,2...5)
C
C  Daria Zieminska Feb. 1989          Created as FLDSEC.FOR
C    Updates:
C-   1. 08-APR-1990   Qizhong Li-Demarteau  rewritten to find correct sectors
C-                                               
C    2. 13-AUG-1991   Dan Claes  Abbreviate code.  Eliminate loops over all
C                                                  Layers, Sectors
C                                                  Pass information via lists
C                                                  rather than ON(0:3,0:31)
C                                                  Include CRATE list to unpk
C    3. 27-SEP-1991   Dan Claes  Temporary patch for COSMIC beam counter width
C
C    4. 03-NOV-1993   Dan Claes  Patch logic dealing with PHI values near TWOPI
C                                and modify the ranking at the end for PHOTON
C                                group which will be using HUGE roads
C--------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'                             
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$INC:PI.DEF/LIST'                             
      INCLUDE 'D0$INC:L2TRAK.INC/LIST'
      INCLUDE 'D0$INC:SECTLIST.INC/LIST'
C--------------------------------------------------------------------
      INTEGER IDCRATE, I, IP, L_TRY, LAYER
      INTEGER N_MAX, N_MIN, SECTOR, ILYR, ISCTR
      REAL ROAD(1:6,0:6,0:3)
      REAL    CEN_MAX, CEN_MIN, MAX_PHI(2), MIN_PHI(2), OFFSET
      REAL    PHIHI, PHILO, PHIMAX, PHIMIN, PHI0, TEMP
C----------------------------------------------------------------------
C
      NROAD = 0
      DO I=0,5
        CRTLST(I)=.FALSE.
      ENDDO
C
      DO 100 LAYER = 3,0,-1              ! Layers 0,2 and 1,3 same staggering
        IF(ROAD(1,0,LAYER).LT.ROAD(1,6,LAYER))THEN
          PHIMIN=ROAD(1,0,LAYER)
        ELSE
          PHIMIN=ROAD(1,6,LAYER)
        ENDIF
        IF(ROAD(2,0,LAYER).GT.ROAD(2,6,LAYER))THEN
          PHIMAX=ROAD(2,0,LAYER)
        ELSE
          PHIMAX=ROAD(2,6,LAYER)
        ENDIF
C
C  PHIMIN and PHIMAX are now determined by L2_ROAD calculations
C  that already correct 0<PHI<TWOPI.  The logical manipulations below
C  work most easily if the ZERO/TWOPI cut is straddled continuously.
C
        IF (PHIMIN.GT.PHIMAX) PHIMAX = PHIMAX + TWOPI
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
        PHI0  = (PHIHI + PHILO)/2     ! Nominal center of road
C
C  the input PHIMIN and PHIMAX (from Calorimeter or muon) have been defined as:
C       PHIMIN = phi - delta_phi
C       PHIMAX = phi + delta_phi
C  With phi between 0 - twopi, PHIMIN could be negative and PHIMAX > twopi
C
C  PHIMIN and PHIMAX are now determined by L2_ROAD calculations
C  that already correct 0<PHI<TWOPI.  This is undone above when neccessary
C  for expediancy.
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
          N_min = ((MIN_PHI(IP) - ICEN)/WIDSEC-1)/2. + 1
          N_max = ((MAX_PHI(IP) - ICEN)/WIDSEC + 1)/2.
          IF (N_min.EQ.-1) N_min = 0
          IF (N_max.EQ.32) N_max = 31
          DO SECTOR = N_min, N_max
            NROAD = NROAD + 1                 
            LAYLST(NROAD) = LAYER           ! Assumes we are unpacking
            SECLST(NROAD) = SECTOR          ! all four layers  0,1,2,3
            IDCRATE = IAND(SECTOR,1)
            IF (SECTOR.GT.8 .AND. SECTOR.LT.25) IDCRATE = 3 - IDCRATE
C
C Patch for Chris' RECDD2 unpacking which applied Cochran's mixed up crate IDs
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
C Delay lines need to be unpacked and mapped out as well
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
C Rank SECTORS by most likely to carry hit-list SECTOR intercptd by PHI0 first
C
        IF (L_try.EQ.1) THEN
          IF (N_max.NE.N_min) THEN          ! Two sectors were found in road
            CEN_MAX = ICEN + 2*N_max*WIDSEC
            CEN_MIN = ICEN + 2*N_min*WIDSEC
            IF (ABS(CEN_MAX-PHI0).LT.ABS(CEN_MIN-PHI0)) THEN
              TEMP = SECLST(NROAD-1)
              SECLST(NROAD-1) = SECLST(NROAD)
              SECLST(NROAD) = TEMP
            ENDIF
          ENDIF
C        ELSE                                                 ! Guaranteeing
C          IF (LAYLST(NROAD-1).EQ.LAYER) THEN                 ! two sectors
C            IF (PHI0.LT.0 .OR. (PHI0 .GT. 2*WIDSEC) ) THEN   ! found in layer
C              SECLST(NROAD-1) = 31
C              SECLST(NROAD)   = 0
C            ELSE
C              SECLST(NROAD-1) = 0
C              SECLST(NROAD)   = 31
C            ENDIF
C          ENDIF
        ENDIF
  100 CONTINUE
C
  999 RETURN  
      END        

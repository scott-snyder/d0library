      SUBROUTINE FLVSEC(PHIMIN,PHIMAX,ON) 
C--------------------------------------------------------------------
C                        
C  Find sectors in VTX chamber along a road 
C               
C  Input:  
C        PHIMIN,PHIMAX = road parameters
C  
C  Output: 
C        ON(0:2,0:31)                 = true for sectors on the road    
C
C  Daria Zieminska Feb. 1989                
C   Updated 14-MAY-1990 P. Grudberg - handle branch cut
C-   Updated  10-MAR-1992   Qizhong Li-Demarteau  speed up for full_tracking 
C                  
C--------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'                             
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'                             
      INCLUDE 'D0$INC:PI.DEF'
C
      REAL PHIMIN, PHIMAX, PHI1, PHI2, LOLIM, HILIM
      REAL PHISEC, PHILO, PHIHI, PHISC0, WIDSEC
      REAL    PRECIS
      PARAMETER( PRECIS = 0.000001 )
      INTEGER LAYER, SECTOR, NSEC(0:2), ICALL
      INTEGER IER, LVRFT, GZVRFT, CASE
      LOGICAL ON(0:2,0:31)
      DATA ICALL / 0 /
C-----------------------------------------------------------------------
C
C  no need to search for sectors for the full_tracking case
C
      IF (ABS(PHIMIN - 0.0) .LE. PRECIS .AND. 
     &    ABS(PHIMAX - TWOPI) .LE. PRECIS) THEN
        CALL VFILL(ON,96,-1)
        GOTO 999
      ENDIF
C
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('NSEC',NSEC,IER)
        CALL EZRSET
        ICALL=1
      END IF
C
      CALL VZERO(ON,96)
      DO LAYER = 0, 2
C
C ****  Sector 0 for each layer is defined as the sector through which PHI = 0
C ****  passes.  In this program the branch cut must be set someplace, which
C ****  means that for sector 0, I can either have the lower phi limit for the
C ****  sector < 0 or the upper phi limit > 2pi.  I choose to have all the
C ****  sector phi limits positive, so I will add 2 pi to the limits obtained
C ****  for sector 0 of each layer.  To deal with the branch cut, I will
C ****  calculate for each layer the limits of the branch used.
C
        LVRFT = GZVRFT()
        PHISC0 = C(LVRFT + 8 + 7*LAYER) * RADIAN
        WIDSEC = C(LVRFT + 6 + 7*LAYER) * RADIAN        ! half cell width
        LOLIM = PHISC0 + WIDSEC
        HILIM = LOLIM + TWOPI
        PHI1 = 0.
        PHI2 = 0.
        DO SECTOR = 0, NSEC(LAYER)
          PHISEC = PHISC0 + SECTOR * 2. * WIDSEC
          IF ( SECTOR .EQ. 0) PHISEC = PHISEC + TWOPI
          PHILO = PHISEC - WIDSEC
          PHIHI = PHISEC + WIDSEC
C
C ****  PHIMIN, PHIMAX are defined as follows:
C               PHIMIN = PHICAL - DELTA_PHI_CAL
C               PHIMAX = PHICAL + DELTA_PHI_CAL
C       where PHICAL is between 0 and two pi.  It is possible that either
C       PHIMIN < 0. or that PHIMAX > twopi.
C
C ****  There are 4 possibilities:
C               1: PHIMIN, PHIMAX < LOLIM
C               2: PHIMIN < LOLIM, LOLIM < PHIMAX < HILIM
C               3: LOLIM < PHIMIN, PHIMAX < HILIM
C               4: LOLIM < PHIMIN < HILIM, HILIM < PHIMAX
C
          IF ( PHI1 .EQ. 0. .AND. PHI2 .EQ. 0. ) THEN
C
C ****  Case 1: just add twopi to both calorimeter road limits
C
            IF ( PHIMIN .LT. LOLIM .AND. PHIMAX .LE. LOLIM ) THEN
              CASE = 1
              PHI1 = PHIMIN + TWOPI
              PHI2 = PHIMAX + TWOPI
            ENDIF
C
C ****  Case 2: only add twopi to the lower limit (here I assume that the
C ****  calorimeter road is <= twopi)
C
            IF ( PHIMIN .LT. LOLIM .AND. PHIMAX .GT. LOLIM ) THEN
              CASE = 2
              PHI1 = PHIMIN + TWOPI
              PHI2 = PHIMAX
            ENDIF
C
C ****  Case 3: no change of limits needed
C
            IF ( PHIMIN .GE. LOLIM .AND. PHIMAX .LE. HILIM ) THEN
              CASE = 3
              PHI1 = PHIMIN
              PHI2 = PHIMAX
            ENDIF
C
C ****  Case 4: subtract twopi from PHIMAX
C
            IF ( PHIMAX .GT. HILIM ) THEN
              CASE = 4
              PHI1 = PHIMIN
              PHI2 = PHIMAX - TWOPI
            ENDIF
          ENDIF
C
          IF ( CASE .EQ. 1 .OR. CASE .EQ. 3 ) THEN
            IF ( PHI1 .GE. PHILO .AND. PHI1 .LT. PHIHI ) THEN
              ON(LAYER, SECTOR) = .TRUE.
            ELSEIF ( PHI2 .GT. PHILO .AND. PHI2 .LE. PHIHI ) THEN
              ON(LAYER, SECTOR) = .TRUE.
            ELSEIF ( PHI1 .LT. PHILO .AND. PHI2 .GT. PHIHI ) THEN
              ON(LAYER, SECTOR) = .TRUE.
            ENDIF
          ELSEIF ( CASE .EQ. 2 .OR. CASE .EQ. 4 ) THEN
            IF ( PHI2 .GT. PHILO ) THEN
              ON(LAYER, SECTOR) = .TRUE.
            ELSEIF ( PHI1 .LT. PHIHI ) THEN
              ON(LAYER, SECTOR) = .TRUE.
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
  999 RETURN  
      END        

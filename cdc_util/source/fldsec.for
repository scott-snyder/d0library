      SUBROUTINE FLDSEC(PHIMIN,PHIMAX,ON) 
C--------------------------------------------------------------------
C                        
C  Find sectors in CDC chamber along a road 
C               
C  Input:  
C        PHIMIN,PHIMAX = road parameters
C  
C  Output: 
C        ON(0:3,0:31)                 = true for sectors on the road    
C
C  Daria Zieminska Feb. 1989                
C-   Updated   8-APR-1990   Qizhong Li-Demarteau  rewritten to find
C-                                                the correct sectors 
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  10-MAR-1992   Qizhong Li-Demarteau  speed up for full_tracking 
C-   Updated  16-JAN-1993   Qizhong Li-Demarteau  changed PHISEC definition
C-   Updated   5-DEC-1995   Srini Rajagopalan  Fix phi wrap around 
C                  
C--------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'                             
      INCLUDE 'D0$INC:PI.DEF/LIST'                             
      INTEGER LAYER, SECTOR, MAXSEC, LDALS, IP, LENGTH, LDRFT, ERR
      INTEGER GZDALS, GZDRFT
      INTEGER PLWIR2, PLWIR3
      INTEGER IER
      REAL PHIMIN, PHIMAX 
      REAL PHISEC, PHILO, PHIHI, WIDSEC , PHI2, PHIMA2, PHIMI1
      REAL    PHIWR2, PHIWR3
      REAL    PRECIS
      PARAMETER( PRECIS = 0.000001 )
      LOGICAL ON(0:3,0:31), FIRST
      LOGICAL EZERROR
      SAVE FIRST
      DATA  FIRST/.TRUE./
C--------------------------------------------------------------------
C
C  no need to search for sectors for the full_tracking case
C
      IF (ABS(PHIMIN - 0.0) .LE. PRECIS .AND. 
     &    ABS(PHIMAX - TWOPI) .LE. PRECIS) THEN
        CALL VFILL(ON,128,-1)
        GOTO 999
      ENDIF
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','FLDSEC',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('MAXSEC',MAXSEC,ERR)
        IF (ERR .NE. 0) MAXSEC = 31
        CALL EZRSET
      ENDIF
C
      CALL VZERO(ON,128)
      PHI2 = 0.0
      DO 100 LAYER = 0, 1 
        DO 200 SECTOR = 0, MAXSEC 
          LDRFT = GZDRFT()
C          PHISEC = C(LDRFT+12+2*LAYER) * PI/180. + (SECTOR * PI/16.)
          WIDSEC = C(LDRFT + 9) * PI / 180.
          LDALS = GZDALS(LAYER,SECTOR)
          IF (LDALS .LE. 0) GOTO 200
          PLWIR2 = LDALS + 6 + 2 * IC(LDALS+6)
          PLWIR3 = LDALS + 6 + 3 * IC(LDALS+6)
          PHIWR2 = ATAN2(C(PLWIR2+2),C(PLWIR2+1))
          IF (PHIWR2 .LE. 0) PHIWR2 = PHIWR2 + TWOPI
          PHIWR3 = ATAN2(C(PLWIR3+2),C(PLWIR3+1))
          IF (PHIWR3 .LE. 0) PHIWR3 = PHIWR3 + TWOPI
          PHISEC = (PHIWR2 + PHIWR3) / 2
          PHILO = PHISEC - WIDSEC 
          PHIHI = PHISEC + WIDSEC 
C
C   sector 31 is staggered with sector 0
C
          PHIMA2 = PHIMAX
          PHIMI1 = PHIMIN
          IF (LAYER .EQ. 1 .AND. SECTOR .EQ. 31) THEN
            PHIMI1 = PHIMI1 - WIDSEC
            PHIMA2 = PHIMA2 - WIDSEC
            PHILO = PHILO - WIDSEC
            PHIHI = PHIHI - WIDSEC
          ENDIF
C
C  PHIMIN could be negative, because in Calorimeter or muon chamber 
C  the PHIMIN and PHIMAX are defined as:
C    PHIMIN = phi - delta_phi
C    PHIMAX = phi + delta_phi
C  The phi is between 0 to twopi. So PHIMIN could be negative 
C                                 or PHIMAX could be bigger than twopi
C
          IF (PHIMI1 .GE. 0.0) THEN
            IF (PHIMAX .GT. TWOPI) THEN
              PHIMA2 = PHIMAX - TWOPI
              IF (PHIMA2 .GT. PHILO) THEN
                ON(LAYER,SECTOR) = .TRUE.
                ON(LAYER+2,SECTOR) = .TRUE.
              ENDIF
              IF (PHIMI1 .LE. PHIHI) THEN
                ON(LAYER,SECTOR) = .TRUE.
                ON(LAYER+2,SECTOR) = .TRUE.
              ENDIF
            ELSE 
              IF (PHIMI1.GE.PHILO .AND. PHIMI1.LT.PHIHI) THEN 
                ON(LAYER,SECTOR) = .TRUE.
                ON(LAYER+2,SECTOR) = .TRUE.
              ELSE IF (PHIMA2.GT.PHILO .AND. PHIMA2.LE.PHIHI) THEN 
                ON(LAYER,SECTOR) = .TRUE.
                ON(LAYER+2,SECTOR) = .TRUE.
              ELSE IF (PHIMI1.LT.PHILO .AND. PHIMA2.GT.PHIHI) THEN 
                ON(LAYER,SECTOR) = .TRUE.
                ON(LAYER+2,SECTOR) = .TRUE.
              ENDIF
            ENDIF
          ELSE
            IF (PHIMA2 .GE. PHILO) THEN
              ON(LAYER,SECTOR) = .TRUE.
              ON(LAYER+2,SECTOR) = .TRUE.
            ENDIF
            IF (PHI2 .EQ. 0.0) PHI2 = PHIMI1 + TWOPI
            IF (PHI2 .GE. PHILO .AND. PHI2 .LT. PHIHI) THEN
              ON(LAYER,SECTOR) = .TRUE.
              ON(LAYER+2,SECTOR) = .TRUE.
            ELSE
              IF (PHI2 .LT. PHILO) THEN
                ON(LAYER,SECTOR) = .TRUE.
                ON(LAYER+2,SECTOR) = .TRUE.
              ENDIF
            ENDIF
          ENDIF
  200   CONTINUE
  100 CONTINUE
C
  999 RETURN  
      END        

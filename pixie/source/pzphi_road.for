      SUBROUTINE PZPHI_ROAD(IDET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Plot arc showing width of PHI road
C-
C-   Inputs  : IDET - 1 for CDC, 2 FOR VTX, 3 FOR FDC, 0 assume CDC
C-   Outputs : 
C-   Controls: 
C-
C-   Created   28-FEB-1989   Sharon Hagopian
C Revised March 5, 1991 to get phi from PARH bank S. Hagopian
C-
C----------------------------------------------------------------------
      IMPLICIT NONE          
C-----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IDET
      REAL PI,TWOPI,HALFPI,RAD
      INCLUDE 'D0$INC:PI.INC/LIST'
      INTEGER LPARH,NROAD,IROAD
      INTEGER IP,INX
      REAL A1,A2,PHI1,PHI2,PHIMIN,PHIMAX
      INTEGER PRUNIT
      INTEGER IVERSION
      REAL RADIUS,DETRAD(3)
      LOGICAL GZPARH
      DATA DETRAD/75.,17.,62./
      DATA PRUNIT /3/
C_________________________________________________________________
      IF(IDET.LT.1.OR.IDET.GT.3)GO TO 999
C get road phi limits from PARH bank
      LPARH=GZPARH()
      IF(LPARH.LE.0)GO TO 999
C Check if is in old format, without road info
      IVERSION=IQ(LPARH+1)
      IF(IVERSION.LT.1)GO TO 999     
      NROAD=IQ(LPARH+10)
      IF(NROAD.LE.0)GO TO 999
      RADIUS=DETRAD(IDET)
      CALL PUOPEN
      CALL PXCOLR('YEL')
      IROAD=0
   10 IROAD=IROAD+1
      IF(IROAD.GT.NROAD)GO TO 900
      INX=LPARH+11+5*(IROAD-1)
      PHI1=Q(INX)
      PHI2=Q(INX+1)
      IF(PHI1.LT.0.AND.PHI2.LT.0)THEN
         PHI1=PHI1+TWOPI
         PHI2=PHI2+TWOPI
      ENDIF
      IF(PHI1.GT.0.AND.PHI2.GT.0)THEN
        PHIMIN=AMIN1(PHI1,PHI2)
        PHIMAX=AMAX1(PHI1,PHI2)
        A1=(180.*PHIMIN)/PI
        A2=(180.*PHIMAX)/PI
        CALL JARC(0.,0.,0.,RADIUS,0,A1,A2)
        GO TO 10
      ELSE
        PHIMIN=AMIN1(PHI1,PHI2)
        PHIMAX=AMAX1(PHI1,PHI2)       
        PHIMIN=PHIMIN+TWOPI
C Draw part of road from PHI=0 to PHIMAX
        A1=0.
        A2=(180.*PHIMAX)/PI
        CALL JARC(0.,0.,0.,RADIUS,0,A1,A2)      
C Draw part of road from PHIMIN to PHI=TWOPI
        A1=(180.*PHIMIN)/PI
        A2=360.
        CALL JARC(0.,0.,0.,RADIUS,0,A1,A2)      
        GO TO 10
      ENDIF              
  900 CALL JRCLOS
C
  999 RETURN
      END       

      LOGICAL FUNCTION STPFDC
C----------------------------------------------------------------------- 
C
C   Purpose and Methods : Handles stepping through the forward drift
C                         cells as defined by DETFDC.
C                         Stores the hits.
C   Inputs  :
C   Outputs :
C-            HITSV(1)   X-GLOBAL
C-            HITSV(2)   Y-GLOBAL
C-            HITSV(3)   Z-GLOBAL
C-            HITSV(4)   distance from cell center along drift direction
C-            HITSV(5)   PULSE HEIGHT ( Integrated charge)
C-            HITSV(6)   Distance to the positive end of the DELAY LINE
C-            HITSV(7)   Distance to the negative end of the DELAY LINE
C-            HITSV(8)   Track length in the cell
C-            HITSV(9)   Track id.=2**11*Secondary track #+Primary track#
C-            HITSV(10)  dx/dz, where: x-drift direction, z-cell thickness.  
C
C-   Created dd-mmm-198y  K.Ng,T.Trippe,K.Nishikawa,Aufderheide,
C-                       G.Rahal-Callot,D.Zieminska
C-   Updated   5-OCT-1988   Jeffrey Bantly : minor corrections 
C-   Updated  17-JUL-1989   Harrison B. Prosper  
C-   Made into pbd inteface function. 
C-   Updated   7-AUG-1989   Jeffrey Bantly : minor corrections  
C                                       
C-----------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCKINE.INC/LIST'                                 
      INCLUDE 'D0$INC:GCTRAK.INC/LIST'
C                                       
      INCLUDE 'D0$INC:GCSETS.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC'
C   
      REAL CELSIZ(4), HITSV(10)    
      REAL TRKLEG, PULPH
      INTEGER IXYZ,IHIT
      REAL  VIN(6), VMID(6), VLOC(6)
C
C ****  Secondary track number bits = 2**11
C
      INTEGER  NSECON    
C
      DATA VIN    /6*0.0/
      DATA NSECON/2048/
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER FDC
C----------------------------------------------------------------------
      STPFDC = .TRUE.
      IF ( DFDC .LT. 2 ) GOTO 999
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL UCTOH('FDC ',FDC,4,4)
      ENDIF
C
      IF ( .NOT. (DFDC .GT. 1 .AND. IHSET .EQ. FDC) ) GOTO 999
C
C ****  No chamber hit if no charge
C
      IF(CHARGE.EQ.0) GO TO 999
C
C ****  Incident point in the sense wire CELL
C
       IF (INWVOL.EQ.1) THEN
        DO 10 IXYZ=1,6
         VIN(IXYZ)=VECT(IXYZ)   
  10    CONTINUE    
       END IF
C
C ****  Exit point : we can look at the DRIFT Distance 
C
       IF ( INWVOL .NE. 2 ) GO TO 999
C
C ****  Calculate midpoint of the track in the cell
C
       DO 20 IXYZ=1,3
         VMID(IXYZ)=(VIN(IXYZ)+VECT(IXYZ))/2.
  20   CONTINUE
C
C ****  Calculate the track length
C
      TRKLEG=SQRT((VIN(1)-VECT(1))**2+(VIN(2)-
     &               VECT(2))**2+(VIN(3)-VECT(3))**2)
C
C ****  Calculate direction at midpoint 
C
      DO 21 IXYZ=1,3
        VMID(IXYZ+3)=(VECT(IXYZ)-VIN(IXYZ))/TRKLEG
   21 CONTINUE
C
C ****  Convert the midpoint to local coordinates
C                            
      CALL LOCGEO(VMID,VLOC,CELSIZ,3)
C
C  Fill hit bank
C     
      IF (IDTYPE.EQ.91) THEN
C  sense wire is along the y axis
        HITSV(4)=VLOC(1)            ! dist. from cell cntr along drift dir.
        HITSV(6)=CELSIZ(2)-VLOC(2)  ! distance to +ve end of wire
        HITSV(7)=CELSIZ(2)+VLOC(2)  ! distance to -ve end of wire
        HITSV(10)=ABS(VLOC(4)/VLOC(6))   ! slope in drift-thickness plane
        IF( HITSV(10) .GT. 100. ) HITSV(10) = 100.0
      ELSE IF (IDTYPE.EQ.92) THEN
C  sense wire is along the x axis
        HITSV(4)=VLOC(2)
        HITSV(6)=CELSIZ(1)-VLOC(1)
        HITSV(7)=CELSIZ(1)+VLOC(1)
        HITSV(10)=ABS(VLOC(5)/VLOC(6))
        IF( HITSV(10) .GT. 100. ) HITSV(10) = 100.0
      ELSE IF (IDTYPE.EQ.93) THEN
C  phi unit 
        HITSV(4)=VLOC(2)
        HITSV(6)=CELSIZ(2)-VLOC(1)
        HITSV(7)=VLOC(1)-CELSIZ(1)
        HITSV(10)=ABS(VLOC(5)/VLOC(6))
        IF( HITSV(7) .LT. 0. ) HITSV(7) = 0.0
        IF( HITSV(10) .GT. 100. ) HITSV(10) = 100.0
      END IF
C
C ****  Landau distribution
C
      CALL CLANDA(TRKLEG,PULPH) 
C
C ****  Fill the rest of hit banks
C
      HITSV(1)= VMID(1)
      HITSV(2)= VMID(2)
      HITSV(3)= VMID(3)        
      HITSV(5)= PULPH
      HITSV(8)= TRKLEG   
C
C ****  Fill track id.=2**11*secondary track # + primary track #
C
      IF ( ISTAK .LE. 15 .AND. ITRA .LE. 2048 )  THEN
         HITSV(9) = FLOAT(ISTAK * NSECON + ITRA)  
      ELSE
         HITSV(9) =  FLOAT(16  * NSECON + ITRA)
      ENDIF
C
      CALL GSAHIT(ISET,IDET,ITRA,NUMBV,HITSV,IHIT)
C--------------------------------------------------------------------
  999 CONTINUE
      RETURN    
      END

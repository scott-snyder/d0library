      SUBROUTINE GUFLD_V1 ( VECT, F )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Produce magnetic field parameters
C-
C-   Inputs  : VECT - initial coordinates
C-   Outputs : F - field components
C-
C-   Created  17-OCT-1985   Hedin
C-   Updated  29-SEP-1990   A.Kiryunin  : include SAMUS toroids' field 
C-   Updated  24-JUL-1991   K. Wyatt Merritt  Fix incorrect usage of 
C-                          GEANT routine GMEDIA. 
C-   Updated   5-AUG-1991   S. Igarashi  Change subroutine name
C-                          from GUFLD to GUFLD_V1.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
      REAL VECT(3),        ! INPUT:INITIAL COORDINATES  
     1 F(3),                ! OUTPUT:FIELD COMPONENTS   
     2 X(3),RAD,XCORN,YCORN,BCF,BEF 
      INTEGER NUMED,I   
      INTRINSIC ABS 
      INTEGER NMAG                      ! Number of the SAMUS magnet
      REAL    VECLOC(3),FIELD(3)        ! Coordinate of particle and
                                        ! field in the SAMUS magnet (local)
      DATA XCORN,YCORN/309.88,309.88/  ! X,Y INSIDE CORNERS OF CF   
      DATA BCF,BEF/20.,20./         ! MAX B IN CF AND EF    
C----------------------------------------------------------------------
C      
      CALL GSCVOL         !  Save GEANT volume tree before GMEDIA call!!
C
      DO 1 I=1,3    
 1    X(I)=VECT(I)  
      CALL GMEDIA(X,NUMED)           ! GET MEDIA NUMBER 
      F(1)=0.   
      F(2)=0.   
      F(3)=0.   
      IF(NUMED.EQ.29) THEN           ! CENTRAL SLABS    
        IF(ABS(X(1)).GT.XCORN) F(2)=BCF*ABS(X(1))/X(1)  
        IF(ABS(X(2)).GT.YCORN) F(1)=-BCF*ABS(X(2))/X(2) 
      ENDIF 
      IF(NUMED.EQ.28) THEN           ! CENTRAL CORNER   
        RAD=SQRT((ABS(X(1))-XCORN)**2+(ABS(X(2))-XCORN)**2) 
        F(1)=-(ABS(X(2))-YCORN)/RAD*ABS(X(2))/X(2)*BCF  
        F(2)= (ABS(X(1))-XCORN)/RAD*ABS(X(1))/X(1)*BCF  
      ENDIF 
      IF(NUMED.EQ.27) THEN           ! END TOROID   
        RAD=SQRT(X(1)**2+X(2)**2)   
        F(1)=-BEF*X(2)/RAD  
        F(2)= BEF*X(1)/RAD  
      ENDIF
      IF(NUMED.EQ.26) THEN
        F(1)=15.0
      ENDIF 
C
C ****  Defines magnetic field in SAMUS toroids
C
      IF ((NUMED.EQ.109).AND.(DSAM.GE.1)) THEN
        CALL SAMAG1 (VECT,VECLOC,NMAG)
        CALL SAMFLD (NMAG,VECLOC,FIELD)
        CALL SAMAG2 (NMAG,FIELD,F)
      ENDIF
C
C      
      CALL GFCVOL         !  Restore GEANT volume tree after GMEDIA call!!
C
  999 RETURN
      END

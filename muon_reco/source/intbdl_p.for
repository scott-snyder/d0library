      SUBROUTINE INTBDL_P(QUAD,ORENT,COEFD,COEFW,X_1,X_2,            
     &   ORDERD,ORDERW,Bdl,ARC,B_mean)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Integrate over the perpendicular component
C-   (to the drift plane) of the magnetic field along the interpolated 
C-   POLINOMIAL path length in the toroid 
C-   Inputs  : QUAD- quadrant,ORENT -oreintation of B-fild                                   
C-   X_1,X_2 -torodial surfaces,COEF -POLINOMIAL
C-   coefficients in the drift plane
C-   Outputs : Bdl - the integral over B,ARC - the length of the 2d path in 
C-    in the toroid
C-   Controls: 
C-
C-   Created  2-JUN-1991   A.Klatchko
C-    2/92 DH USE GTMFLD
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ORENT,QUAD,NSTEPS,I,ORDERD,ORDERW
      REAL UVEC(3),COEFD(*),Bdl,ARC,ETA,dETA,Z_old,Z_new,POLI,dl,X_1,X_2
      REAL dZ,Z_mid,X_11,X_22,ETA_mid,VECOR(3),Y_old,Y_new,dY,Y_mid,ARC2
      REAL X_mid,X_12,X_21,X_old,X_new,dX,dl_3,B(3),B_c,X1,X2,COEFW(*)
      REAL B_mean
      DATA NSTEPS/100/
C----------------------------------------------------------------------
      ETA=X_1
      dETA=(X_2 - X_1)/NSTEPS  
      Bdl=0.0
      ARC=0.0
C  component of the magnetic field which is perpendicular to the bend view    
      IF (ORENT .EQ. 1 .OR. ORENT .EQ. 3)THEN  ! B_y component
       UVEC(1)=0.0 
       UVEC(2)=1.0 
       UVEC(3)=0.0 
      ELSEIF (ORENT .EQ. 2 .OR. ORENT .EQ. 4)THEN  ! B_x component
       UVEC(1)=1.0 
       UVEC(2)=0.0 
       UVEC(3)=0.0 
      ENDIF
      DO I=1,NSTEPS
       Z_old = POLI(ETA,COEFD,ORDERD) 
       Y_old = POLI(ETA,COEFW,ORDERW)
       X_old = ETA
       ETA_mid = ETA + dETA/2.
       Z_mid = POLI(ETA_mid,COEFD,ORDERD)
       Y_mid = POLI(ETA_mid,COEFW,ORDERW)
       X_mid = ETA_mid
       ETA=ETA+dETA
       X_new = ETA
       Z_new = POLI(ETA,COEFD,ORDERD)
       Y_new = POLI(ETA,COEFW,ORDERW)
       dZ = Z_new - Z_old
       dY = Y_new - Y_old
       dX = X_new - X_old
       dl = SQRT(dZ*dZ + dX*dX)
       dl_3 = SQRT(dZ*dZ + dX*dX + dY*dY)
       ARC = ARC + dl_3 
       ARC2 = ARC2 + dl 
       IF(ORENT .EQ. 1)THEN
        VECOR(1) = X_mid
        VECOR(2) = Y_mid
        VECOR(3) = Z_mid
       ELSEIF (ORENT .EQ. 2)THEN  
        VECOR(1) = Y_mid
        VECOR(2) = X_mid
        VECOR(3) = Z_mid
      ELSEIF (ORENT .EQ. 3)THEN  
        VECOR(1) = Z_mid
        VECOR(2) = Y_mid
        VECOR(3) = X_mid
      ELSEIF (ORENT .EQ. 4)THEN  
        VECOR(1) = Y_mid
        VECOR(2) = Z_mid
        VECOR(3) = X_mid
       ENDIF
       CALL GTMFLD(QUAD,VECOR,B) !get B field in the midle of the increment
       B_c = B(1)*UVEC(1) + B(2)*UVEC(2) + B(3)*UVEC(3)
       Bdl = Bdl + B_c*dl
      ENDDO 
      Bdl=ABS(Bdl)
      B_mean=Bdl/ARC2
  999 RETURN
      END

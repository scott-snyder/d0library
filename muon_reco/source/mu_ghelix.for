      SUBROUTINE MU_GHELIX (QUAD,CHARGE,STEP,VECT,VOUT) 
C.  
C.    ******************************************************************    
C.    *                                                                *    
C.    *  Performs the tracking of one step in a magnetic field         *    
C.    *  The trajectory is assumed to be a helix in a constant field   *    
C.    *  taken at the mid point of the step.                           *    
C.    *  Parameters:                                                   *    
C.    *   input  QUAD                                                  *    
C.    *     STEP =arc length of the step asked                         *    
C.    *     VECT =input vector (position,direction cos and momentum)   *    
C.    *     CHARGE=  electric charge of the particle                   *    
C.    *   output                                                       *    
C.    *     VOUT = same as VECT after completion of the step           *    
C.    *                                                                *    
C.    *       Author    M.Hansroul  *********                          *    
C.    *       Updated   8-MAY-1991   AKL - IMPLICIT NONE & GTBFLD      *    
C      DH 2/92 USE GTMFLD
C.    ******************************************************************    
C.  
      IMPLICIT NONE
      INTEGER QUAD,I
      REAL VECT(7),VOUT(7),EC,CHARGE,STEP,PINV,FMINV,PW,PT,RHO,FM 
      REAL XYZ(3),F(3),U(3),V(3),W(3),TET,STET,CTET,DELU,DELV,DELW
      REAL VMOD,VDOT  
      DATA EC/2.9979251E-4/ 
C.  
C.    ------------------------------------------------------------------    
C.  
C       units are kgauss,centimeters,gev/c  
C   
      IF (CHARGE.EQ.0.) GO TO 80    
      XYZ(1) = VECT(1) + 0.5 * STEP * VECT(4)   
      XYZ(2) = VECT(2) + 0.5 * STEP * VECT(5)   
      XYZ(3) = VECT(3) + 0.5 * STEP * VECT(6)   
C   
      CALL GTMFLD(QUAD,XYZ,F) 
      FM     = VMOD(F,3)    
      IF (FM.LT.1.E - 3) GO TO 80   
C   
      PINV   = 1. / VECT(7) 
      FMINV  = 1./FM    
C   
C      w=unit vector in the direction of the field  
C   
      W(1)   = F(1) * FMINV 
      W(2)   = F(2) * FMINV 
      W(3)   = F(3) * FMINV 
C   
C      u=unit vector perpendicular to the plane defined by the field    
C        and the direction cosines vectors  
C        u is given by the normalized cross product of the direction    
C          cosines vector and the unit vector w 
C   
      CALL CROSS(VECT(4),W,U)   
      CALL VUNIT(U,U,3) 
C   
C     v=unit vector perpendicular to u and w to form a right handed 
C       coordinate system   
C   
      CALL CROSS(W,U,V) 
C   
C      radius of curvature  rho=pt*charge/(ec*field)    
C                   where PT is the momentum perpendicular to field 
C      PW=momentum component along the field    
C   
      PW     = VDOT(VECT(4),W,3) * VECT(7)  
      PT     = SQRT(ABS((VECT(7)+PW)*(VECT(7)-PW))) 
      IF(PT.LT.1.E-6)GOTO 80    
      RHO    = CHARGE * PT * FMINV / EC 
C   
C     tet=angle of helix arc in plane perpendicular to field    
C   
      TET    = STEP * PT * PINV / RHO   
      STET   = SIN(TET) 
      CTET   = COS(TET) 
      DELU   = RHO * (1. - CTET)    
      DELV   = RHO * STET   
      DELW   = STEP * PW * PINV 
C   
C    result of step 
C   
      DO 70 I=1,3   
         VOUT(I) = VECT(I) + DELU * U(I) + DELV * V(I) + DELW * W(I)    
         VOUT(I+3) = PT*PINV* (STET*U(I) + CTET*V(I)) + PW*PINV*W(I)    
  70  CONTINUE  
      VOUT(7) = VECT(7) 
C   
      GO TO 99  
  80  CONTINUE  
      DO 90 I=1,3   
         VOUT(I) = VECT(I) + STEP * VECT(I+3)   
         VOUT(I+3) = VECT(I+3)  
  90  CONTINUE  
      VOUT(7) = VECT(7) 
C   
  99  RETURN
      END   

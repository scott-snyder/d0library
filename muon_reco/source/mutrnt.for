      SUBROUTINE MUTRNT(QUAD,P,STEP,VECT)  
C.  
C.    ******************************************************************    
C.    *                                                                *    
C.    *  Runge-Kutta method for tracking a particle through a magnetic *    
C.    *  field. Uses NYSTROEM algorithm (See HANDBOOK NAT. BUR. OF     *    
C.    *  STANDARDS, PROCEDURE 25.5.20)                                 *    
C.    *                                                                *    
C.    *  Input parameters                                              *    
C.    *       P        Particle momentum                               *    
C.    *       STEP     Step size                                       *    
C.    *       VECT     Initial co-ords,direction cosines               *    
C.    *  Output parameters                                             *    
C.    *       VECT     Output co-ords,direction cosines                *
C.    *  User routine called                                           *    
C.    *       CALL GTMFLD(QUAD,X,F)                                    *    
C.    *                                                                *    
C.    ******************************************************************    
C.
C-   Created 3-DEC-1992   Daria Zieminska   
      IMPLICIT  NONE
C     ------------------------------------------------------------------
      INTEGER      I,QUAD
      REAL         F(3),HSAVE(3),XYZ(3),XYZT(3),VECT(6),
     +             SECXS(4),SECYS(4),SECZS(4)   
      REAL         CBA,A,D,C,AT,BT,CT,EC,H,H2,H4,PH2,P,PINV,CHARGE,
     +             STEP,XOLD,YOLD,ZOLD,X,Y,Z,XT,YT,ZT,PH,THIRD
      EQUIVALENCE (X,XYZ(1)),(Y,XYZ(2)),(Z,XYZ(3)), 
     +            (XT,XYZT(1)),(YT,XYZT(2)),(ZT,XYZT(3))    
      DATA         EC/2.9979251E-4/ 
      DATA         THIRD / 0.3333333333 /   
      DATA         XOLD,YOLD,ZOLD/9999.,9999.,9999./    
      DATA         HSAVE/0.,0.,0./  
C.  
C.    ------------------------------------------------------------------    
C.  
C             This constant is for units CM,GEV/C and KGAUSS    
C   
      CHARGE = P/ABS(P)
      PINV   = EC * CHARGE / ABS(P)
      H      = STEP 
C   
      IF (VECT(1).NE.XOLD) GO TO 10  
      IF (VECT(2).NE.YOLD) GO TO 10  
      IF (VECT(3).NE.ZOLD) GO TO 10  
      DO 5 I=1,3    
   5  F(I)   = HSAVE(I) 
      GO TO 20  
C   
  10  CALL GTMFLD(QUAD,VECT,F)    
      DO 15 I=1,3   
  15  HSAVE(I) = F(I)   
C   
C             Start of integration  
C   
  20  X      = VECT(1)  
      Y      = VECT(2)  
      Z      = VECT(3)  
      A      = VECT(4)  
      D      = VECT(5)  
      C      = VECT(6)  
C   
      H2     = 0.5 * H  
      H4     = 0.25 * H 
      PH     = PINV * H 
      PH2    = 0.5 * PH 
      SECXS(1) = (D * F(3) - C * F(2)) * PH2    
      SECYS(1) = (C * F(1) - A * F(3)) * PH2    
      SECZS(1) = (A * F(2) - D * F(1)) * PH2    
      XT     = X + H2 * A + H4 * SECXS(1)   
      YT     = Y + H2 * D + H4 * SECYS(1)   
      ZT     = Z + H2 * C + H4 * SECZS(1)   
C   
C              Second intermediate point    
C   
      CALL GTMFLD(QUAD,XYZT,F)    
      AT     = A + SECXS(1) 
      BT     = D + SECYS(1) 
      CT     = C + SECZS(1) 
C   
      SECXS(2) = (BT * F(3) - CT * F(2)) * PH2  
      SECYS(2) = (CT * F(1) - AT * F(3)) * PH2  
      SECZS(2) = (AT * F(2) - BT * F(1)) * PH2  
      AT     = A + SECXS(2) 
      BT     = D + SECYS(2) 
      CT     = C + SECZS(2) 
      SECXS(3) = (BT * F(3) - CT * F(2)) * PH2  
      SECYS(3) = (CT * F(1) - AT * F(3)) * PH2  
      SECZS(3) = (AT * F(2) - BT * F(1)) * PH2  
      XT     = X + H * (A + SECXS(3))   
      YT     = Y + H * (D + SECYS(3))   
      ZT     = Z + H * (C + SECZS(3))   
      AT     = A + SECXS(3) + SECXS(3)  
      BT     = D + SECYS(3) + SECYS(3)  
      CT     = C + SECZS(3) + SECZS(3)  
C   
      CALL GTMFLD(QUAD,XYZT,F)    
C   
      Z      = Z + (C + (SECZS(1) + SECZS(2) + SECZS(3)) * THIRD) * H   
      Y      = Y + (D + (SECYS(1) + SECYS(2) + SECYS(3)) * THIRD) * H   
      X      = X + (A + (SECXS(1) + SECXS(2) + SECXS(3)) * THIRD) * H   
C   
      SECXS(4) = (BT*F(3) - CT*F(2))* PH2   
      SECYS(4) = (CT*F(1) - AT*F(3))* PH2   
      SECZS(4) = (AT*F(2) - BT*F(1))* PH2   
      A      = A+(SECXS(1)+SECXS(4)+2.0 * (SECXS(2)+SECXS(3))) * THIRD  
      D      = D+(SECYS(1)+SECYS(4)+2.0 * (SECYS(2)+SECYS(3))) * THIRD  
      C      = C+(SECZS(1)+SECZS(4)+2.0 * (SECZS(2)+SECZS(3))) * THIRD  
C   
      CBA    = 1. / SQRT(A*A + D*D + C*C)   
      VECT(1) = X   
      VECT(2) = Y   
      VECT(3) = Z   
      VECT(4) = CBA*A   
      VECT(5) = CBA*D   
      VECT(6) = CBA*C   
C   
      DO 90 I=1,3   
  90  HSAVE(I) = F(I)   
      XOLD   = X    
      YOLD   = Y    
      ZOLD   = Z    
C   
  99  END   

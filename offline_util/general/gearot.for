      SUBROUTINE GEAROT(TH1,PH1,TH2,PH2,TH3,PH3,ROT)
C----------------------------------------------------------------------
C     S/R GEAROT converts angles for rotation in Geant (defined in
C  theta and phi) to 3 x 3 rotation matrix defined in direction 
C  cosines.
C     Inverse conversion, from rotation matrix to Geant angles, may
C  be done by S/R ANGRG.
C
C  INPUT:
C     TH1,PH1      angles to x'-axis defined in Geant.  (in degree)
C     TH2,PH2      angles to y'-axis   :                (    :    )
C     TH3,PH3      angles to z'-axis   :                (    :    )
C                                                                  
C  OUTPUT:
C     ROT(3,3)     3x3 rotation matrix.
C                     ROT(1:3,1) =  cos(x',X)  cos(x',Y)  cos(x',Z)
C                     ROT(1:3,2) =  cos(y',X)  cos(y',Y)  cos(y',Z)
C                     ROT(1:3,3) =  cos(z',X)  cos(z',Y)  cos(z',Z)   
C
C  S.Kunori,   Jan,1986
C---------------------------------------------------------------------
C
      IMPLICIT NONE
C
      REAL TH1,PH1,TH2,PH2,TH3,PH3
      REAL ROT(3,3)                                                
C
C  local variables...
C
      REAL STH,THRAD,PHRAD
      REAL RADDEG        ! conversion factor from radian to degree...
      DATA RADDEG /57.29578/     
C
C  -- conversion for x' axis...
C
      THRAD=TH1/RADDEG
      PHRAD=PH1/RADDEG
      STH=SIN(THRAD)
      ROT(1,1)=STH*COS(PHRAD)
      ROT(2,1)=STH*SIN(PHRAD)
      ROT(3,1)=COS(THRAD)
C
C  -- conversion for y' axis...
C
      THRAD=TH2/RADDEG
      PHRAD=PH2/RADDEG
      STH=SIN(THRAD)
      ROT(1,2)=STH*COS(PHRAD)
      ROT(2,2)=STH*SIN(PHRAD)
      ROT(3,2)=COS(THRAD)
C
C  -- conversion for z' axis...
C
      THRAD=TH3/RADDEG
      PHRAD=PH3/RADDEG
      STH=SIN(THRAD)
      ROT(1,3)=STH*COS(PHRAD)
      ROT(2,3)=STH*SIN(PHRAD)
      ROT(3,3)=COS(THRAD)
C
      RETURN
      END

      SUBROUTINE FITLOC(R,Z,WT,N,ALFA,RG,ZG,VG,VALFA,CHISQ) 
C-----------------------------------------------------------------
C
C   Straight line fit to N indpendent measurements
C   (z-ZG) = (r-RG)*tg(ALFA)
C   (assume errors of r are negligible)
C  
C   Input: 
C             R,Z       measured coordinate pairs 
C             WT        weights=1./sigma**2   
C             N         number of points   
C   
C   Output: 
C             ALFA      arctan(dy/dx) 
C             RG,ZG     center of gravity of fitted line
C             VG        variance of center of gravity
C             VALFA     variance of alfa
C             CHISQ     chi square per d.f. 
C
C   Daria Zieminska MAY 1987
C   
C-----------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N,I,NMAX
      REAL Z(N),R(N),WT(N),ALFA,RG,ZG,VG,VALFA,CHISQ,NDF
      PARAMETER (NMAX=100)
      REAL SR,SZ,S1,SRR,SRZ,SZZ 
C   
      SR  = 0.   
      SZ  = 0.   
      S1  = 0.   
      SRR = 0.  
      SZZ = 0.  
      SRZ = 0.  
      NDF=-2
      DO 100 I = 1,N 
        SR  = SR+R(I)*WT(I)   
        SZ  = SZ+Z(I)*WT(I)   
        S1  = S1+WT(I)    
        IF (WT(I).GT.0.) NDF=NDF+1.
  100 CONTINUE  
      RG=SR/S1
      ZG=SZ/S1
      VG=1./S1
      DO 200 I=1,N
        SRR = SRR+WT(I)*(R(I)-RG)**2 
        SRZ = SRZ+WT(I)*(Z(I)-ZG)*(R(I)-RG)  
        SZZ = SZZ+WT(I)*(Z(I)-ZG)**2 
  200 CONTINUE
      ALFA    = ATAN2(SRZ,SRR)
      VALFA   = COS(ALFA)**4/SRR 
      CHISQ=SZZ-SRZ**2/SRR
      IF (NDF.GT.0.) THEN
        CHISQ=CHISQ/NDF
      ELSE
        CHISQ=0.
      END IF
      IF (CHISQ.GT.999.99) CHISQ=999.
      RETURN
      END   

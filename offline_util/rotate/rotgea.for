      SUBROUTINE ROTGEA(ROT,TH1,PH1,TH2,PH2,TH3,PH3)
C---------------------------------------------------------------------
C     S/R ROTGEA converts angles in rotation matrix (defined in
C  direction cosines) to angles, theta and phi, defined in Geant. 
C  Inverse conversion is done by S/R ANGGR.
C
C  Input:                  
C     ROT(3,3)     3x3 rotation matrix.
C                     ROT(1:3,1) =  cos(x',X)  cos(x',Y)  cos(x',Z)
C                     ROT(1:3,2) =  cos(y',X)  cos(y',Y)  cos(y',Z)
C                     ROT(1:3,3) =  cos(z',X)  cos(z',Y)  cos(z',Z)
C
C  Output:
C     TH1,PH1      angles to x'-axis defined in Geant.  (in degree)
C     TH2,PH2      angles to y'-axis   :                (    :    )
C     TH3,PH3      angles to z'-axis   :                (    :    )
C
C  S.Kunori,    Jan,1986
C---------------------------------------------------------------------
C
      IMPLICIT NONE
C
      REAL ROT(3,3)
      REAL TH1,PH1,TH2,PH2,TH3,PH3       
C  -- conversion factor from radian to degree.
      REAL RADDEG
      DATA RADDEG/57.29578/
C
C  -- conversion for x' axis...
C
      TH1=ACOS(ROT(3,1))*RADDEG
      IF(ABS(ROT(2,1)).GT.1.0E-20.OR.ABS(ROT(1,1)).GT.1.0E-20)THEN
        PH1=ATAN2(ROT(2,1),ROT(1,1))*RADDEG
      ELSE
        PH1=0.0
      ENDIF
C
C  -- conversion for y' axis...                                    
C
      TH2=ACOS(ROT(3,2))*RADDEG
      IF(ABS(ROT(2,2)).GT.1.0E-20.OR.ABS(ROT(1,2)).GT.1.0E-20)THEN
        PH2=ATAN2(ROT(2,2),ROT(1,2))*RADDEG
      ELSE
        PH2=0.0
      ENDIF
C
C  -- conversion for z' axis...
C
      TH3=ACOS(ROT(3,3))*RADDEG
      IF(ABS(ROT(2,3)).GT.1.0E-20.OR.ABS(ROT(1,3)).GT.1.0E-20)THEN
        PH3=ATAN2(ROT(2,3),ROT(1,3))*RADDEG
      ELSE
        PH3=0.0
      ENDIF
C
      RETURN
      END

C----------------------------------------------------------------------
C-
C-   Created  19-JUL-1994   R. J. Genik II
C-   Updated  21-NOV-1994   R. J. Genik II  increased mtop range down to
C-   125 GeV and tanbeta max to 63
C-   Updated  10-MAR-1995   R. J. Genik II  set "final" values 
C-
C----------------------------------------------------------------------
C
      Real  Top_Xminn, Top_Xmaxx, Tb_Xminn, Tb_Xmaxx, M0_Xminn,
     +  M0_Xmaxx, Mhalf_Xminn, Mhalf_Xmaxx, A0_Xminn, A0_Xmaxx,
     +  Smu_Xminn, Smu_Xmaxx, M0_Break
      PARAMETER(Top_Xminn  =  131. )
      PARAMETER(Top_Xmaxx  =  220.) 
      PARAMETER(Tb_Xminn   =  1.05)
      PARAMETER(Tb_Xmaxx   =  70.) 
      PARAMETER(M0_Xminn   =  0.01)
      PARAMETER(M0_Break   =  500.) 
      PARAMETER(M0_Xmaxx   =  4000.) 
      PARAMETER(Mhalf_Xminn  =  35.)
      PARAMETER(Mhalf_Xmaxx  =  1250.)
      PARAMETER(A0_Xminn   =  -5.)  
      PARAMETER(A0_Xmaxx   =  +5.)
      PARAMETER(Smu_Xminn  =  -1.)
      PARAMETER(Smu_Xmaxx    =  +1. )
      REAL    Top_Xmin, Top_Xmax, Tb_Xmin, Tb_Xmax, M0_Xmin, M0_Xmax,
     +  Mhalf_Xmin, Mhalf_Xmax, A0_Xmin, A0_Xmax, Smu_Xmin, Smu_Xmax
      COMMON/MASS_Limits/ Top_Xmin, Top_Xmax, Tb_Xmin, Tb_Xmax, M0_Xmin,
     +  M0_Xmax,  Mhalf_Xmin, Mhalf_Xmax, A0_Xmin, A0_Xmax, Smu_Xmin,
     +  Smu_Xmax

C----------------------------------------------------------------------
C-
C-   Created  19-JUL-1994   R. J. Genik II
C-   Updated  21-NOV-1994   R. J. Genik II  increased top sigma to 40 from
C-   15 and set mean to be 165 from 175; should give us quite a flat
C-   distribution. 
C-
C----------------------------------------------------------------------
C
C ****  'Mean' and 'sigma' for distributions
C
      Real Top_Mean, Top_Sig, Tb_Mean, Tb_Sig, M0_Mean, M0_Sig,
     +  Mhalf_Mean, Mhalf_Sig, A0_Mean, A0_Sig, Smu_Mean, Smu_sig
      PARAMETER( Top_Mean =  165. )
      PARAMETER( Top_Sig=   40.)
      PARAMETER( Tb_Mean =  1.5)
      PARAMETER( Tb_Sig =  3.)
      PARAMETER( M0_Mean=  30.)
      PARAMETER( M0_Sig=  100.)
      PARAMETER( Mhalf_Mean= 30. )
      PARAMETER( Mhalf_Sig=  100.)
      PARAMETER( A0_Mean= 0. ) 
      PARAMETER( A0_Sig=  35.0 )
      PARAMETER( Smu_Mean=  0.)
      PARAMETER( Smu_sig=  0.5)
      Real sqrrt_two_pi
      PARAMETER( sqrrt_two_pi = 0.3989423 ) 

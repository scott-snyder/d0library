* spythia.inc
      character*8 schaf(41:75)
      data schaf/'d_L','d_R','u_L','u_R','s_L','s_R','c_L','c_R',
     $ 'b_L','b_R','t_1','t_2',
     $ 'e_L','e_R','nue_L','nue_R',
     $ 'mu_L','mu_R','numu_L','numu_R',
     $ 'tau_L','tau_R','nutau_L','nutau_R',
     $ 'gluino','chi01','chi02','chi03','chi04',
     $ 'chi11','chi12','H1','H2','A','HC'/
      integer skchg(41:75,3)
      data skchg/-1,-1,2,2,-1,-1,2,2,-1,-1,2,2,
     $ -3,-3,0,0,-3,-3,0,0,-3,-3,0,0,0,4*0,3,3,0,0,0,3,
     $ 12*1,12*0,2,10*0,
     $ 12*1,12*1,0,4*0,2*1,3*0,1/
********

C----------------------------------------------------------------------
C-
C-   Created  18-FEB-1995   EN
C-
C----------------------------------------------------------------------
      COMMON /PRSHDISTECM/ IS,X1,Y1,Z1,X2,Y2,Z2,EHIT,AXENDP,
     +                   AXENDPZ,SENDP,SENDPPHI
      REAL EHIT           
      INTEGER IS          
      REAL X1,Y1,Z1       
      REAL X2,Y2,Z2       
      INTEGER NZDIV       
      PARAMETER(NZDIV=10)
      INTEGER NPHIDIV     
      PARAMETER(NPHIDIV=10)
      INTEGER NFMAX       
      PARAMETER(NFMAX=2000)
      REAL AXENDP(NFMAX)  
                          
      REAL AXENDPZ(NFMAX,NZDIV) 
                                
      REAL SENDP(NFMAX),SENDPPHI(NFMAX,NPHIDIV) 

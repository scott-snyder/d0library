C----------------------------------------------------------------------
C-
C-   Created  10-JAN-1989   Serban D. Protopopescu
C-   flags for unpacking CALORIMETER raw data
C-
C----------------------------------------------------------------------
      COMMON /CUNFLG/ NOTPED,PULSON,ZSUPRS,GNCORR,PEDSUB,
     &  DATYPE,
     &  SBUFF,SSUPRS,
     &  D0VSN,CALVSN,SFTVSN,CNTRLWRD,SYNC,STATUS_VERTEX,PLSWRD,
     &  NCARD,CRT,CAD_POINTER,HEADER_LENGTH,CADEND_POINTER,PLSCOR
      LOGICAL NOTPED,     
     &        PULSON,     
     &        ZSUPRS,     
     &        GNCORR,     
     &        PEDSUB,     
     &        SBUFF,      
     &        SSUPRS,     
     &        PLSCOR      
      INTEGER DATYPE      
C                         
      INTEGER D0VSN       ! =0 D0 data, ='40'X TB data ='20'X Monte Carlo
C                         ! ='60'X Monte Carlo data
      INTEGER CALVSN      
C                         
      INTEGER SFTVSN      
      INTEGER CNTRLWRD,   
     &  SYNC,             
     &  STATUS_VERTEX,    
     &  PLSWRD,           
     &  NCARD,            
     &  CRT,              
     &  CAD_POINTER,      
     &  HEADER_LENGTH,    
     &  CADEND_POINTER    

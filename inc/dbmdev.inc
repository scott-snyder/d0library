C----------------------------------------------------------------------
C- Devices, attributes and areas filled by RCP file
C----------------------------------------------------------------------
      INTEGER         MAXATT
      PARAMETER      (MAXATT = 80)
      INTEGER         MAXDEV
      PARAMETER      (MAXDEV = 140)
      INTEGER         MAXTYP
      PARAMETER      (MAXTYP=13)
      INTEGER        N_TYP                     
      CHARACTER*12   DEV_NAM(MAXDEV,MAXTYP)    
      CHARACTER*4    DEV_ATT(MAXATT,MAXTYP)    
      INTEGER        DEV_ENA(MAXDEV,MAXTYP)    
      INTEGER        N_DEV_NAM(MAXTYP)         
                                               
      INTEGER        N_DEV_ATT(MAXTYP)         
                                               
      CHARACTER*12   DEV_TYP(MAXTYP)           
      INTEGER        TYP_NUM (MAXTYP)          
      INTEGER        TYP_ENA (MAXTYP)          
      INTEGER        TYP_POI (MAXTYP)          
      LOGICAL        DEV_ATTENA(MAXATT,MAXDEV,MAXTYP)   
                                                        
      COMMON /DBMDEVICE/ N_TYP,N_DEV_NAM,N_DEV_ATT,DEV_NAM,
     &  DEV_ATT,DEV_TYP,TYP_NUM,TYP_ENA,TYP_POI,DEV_ATTENA

C----------------------------------------------------------------------
C-                                                      L2JETS_TRGR.INC
C-   Created   6-MAY-1990   Richard V. Astur
C-
C-      Common block to hold parameters used for accessing TRGR bank
C-      L2JETS_TRGR common
C----------------------------------------------------------------------
      INTEGER LJT_HOT,LEM_HOT           
      INTEGER LEN_EM1,LEN_EM2           
      INTEGER LEN_HD1,LEN_HD2           
C                                       
      REAL CTS_TO_GEV                   
C                                       
      REAL CTS_EM_OFF,CTS_HD_OFF        
                                        

      COMMON /L2JETS_TRGR/ LEM_HOT,LJT_HOT,LEN_EM1,LEN_EM2,LEN_HD1,
     &  LEN_HD2,CTS_TO_GEV,CTS_EM_OFF,CTS_HD_OFF


C----------------------------------------------------------------------
C-
C-   Created  18-FEB-1995   EN    HIT CELL INFORMATION FOR AXIAL LAYER
C-
C----------------------------------------------------------------------
      COMMON /AXHITCL/ HITCL,NHITCL,PTHLGCL,ZBIN
      INTEGER NCLMX
      PARAMETER(NCLMX=100)
      INTEGER HITCL(NCLMX)    
      INTEGER NHITCL          
      REAL PTHLGCL(NCLMX)     
      INTEGER ZBIN(NCLMX)     
                              

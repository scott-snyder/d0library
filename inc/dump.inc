C----------------------------------------------------------------------
C-
C-   Created  29-JUL-1988   Serban D. Protopopescu
C-    to manage event dumps
C-
C----------------------------------------------------------------------
      INTEGER NDFL
      PARAMETER (NDFL=20)
      INTEGER NELIST
      PARAMETER (NELIST=8)
      COMMON /DUMP/DUNIT,NFDONE,NHDONE,NDUMP,NSKIP
      INTEGER 
     &  DUNIT,     
     &  NFDONE,    
     &  NHDONE,    
     &  NDUMP,     
     &  NSKIP      

C----------------------------------------------------------------------
C-
C-   Created  21-FEB-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      INTEGER MLEN
      PARAMETER( MLEN = 32 )    
      COMMON /LSQ_MATRIX/ M_NAME(LSQ_MAX),M_ROWS(LSQ_MAX),
     &  M_COLS(LSQ_MAX),M_DELETE(LSQ_MAX),NMAT
C
      CHARACTER*(MLEN) M_NAME   
      INTEGER M_ROWS            
      INTEGER M_COLS            
      INTEGER NMAT              
      INTEGER M_DELETE          
C
      INTEGER MB                
      PARAMETER( MB = MLEN*LSQ_MAX/4 + 3*LSQ_MAX + 1 )
      INTEGER M_BUF(MB)
      EQUIVALENCE( M_BUF(1),M_NAME(1))

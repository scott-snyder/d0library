C DEC/CMS REPLACEMENT HISTORY, Element LBOOST.FOR
C *1    14-DEC-1989 17:47:10 SERBAN "do lorentz boost"
C DEC/CMS REPLACEMENT HISTORY, Element LBOOST.FOR
      SUBROUTINE LBOOST(PREST,N,P1,P2)  
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-            BOOST 4-VECTORS P1 TO PREST REST FRAME 
C-            PUT RESULTING 4-VECTORS IN P2  
C-
C-   Inputs  : 
C-      PREST(4)= 4-vector defining rest frame
C-      N       = number of input vectors
C-      P1(4,N) = input vectors
C-   Outputs : 
C-      P2(4,N) = resulting vectors
C-
C-   Created  14-DEC-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N
      REAL PREST(4),P1(4,N),P2(4,N)    
      REAL    WCN,FACT
      INTEGER I,II,K
C----------------------------------------------------------------------
C   
      DO 1 I=1,N    
      WCN=SQRT(PREST(4)**2-PREST(1)**2-PREST(2)**2-PREST(3)**2) 
      II=(I-1)*4    
      P2(4,I)=(P1(4,I)*PREST(4)-P1(1,I)*PREST(1)-P1(2,I)*PREST(2)   
     1         -P1(3,I)*PREST(3))/WCN   
      FACT=(P2(4,I)+P1(4,I))/(WCN+PREST(4)) 
      DO 2 K=1,3    
    2 P2(K,I)=P1(K,I)-FACT*PREST(K) 
    1 CONTINUE  
      RETURN    
      END   

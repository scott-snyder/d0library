C
C
C======================================================================
C  COMMON /ONEPRT/ contains information about one particle and is 
c  the main communication channel among subroutines generating data
c  for that particle.
c  filled by LOGICAL FUNCTION INITP.
C
      REAL
     1  PX,             
     2  PY,
     3  PZ,
     4  E,              
     5  PHI,            
     6  THETA,          
     7  ETA,            
     7  CST,SNT,        
     8  CSP,SNP,        
     9  AINTL,          
     A  AINTR,          
     A  DCAYL,          
     A  VXYZ            
C
      INTEGER
     1  JDPAR,		
     2  LVERT,          
     3  LPART,          
     4  LPARNT          
C
      LOGICAL QCH,	
     1  QHAD,		
     2  QEM		
C
      COMMON/ONEPRT/LPART,LPARNT,LVERT,JDPAR,PX,PY,PZ,E,PHI,THETA,ETA
     1	,DCAYL,AINTL,AINTR,VXYZ(3),CST,SNT,CSP,SNP,QCH,QHAD,QEM
C	====================================================================
C

C C2EM_ZLINKA.INC
C
C 12-JAN-1994  James T. McKinley
C
C   zebra link for c2em area since ZLINKA is too small
C   I use array element 1 for L2EM link, the rest are for C2EM links
      INTEGER C2EM_NSLINK,C2EM_NRLINK,C2EM_LSLINK,C2EM_LRLINK
      PARAMETER (C2EM_NSLINK=256)        
      PARAMETER (C2EM_NRLINK=256)        
      COMMON/C2EM_ZLINKA/C2EM_LSLINK(C2EM_NSLINK),
     &  C2EM_LRLINK(C2EM_NRLINK)
C
      INTEGER IL2EM,FILTVER,NFIX,NPMAX,NR,NL1MAX
      PARAMETER (IL2EM = 1)     
      PARAMETER (FILTVER = 5)   
      PARAMETER (NFIX = 30)     
      PARAMETER (NR = 8)        
      PARAMETER (NPMAX = 128)   
      PARAMETER (NL1MAX = 256)  



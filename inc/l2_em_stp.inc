C------------------------------------------------------------------
C   The INC file for use in L2_EM cluster analysis of D0
C   data. It is only for Level2 code.
C   This common block contains cuts downloaded only once in the STP file
C      Oct. 5, 89                        Yi    Xia
C-   Updated  16-DEC-1991   James T. Linnemann  equivalence long cuts
C-                              to a master array
C-   Updated   8-FEB-1992   James T. Linnemann   do readin directly to 3d array
C-   Updated  21-FEB-1992   Yi  Xia , Add a memory array.
C-   Updated  28-JUL-1992   James T. McKinley, increase size of EMCUTS array
C-   Updated   6-SEP-1992   James T. Linnemann   remove memory,l2sin
C-   Updated   6-NOV-1993   James T. Linnemann   add ETMIN_CELL
C------------------------------------------------------------------
      INTEGER NETA
      PARAMETER(NETA = 40)
      INTEGER NC1
      PARAMETER(NC1 = 12)
      INTEGER NENRGBIN,NETABIN
      PARAMETER( NENRGBIN = 4 )
      PARAMETER( NETABIN = 8 )
      REAL EMCUTS(NC1,NENRGBIN,NETABIN)
C
      REAL ETH1,ETH2
      REAL ELEVL1,ELEVL2,ELEVL3,EM3L,SS3
      REAL A2,B1,C0,ESIZE(NC1),ETMIN_CELL
C...this is to support some old testing code
      REAL ELECT3(NC1),E_BOUNDS((NENRGBIN-1),2)   


      EQUIVALENCE (ELECT3,ESIZE)
C
C...the following are extracted from L2_EM_RCP
      LOGICAL ET_IN_CAEP
      INTEGER ETA_BOUNDS(NETABIN)
      INTEGER LO_GAMMA_FLOOR,HI_GAMMA_FLOOR 
      INTEGER LO_CONE_LAYER,HI_CONE_LAYER 
      LOGICAL CONE_USE_ICD  

      COMMON/L2_EM_STP/EMCUTS,ESIZE,
     & A2,B1,C0,SS3,EM3L,ET_IN_CAEP,CONE_USE_ICD,
     & LO_GAMMA_FLOOR,HI_GAMMA_FLOOR,LO_CONE_LAYER,HI_CONE_LAYER,
     & ETA_BOUNDS,E_BOUNDS,ETMIN_CELL

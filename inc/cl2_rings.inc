C----------------------------------------------------------------------
C-
C-   Created  14-JUN-1991   James T. Linnemann
C-   Updated  27-Jan-1992   James T. Linnemann add L2 arrays
C-   Updated  29-FEB-1992   James T. Linnemann BYTE; border->NPHI/2
C----------------------------------------------------------------------
      INTEGER BORDER1 
      PARAMETER( BORDER1 = NPHIL1/2 )
      INTEGER BORDER2
      PARAMETER( BORDER2 = NPHIL/2 )
      BYTE L1_JETA                   
      BYTE JETA_L1                   
      BYTE L2_JETA                   
      BYTE JETA_L2                   
      BYTE L1_ETA2LO                 
      BYTE L1_ETA2HI                 
      BYTE L1_JPHI                   
      BYTE L2_JPHI                   
      BYTE DETA3,DPHI3               
      INTEGER*2 L3_DPHI              
      COMMON /CL2_RINGS/ L1_JETA(-NETAL11-BORDER1:NETAL11+BORDER1),
     &                   JETA_L1(-NETAL11-BORDER1:NETAL11+BORDER1),
     &                   L2_JETA(-NETAL-BORDER2:NETAL+BORDER2),
     &                   JETA_L2(-NETAL-BORDER2:NETAL+BORDER2),
     &                   L1_ETA2LO(-NETAL11-BORDER1:NETAL11+BORDER1),
     &                   L1_ETA2HI(-NETAL11-BORDER1:NETAL11+BORDER1),
     &                   L1_JPHI(-BORDER1:NPHIL1+BORDER1),
     &                   L2_JPHI(-BORDER2:NPHIL+BORDER2),
     &                   DETA3(LYEM3A:LYEM3D),DPHI3(LYEM3A:LYEM3D),
     &                   L3_DPHI(-2*NPHIL:2*NPHIL) 


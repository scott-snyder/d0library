C----------------------------------------------------------------------
C-
C-   Created   7-FEB-1989   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      COMMON /FDMAP3/ FDMAP3(2),
     &      FDTMAP(0:1,0:1,0:3,0:5,0:9),  
     &      FDPMAP(0:1,0:36,0:15),        
     &      LCDDN,                        
     &      FDTMPX(0:1,0:1,0:3,0:5,0:9,0:2),
     &      FDPMPX(0:1,0:36,0:15,0:2)
      INTEGER FDMAP3,FDTMAP,FDPMAP,LCDDN,FDTMPX,FDPMPX
C
C   Conents of FDTMPX, FDPMPX are 0 - Fadc channel number
C                                 1 - Length of channel data
C                                 2 - Number of hits in channel

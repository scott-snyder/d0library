C----------------------------------------------------------------------
C-   COMMON /VTXLNK/ contains the vertex chamber /ZEBCOM/ links.
C-   Created around 1986,  Tom Trippe
C-   Modified 7-OCT-1988   Daria Zieminska
C----------------------------------------------------------------------
      COMMON /VTXLNK/ VTXLNK,LVTXH,LVLAY,LVZLA,LVSEC,
     + LVWDA,LVZDA,LCDD1
      INTEGER VTXLNK(2),LVTXH,LVLAY(0:2),LVZLA(0:5),
     + LVSEC(0:31,0:2),LVWDA(0:31,0:2),LVZDA(0:5),LCDD1
C         211 words total.

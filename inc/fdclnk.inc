C----------------------------------------------------------------------
C-   FDCLNK.INC
C-
C-   Link area for forward drift chamber hit bank /ZEBCOM/ links.
C-
C-   Created  29-NOV-1988   Jeffrey Bantly
C-   Updated  19-MAR-1990   Jeffrey Bantly   use logical format
C----------------------------------------------------------------------
      COMMON /FDCLNK/ FDCLNK,LFDCH,LFHLF,LFDUN,LFTQD,LFTSC,
     + LFTDA,LFPSC,LFPDA,LCDD3
      INTEGER FDCLNK(2),LFDCH,LFHLF(0:1),LFDUN(0:1,0:1),
     + LFTQD(0:7,0:1),LFTSC(0:5,0:7,0:1),LFTDA(0:5,0:7,0:1),
     + LFPSC(0:35,0:1),LFPDA(0:35,0:1),LCDD3

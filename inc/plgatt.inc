C DEC/CMS REPLACEMENT HISTORY, Element PLGATT.INC
C *4    24-JUL-1989 00:57:44 ABACHI "UPDATE"
C *3    24-JUL-1989 00:29:29 ABACHI "UPDATE"
C *2    23-JUL-1989 22:50:21 ABACHI "UPDATE FOR NEW BETA RELEASE"
C *1    11-MAY-1989 14:05:44 ABACHI "Inc files for EAS emulator"
C DEC/CMS REPLACEMENT HISTORY, Element PLGATT.INC
C **** COMMON BLOCK PLGATT
C
C    This common block is used for polygon attributes.
C      CPIDCO      integer      Current Polygon interior color.
C      DPIDCO      integer      Default Polygon interior color.
C      CPIDIN      integer      Current Polygon interior intensity.
C      DPIDIN      integer      Default Polygon interior intensity.
C      CPINTR      integer      Current polygon interior style.
C      DPINTR      integer      Default polygon interior style.
C      CPEDGE      integer      Current polygon edge style.
C      DPEDGE      integer      Default polygon edge style.
C      MVERT       integer      Maximum number of vertices.
C      VERTIC(4,MVERT) real     Vertices of polygon.
C      NVERT       integer      Number of vertices defined.
C      PFSIM       integer      Polygon fill simulation mode.
C
      INTEGER MVERT
      PARAMETER (MVERT=100000)
      INTEGER CPIDCO, DPIDCO, CPIDIN, DPIDIN, CPINTR, DPINTR
      INTEGER CPEDGE, DPEDGE
      REAL    VERTIC(4,MVERT)
      INTEGER NVERT, PFSIM

      COMMON /PLGATT/ CPIDCO, DPIDCO, CPIDIN, DPIDIN, CPINTR, DPINTR
      COMMON /PLGATT/ CPEDGE, DPEDGE,  PFSIM,  NVERT, VERTIC

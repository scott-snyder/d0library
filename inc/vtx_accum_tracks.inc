C----------------------------------------------------------------------
C-
C-   Created    8-MAR-1994   Justin R. Bendich
C-   Modified  21-MAR-1994   JB:  Change structure to REALs, increase
C-                           size, change name of common block
C-
C----------------------------------------------------------------------
      INTEGER MaxTrax
      PARAMETER (MaxTrax=50000)
      STRUCTURE /TD/
        LOGICAL*1 Flag
        REAL VertexZ, Phi, PhiErr, Xc, Yc
      END STRUCTURE
      INTEGER NTracks
      RECORD /TD/ TrackData(MaxTrax)
      DATA NTracks/0/
      COMMON /GBNWX6/ NTracks, TrackData

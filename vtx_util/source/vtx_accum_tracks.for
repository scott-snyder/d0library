      SUBROUTINE VTX_ACCUM_TRACKS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Save tracks in a big array so we can process
C-                         them at the end of the run.  Also, call
C-                         WRITE_VTXT to write the tracks to the CALIB
C-                         file.
C-
C-   Created   8-MAR-1994   Justin R. Bendich
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:VTX_ACCUM_TRACKS.INC'
      LOGICAL WRITE_VTXT
      INTEGER GZVTXT, VTXTPtr
C----------------------------------------------------------------------
      IF(WRITE_VTXT()) THEN
        VTXTPtr = GZVTXT(0)
        DO 100 WHILE((VTXTPtr .NE. 0) .AND. (NTracks .LT. MaxTrax))
          NTracks = NTracks + 1
          TrackData(NTracks).VertexZ = Q(VTXTPtr + 15)
          TrackData(NTracks).Phi = Q(VTXTPtr + 6)
          TrackData(NTracks).PhiErr = Q(VTXTPtr + 16)
          TrackData(NTracks).Xc = Q(VTXTPtr + 7)
          TrackData(NTracks).Yc = Q(VTXTPtr + 8)
          VTXTPtr = LQ(VTXTPtr)
  100   CONTINUE
      ENDIF
      END

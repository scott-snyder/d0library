C     FDDELP.INC
C----------------------------------------------------------------------
C-   Purpose: Stores sense wire 0 and delay line information for use
C-            by the graphics routines
C-
C-   Created  27-FEB-1989   Lupe Rosas & Jeffrey Bantly
C-   Updated   4-OCT-1991   Robert E. Avery   Completely changed.
C-                      (note: requires parameters declared in FDPARA.PARAMS)
C-
C----------------------------------------------------------------------
      COMMON /FDDELP/ N_DL_HITS,DRIFT_DIST,DL_DIST,ON_TRK
C
      INTEGER N_DL_HITS(0:MXHALF,0:MXQUAD,0:MXSECT)
      REAL DRIFT_DIST(0:MXHALF,0:MXQUAD,0:MXSECT,MX_HIT_WIRE)
      REAL DL_DIST(0:MXHALF,0:MXQUAD,0:MXSECT,MX_HIT_WIRE)
      LOGICAL ON_TRK(0:MXHALF,0:MXQUAD,0:MXSECT,MX_HIT_WIRE)

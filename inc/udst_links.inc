C----------------------------------------------------------------------
C-   UDST.INC
C-   Created  13-DEC-1993   Ian Adam
C-   Updated  15-OCT-1994   Ulrich Heintz  add VTXT links
C-
C----------------------------------------------------------------------
      INTEGER     N_VTXT_MAX
      PARAMETER(N_VTXT_MAX=50)
      INTEGER     N_TRAK_MAX
      PARAMETER(N_TRAK_MAX=50)
      INTEGER     N_VERT_MAX
      PARAMETER(N_VERT_MAX=10)
      INTEGER     VTXT_NUMBER(N_VTXT_MAX)
      INTEGER     TRACK_NUMBER(N_TRAK_MAX)
      INTEGER     VERT_NUMBER(N_VERT_MAX)
      COMMON /UDST_LINKS/ TRACK_NUMBER,VERT_NUMBER,VTXT_NUMBER

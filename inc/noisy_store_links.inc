C----------------------------------------------------------------------
C-
C-   Created  15-OCT-1991   Allen I. Mincer
C-
C----------------------------------------------------------------------
      INTEGER NVT_MAX
      PARAMETER( NVT_MAX = 105 )
      COMMON /NOISE_STORE_LINKS/SEVENT_HEAD(NVT_MAX),SRUN_HEAD
      INTEGER SEVENT_HEAD,SRUN_HEAD

      SUBROUTINE ISGETP(MOMENTUM_SPEC,N_MOM_CELL,LOWEDG,BWID,
     &  PARTICLE_MOMENTUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To output a random momentum value according to
C-                         a given histogram.
C-
C-   Inputs  :    MOMENTUM_SPEC(*)    Array of momentum spectrum
C-                N_MOM_CELL          number of momentum cells
C-                LOWEDG              Low edge of the histogram
C-                BWID                Bin width
C-
C-   Outputs :    PARTICLE_MOMENTUM   Randomly generated momentum
C-   Controls:
C-
C-   Created  26-OCT-1990   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N_MOM_CELL, LOWEDG
      REAL MOMENTUM_SPEC(N_MOM_CELL),BWID
      REAL PARTICLE_MOMENTUM
C
      REAL XRAN, V(1000)
      INTEGER I
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
      IF(FIRST) THEN
        CALL HBOOK1(2001,'GIVEN MOMENTUM SPECTRUM$',
     &              N_MOM_CELL,LOWEDG,LOWEDG+N_MOM_CELL*BWID,0.0)
        CALL HBOOK1(2002,'GIVEN MOMENTUM SPECTRUM$',
     &              N_MOM_CELL,LOWEDG,LOWEDG+N_MOM_CELL*BWID,0.0)
        CALL HPAK(2001, MOMENTUM_SPEC)
        CALL HISPRE(MOMENTUM_SPEC, N_MOM_CELL)
        FIRST = .FALSE.
      ENDIF
C
      CALL HISRAN(MOMENTUM_SPEC, N_MOM_CELL, LOWEDG, BWID, XRAN)
C
      PARTICLE_MOMENTUM = XRAN
C
      CALL HFILL(2002, PARTICLE_MOMENTUM, 0., 1.0)
C
  999 RETURN
      END

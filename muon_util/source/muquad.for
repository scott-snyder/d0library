      INTEGER FUNCTION MUQUAD(NMOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determines what tracking quadrant 
C-                         a muon module is in
C-
C-   Inputs  : NMOD is Module ID (Phil Martin number)
C-
C-   Outputs : 14 quadrants:
C-                           1-4 in central 1 = +X, 
C-                                          2 = +Y (CEILING)  split into -2,+2
C-                                          3 = -X
C-                                          4 = -Y (BOTTOM)   split into -4,+4
C-                           5-8 north (+Z) end, 9-12 south (-Z) end
C-                           13 SAMUS north, 14 SAMUS south
C-
C-   Created  MAY-1986   David Hedin
C-   DH 12/92 use array
C-   MF 6/94 add SAMUS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMOD,IMOD,PLNX(307)
      DATA PLNX/9*0,1,-2,2,3,0,4,-4,3*0,            ! 1-19
     A 1,-2,2,3,0,4,-4,3*0,1,-2,2,3,0,4,4,13*0,     ! 20-49
     A 11*0,6,6,0,7,8,0,5,23*0,10,10,0,11,12,0,9,2*0,    ! 50-99
     A 1,-2,2,3,3,4,-4,1,2*0,1,-2,2,3,3,4,-4,1,2*0,  ! 100-119
     A 1,-2,2,3,3,0,0,1,2*0,1,-2,2,3,3,4,-4,1,2*0,  ! 120-139
     A 1,-2,2,3,3,4,-4,1,2*0,5,0,0,7,6*0,5,6,6,7,7,8,8,5,2*0,  ! 140-169
     A 10*0,9,0,0,11,6*0,9,10,10,11,11,12,12,9,2*0,   ! 170-199
     A 1,-2,2,3,3,4,-4,1,2*0,1,-2,2,3,3,4,-4,1,2*0,  ! 200-219
     A 1,-2,2,3,3,0,0,1,2*0,1,-2,2,3,3,4,-4,1,2*0,  ! 220-239
     A 1,-2,2,3,3,4,-4,1,2*0,5,6,0,7,0,8,4*0,5,6,6,7,7,8,8,5,2*0,  ! 240-269
     A 5,6,6,7,7,8,8,5,2*0,9,10,0,11,0,12,4*0,         ! 270-289
     A 9,10,10,11,11,12,12,9,2*0,9,10,10,11,11,12,12,9/ ! 290-307
C
      MUQUAD = 0
      IMOD = IABS(NMOD)
      IF (IMOD.GT.0.AND.IMOD.LT.310) MUQUAD=PLNX(IMOD) 
      IF (IMOD.GE.400.AND.IMOD.LT.430) MUQUAD=13
      IF (IMOD.GE.430.AND.IMOD.LT.460) MUQUAD=14
      IF (MUQUAD.LT.0.AND.NMOD.GT.0) MUQUAD=-MUQUAD
      RETURN
      END

C----------------------------------------------------------------------
C-
C-   Created  15-AUG-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      INTEGER MXPART
      PARAMETER( MXPART = 500 )         
      COMMON /CTRAK/ NPART,IDPART(MXPART),PART(5,MXPART),VERT(3),
     &  UVEC(3,MXPART),PHI(MXPART),RAP(MXPART)
      INTEGER NPART,IDPART
      REAL    PART,VERT,PHI,RAP
      REAL    UVEC
C NPART = Number of particles.
C IDPART = Isajet Particle ID
C PART(1-5) = Px,py,pz,P and mass of particle.
C VERT = Primary vertex
C UVEC(3) = unit vector of track direction.
C PHI  = Phi of track.
C RAP = Rapidity of track.

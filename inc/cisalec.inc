C----------------------------------------------------------------------
C-
C-   Created  2-MAY-1990   N.A. Graf
C-   Contains ISAJET electron information. Used by CAPHEL package routines
C-
C----------------------------------------------------------------------
      INTEGER MXISALEC
      PARAMETER( MXISALEC = 20 )         
      COMMON /CISALEC/ NISALEC,P_ISALEC(5,MXISALEC),VERT_ISALEC(3),
     &  UVEC_ISALEC(3,MXISALEC),THETA_ISALEC(MXISALEC),
     &  PHI_ISALEC(MXISALEC),ETA_ISALEC(MXISALEC)

      INTEGER NISALEC
      REAL    P_ISALEC,VERT_ISALEC,THETA_ISALEC,PHI_ISALEC,ETA_ISALEC
      REAL    UVEC_ISALEC
C NISALEC = Number of ISAJET electrons.
C P_ISALEC(1-5) = Px,py,pz,P and mass of electrons.
C VERT_ISALEC = Primary vertex
C UVEC_ISALEC(3) = unit vector of track direction.
C THETA_ISALEC = Theta of track
C PHI_ISALEC  = Phi of track.
C ETA_ISALEC = Eta of track.

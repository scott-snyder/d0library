C----------------------------------------------------------------------
C-
C-      /ECOH_MODULE/
C-
C-      ECOH 'Module volume' geometrical properties.
C-      This puppy is a Geant TRAP.
C-
C-   Modified by Chiaki Yanagisawa 28-March-1991
C-   Created  31-AUG-1990   T DIEHL
C-
C----------------------------------------------------------------------
      INTEGER ECOH_NUMBER_MODULES
      REAL ECOH_MODULE_DZ
      REAL ECOH_MODULE_THETA
      REAL ECOH_MODULE_PHI
      REAL ECOH_MODULE_H1
      REAL ECOH_MODULE_LB1
      REAL ECOH_MODULE_LH1
      REAL ECOH_MODULE_THETA1
      REAL ECOH_MODULE_H2
      REAL ECOH_MODULE_LB2
      REAL ECOH_MODULE_LH2
      REAL ECOH_MODULE_THETA2
      REAL ECOH_MODULE_VOLUME
      REAL X_POSITION0,Y_POSITION0,Z_POSITION0
      COMMON /ECOH_MODULE/ECOH_NUMBER_MODULES,
     $                    ECOH_MODULE_DZ     ,
     $                    ECOH_MODULE_THETA  ,
     $                    ECOH_MODULE_PHI    ,
     $                    ECOH_MODULE_H1     ,
     $                    ECOH_MODULE_LB1    ,
     $                    ECOH_MODULE_LH1    ,
     $                    ECOH_MODULE_THETA1 ,
     $                    ECOH_MODULE_H2     ,
     $                    ECOH_MODULE_LB2    ,
     $                    ECOH_MODULE_LH2    ,
     $                    ECOH_MODULE_THETA2 ,
     $                    ECOH_MODULE_VOLUME ,
     $                    X_POSITION0        ,
     $                    Y_POSITION0        ,
     $                    Z_POSITION0

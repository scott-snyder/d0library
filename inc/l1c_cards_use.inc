C----------------------------------------------------------------------
C-
C-                      LEVEL1_CAL_TRIG_SETUP
C-                      =====================
C-
C-   Contains the description of the Level 1 Calorimeter Trigger Hardware
C-   actually in use.
C-
C-   Created   8-DEC-1989   Sylvain Tisserant (MSU)
C-   Updated  24-OCT-1991   Philippe Laurens, Steven Klocek  
C-                              Changed name of common block from
C-                              LEVEL1_CAL_TRIG_SETUP to L1C_CARDS_USE. 
C-
C----------------------------------------------------------------------
C
C     Number of covered trigger towers and corresponding eta range
C     ------------------------------------------------------------
C
      INTEGER TOWER_USE, ETA_USE
C
C
C     Number of CTFE cards
C     --------------------
C
      INTEGER CTFE_USE
C
C     Number of CHTCR cards
C     ---------------------
C
      INTEGER CHTCR_USE
C
C     Hot Tower Adder Tree
C     --------------------
C
      INTEGER SCND_HOT_USE
C
C     Scalar Adder Trees
C     ------------------
C
      INTEGER FRST_SCALAR_USE, SCND_SCALAR_USE
      INTEGER GL_CMP_USE(GL_EMET_THRTYP:GL_TOTL2_THRTYP)
C
C     Missing Transverse Energy Adder Tree
C     ------------------------------------
C
      INTEGER FRST_MPT_USE, SCND_MPT_USE
      INTEGER MPT_CMP_USE
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C
      COMMON /L1C_CARDS_USE/         TOWER_USE, ETA_USE,
     +                               CTFE_USE,
     +                               CHTCR_USE, 
     +                               SCND_HOT_USE,
     +                               FRST_SCALAR_USE, SCND_SCALAR_USE,
     +                               GL_CMP_USE,
     +                               FRST_MPT_USE, SCND_MPT_USE,
     +                               MPT_CMP_USE

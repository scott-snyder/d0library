C----------------------------------------------------------------------
C-
C-                      GLOBAL_ENERGY_REFERENCES
C-                      ========================
C-
C-   Contains the End of Energy Adder Tree (scalar and missing Pt)
C-   references.
C-
C-   Created  27-FEB-1990   Sylvain Tisserant (MSU)
C-   Updated  24-OCT-1991   Philippe Laurens, Steven Klocek  
C-                              Changed name of common block from
C-                              GLOBAL_ENERGY_REFERENCES to
C-                              L1C_GLOBAL_ENERGY_THRESHOLDS.  
C-
C----------------------------------------------------------------------
C
C     End of Scalar Adder trees :
C     ---------------------------
C
      INTEGER GLOBAL_ENERGY_REF (4,CAT3_MAX,
     +                           GL_EMET_THRTYP:GL_TOTL2_THRTYP)
C
C     End of Total Missing Transverse Energy Adder tree :
C     ---------------------------------------------------
C
      REAL TOTAL_MPT_REF        (MPT_CMP_MAX)
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C
      COMMON /L1C_GLOBAL_ENERGY_THRESHOLDS/ GLOBAL_ENERGY_REF,
     +                                  TOTAL_MPT_REF

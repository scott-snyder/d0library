      INTEGER FUNCTION MTCL2_IWHERE(NETA,NPHI,NLYR)
C----------------------------------------------------------------------
C- MTCL2_IWHERE: part of MTC L2 code (Muon Tracking in the Calorimeter)
C-
C-   Purpose and Methods : This is a version of MTC_IWHERE
C-      shortened for exclusively the hadronic sections of the cal.
C-      Given an input NETA,NPHI,NLYR, the returned value
C-      is a nonzero integer as indicated below for each different
C-      hadronic section of the calorimeter.
C-      The returned value is zero if the cell DNE or is not hadronic.
C-                                         layer   eta
C-   Returned value : MTCL2_IWHERE = 6 CCFH  11-13   (0.,1.0)
C-                               = 7 ECIH  11-15   (1.6,4.5 to 5.2)
C-                               = 8 ECMH  11-15   (1.0,2.0)
C-                               = 9 CCCH  15      (0.,0.6)
C-                               =10 ECOH  15-17   (0.7,1.5)
C-                               = 0 calorimeter cell dne (does not exist)
C-                                 (or it is ganged to another cell)
C-                                 (or it is EM or ICD/MG)
C-   Inputs  : (neta,nphi,nlyr)
C-   Outputs : integer MTCL2_IWHERE ranging from 6-10 or 0
C-
C-   Created  22-JUN-1994   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c- input
      INTEGER NETA,NPHI,NLYR
C- local
      INTEGER aneta
C----------------------------------------------------------------------
      MTCL2_IWHERE = 0
C- is it an EM or ICD/MG layer ?
      IF(nlyr.LE.10) RETURN
C- It must be CCFH,CH,ECOH,MH or IH
      aneta = abs(neta)
C- find ECOH by brute force (ignore OCH3 tied to MCH)
      IF(NLYR.EQ.15 .AND.
     &    (ANETA.GE.8 .AND. ANETA.LE.12)) MTCL2_IWHERE = 10
      IF(NLYR.EQ.16 .AND.
     &    (ANETA.GE.9 .AND. ANETA.LE.13)) MTCL2_IWHERE = 10
      IF(NLYR.EQ.17 .AND.
     &    (ANETA.GE.11 .AND. ANETA.LE.14)) MTCL2_IWHERE = 10
      IF(MTCL2_IWHERE.EQ.10) RETURN
C- there are no more layers ge 16 ...
      IF(NLYR.GE.16) RETURN
C- Is it CCCH (check for main ring beam pipe around iphi=18)?
      IF(NLYR.EQ.15 .AND. ANETA.LE.6) THEN
        MTCL2_IWHERE =  9
        IF( (NPHI.GE.17 .AND. NPHI.LE.19) ) MTCL2_IWHERE = 0
        RETURN
      END IF
C- Is it CCFH ?
      IF(NLYR.LE.13 .AND. (ANETA+NLYR).LE.21) THEN
        MTCL2_IWHERE = 6
        RETURN
      END IF
C- Is it ECIH ?
      IF(( ANETA-NLYR ).GE.6) THEN
        MTCL2_IWHERE = 7
        IF(NLYR.LE.12) THEN
          IF(ANETA.GE.37) MTCL2_IWHERE = 0
        ELSE
          IF(ANETA.GE.38) MTCL2_IWHERE = 0
        END IF
        RETURN
      END IF
C- Is it ECMH ?
      IF( (NLYR-ANETA.LE.1) .AND. (NLYR-ANETA.GE.-5)   ) THEN
        MTCL2_IWHERE = 8
        IF(NLYR.LE.13 .AND. NLYR-ANETA.EQ.1) MTCL2_IWHERE = 0
        IF( (NLYR.EQ.13.OR.NLYR.EQ.14) .AND.
     &        (NLYR-ANETA.EQ.-5) ) MTCL2_IWHERE = 0
        RETURN
      ELSE
        MTCL2_IWHERE = 0
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END

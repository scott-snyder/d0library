      SUBROUTINE OBJECT_TAU(IDX, NMAX, ARRAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return quantities for object TAU.
C-
C-   Inputs  : IDX      [I]   Object Number
C-             NMAX     [I]   Maximum number of quantities to return
C-   Outputs : ARRAY(*) [R]   Array of quantities
C-
C-              ARRAY(1..9)
C-                            PX, PY, PZ, E, ET, ETA, PHI, DET_ETA, QUALITY
C-                    I = 9
C-              ARRAY(I+1)    RMS width
C-              ARRAY(I+2)    EM fraction
C-              ARRAY(I+3)    number of tracks
C-              ARRAY(I+4)    Distance to tau jet center in eta-phi for a track
C-              ARRAY(I+5)    Detector eta of the track
C-              ARRAY(I+6)    impact parameter in X-Y plane of the track
C-              ARRAY(I+7)    distance to VERTEX_Z along Z axis of the track
C-              (I+4)TH TO (I+7)TH repeated for each additional track
C-   Controls:
C-
C-   Created   1-DEC-1992   Harrison B. Prosper
C-   Updated  17-MAY-1993   Hailin Li and Marc Paterno
C-            added tracking related information
C-   Updated  25-MAY-1993   Marc Paterno  Corrected FLINT complaints
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDX, NMAX
      REAL    ARRAY(*)
      INTEGER NOBJS, NSIZE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER STATUS, II, NN
      INTEGER NUMBER, NTOTAL
C
      INTEGER IBUFFER(100)
      REAL    BUFFER(100)
      EQUIVALENCE (IBUFFER(1), BUFFER(1))
C----------------------------------------------------------------------
      INTEGER MINNUM
      PARAMETER( MINNUM  =   9 )
      INTEGER JPX, JPY, JPZ, JE, JPT, JETA, JPHI, JDETA, JQUAL, JBASE
      PARAMETER( JPX = 1,
     &           JPY = JPX + 1,
     &           JPZ = JPY + 1,
     &           JE  = JPZ + 1,
     &           JPT = JE  + 1,
     &           JETA  = JPT   + 1,
     &           JPHI  = JETA  + 1,
     &           JDETA = JPHI  + 1,
     &           JQUAL = JDETA + 1,
     &           JBASE = JQUAL)
C----------------------------------------------------------------------
      REAL E4(4), ET, THETA, ETA, PHI, RMS_WIDTH
      INTEGER NENTRIES
      REAL PTAUREF(20)
      SAVE NUMBER, NTOTAL
C----------------------------------------------------------------------
C
C ****  OBJECT: TAU
C
      II = MIN(IDX, NUMBER)
      NN = MIN(NMAX, NTOTAL)
      CALL VZERO(BUFFER, NN)
C
      IF ( II .GT. 0 ) THEN
        CALL GTPTAU(II, E4, ET, THETA, ETA, PHI, RMS_WIDTH, STATUS)
C
        BUFFER(JPX)     = E4(1)
        BUFFER(JPY)     = E4(2)
        BUFFER(JPZ)     = E4(3)
        BUFFER(JE)      = E4(4)
        BUFFER(JPT)     = ET
        BUFFER(JETA)    = ETA
        BUFFER(JPHI)    = PHI
        BUFFER(JBASE+1) = RMS_WIDTH
C
        CALL GTPTAU_REF(II, NENTRIES, PTAUREF, STATUS)
        IF ( NENTRIES .GT. 0 ) THEN
          BUFFER(JDETA)    = PTAUREF(1)
          CALL UCOPY(PTAUREF(2), BUFFER(JBASE+2), NENTRIES-1)
        ENDIF
C
        CALL UCOPY(BUFFER, ARRAY, NN)
      ENDIF
      RETURN
C
      ENTRY NOBJ_TAUS(NOBJS, NSIZE)
      CALL GTPTAU_TOTAL(NUMBER, STATUS)
      NTOTAL= MINNUM + 15
      NOBJS = NUMBER
      NSIZE = NTOTAL
C
      RETURN
      END

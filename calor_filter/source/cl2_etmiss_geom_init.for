      SUBROUTINE CL2_ETMISS_GEOM_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  fill array of sin, cos phi for met calculations
C-                          initialize z part for ignoring vertex position
C-   Inputs  : none
C-   Outputs : /CL2_ETMISS_GEOM/ with phi part filled and Z part initialized
C-                            for Z = 0
C-   Controls: none
C-
C-   Created  23-OCT-1992   James T. Linnemann
C-   Updated  07-JAN-1993   Amber Boehnlein, added total et tables
C-                                         
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER IETA,IPHI,LYR             ! offline indices after unpacking
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:CL2_ETMISS_GEOM.INC'
      INTEGER I,ICOARSE
      REAL    CL2_ET_CORR, CL2_ET_TO_E
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        DO IPHI = 1,NPHIL
          SN(IPHI,1) = SIN(TWOPI*(IPHI-.5)/NPHIL)       ! fine segments
          CS(IPHI,1) = COS(TWOPI*(IPHI-.5)/NPHIL)
          SN(IPHI,2) = SIN(TWOPI*FLOAT(IPHI)/NPHIL)       ! coarse segments
          CS(IPHI,2) = COS(TWOPI*FLOAT(IPHI)/NPHIL)
        ENDDO
        DO IETA = -NETAL,NETAL
          ET_CORR(IETA)= 1.0
          ET_TO_E(IETA) = CL2_ET_TO_E(IETA)
        ENDDO
        FIRST = .FALSE.
      ENDIF
      RETURN
      END

      SUBROUTINE CC_GEOM(LUNIT_VOL,LUNIT_SET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute CC geometry for GEANT
C-
C-      The CC_GEOM package uses detailed Central Calorimeter
C-      construction parameters, which are stored in a SRCP
C-      data file, to construct CC volumes for use in GEANT.
C-      The volume information includes both dimensions and
C-      material content.  The derived information is then
C-      also stored in a SRCP data file, which is used as
C-      input to GEANT.
C-
C-      It is assumed that the SRCP bank with the construction
C-      parameters has been filled from the SRCP file prior to 
C-      this routine.
C-
C-   Inputs  :  LUNIT_VOL       Logical unit for Volume SRCP output
C-              LUNIT_SET       Logical unit for Set SRCP output
C-   Outputs : none
C-   Controls: none
C-
C-   Created  27-OCT-1988   Stuart Fuess
C-   Updated   4-FEB-1990   Stuart Fuess  New EZ routines; more general
C-                                        common blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Arguments
      INTEGER LUNIT_VOL
      INTEGER LUNIT_SET
C  Include files
      INCLUDE 'D0$INC:MATERIAL_LIST.INC'
      INCLUDE 'D0$INC:MATRIX_LIST.INC'
      INCLUDE 'D0$INC:WRITE_UNIT.INC'
C  Integers
      INTEGER IER
      INTEGER LEN
C----------------------------------------------------------------------
C  Store output logical units in common
C----------------------------------------------------------------------
      OUT_VOL = LUNIT_VOL
      OUT_SET = LUNIT_SET
C----------------------------------------------------------------------
C  Zero list of materials
C----------------------------------------------------------------------
      CALL ZERO_MATERIALS
      CALL EZGETS ('CC_MATERIAL_LIST_SRCP_LABEL',1,
     &              MATERIAL_LIST_SRCP_LABEL,LEN,IER)
C----------------------------------------------------------------------
C  Zero list of matrices
C----------------------------------------------------------------------
      CALL ZERO_MATRICES
      CALL EZGETS ('CC_MATRIX_LIST_SRCP_LABEL',1,
     &              MATRIX_LIST_SRCP_LABEL,LEN,IER)
C----------------------------------------------------------------------
C  CCEM
C----------------------------------------------------------------------
      CALL CCEM_GEOM
C----------------------------------------------------------------------
C  CCFH
C----------------------------------------------------------------------
      CALL CCFH_GEOM
C----------------------------------------------------------------------
C  CCCH
C----------------------------------------------------------------------
      CALL CCCH_GEOM
C----------------------------------------------------------------------
C  Massless Gap
C----------------------------------------------------------------------
      CALL CC_MASSLESS_GAP
C----------------------------------------------------------------------
C  Write materials
C----------------------------------------------------------------------
      CALL WRITE_MATERIALS
C----------------------------------------------------------------------
C  Write rotation matrices
C----------------------------------------------------------------------
      CALL WRITE_MATRICES
C----------------------------------------------------------------------
C  CC Detector Sets
C----------------------------------------------------------------------
      CALL CC_IUSET
      RETURN
      END

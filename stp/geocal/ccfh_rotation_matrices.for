      SUBROUTINE CCFH_ROTATION_MATRICES
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CCFH Rotation Matrices
C-
C-      Establish rotation matrices for each CCFH module.  Positioning
C-      of each module will be derived from rotation matrix.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  21-NOV-1988   Stuart Fuess
C-   Updated   4-FEB-1990   Stuart Fuess  New EZ routines; more general
C-                                        common blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:MATRIX.INC'
C  Integers
      INTEGER IER
      INTEGER MODULE
      INTEGER NUMBER_MODULES
      INTEGER ID
      INTEGER FIRST_ID
C  Reals
      REAL OFFSET
      REAL ANGLE
C----------------------------------------------------------------------
C  Get the number of CCFH modules, and the angular offset of the first
C  module.  The modules are to be positioned parallel to the Z axis,
C  with the first module being the one just above or containing the
C  horizontal (X) axis, and sequentially in a right-handed manner
C  about the +Z axis.
C----------------------------------------------------------------------
      CALL EZGET('CCFH_NUMBER_MODULES',NUMBER_MODULES,IER)
      CALL EZGET('CCFH_ANGULAR_OFFSET',OFFSET,IER)
      CALL EZGET('CCFH_FIRST_ROTATION_MATRIX',FIRST_ID,IER)
C----------------------------------------------------------------------
C  Set angular size of each module
C----------------------------------------------------------------------
      ANGLE = 360. / NUMBER_MODULES
C----------------------------------------------------------------------
C  Initialize matrix ID number
C----------------------------------------------------------------------
      ID = FIRST_ID - 1
C----------------------------------------------------------------------
C  Loop over modules, set and store rotation matrix info
C----------------------------------------------------------------------
      DO MODULE=1,NUMBER_MODULES
        ID = ID + 1
        ID_MATRIX = ID
        VAL_MATRIX(1) = 90.
        VAL_MATRIX(2) = 90. + (MODULE-1) * ANGLE + OFFSET
        VAL_MATRIX(3) = 0.
        VAL_MATRIX(4) = 0.
        VAL_MATRIX(5) = 90.
        VAL_MATRIX(6) = (MODULE-1) * ANGLE + OFFSET
        CALL STORE_MATRIX
      ENDDO
      RETURN
      END

      SUBROUTINE CCCH_CRACK(MODE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CCCH Crack material determination
C-
C-      Use the excess material from each CCCH floor volume
C-      as Crack material.  Material types are:
C-              1 = Uranium
C-              2 = Stainless Steel
C-              3 = Copper
C-              4 = G10
C-      
C-   Inputs  : MODE     Character mode, from the list:
C-                          'ZERO'      Zero sums
C-                          'FLOOR'     Floor contribution
C-                          'ENDPLATE'  Endplate volume subtraction
C-                          'FRONTPLATE'Front plate volume subtraction
C-                          'BACKPLATE' Back plate volume subtraction
C-                          'SKIN'      Module skin contribution
C-                          'MATERIAL'  Determine net material
C-   Outputs : none
C-   Controls: none
C-
C-   Created  04-NOV-1988   Stuart Fuess
C-   Updated   6-JAN-1989   Stuart Fuess  Add backplate correction 
C-   Updated   4-FEB-1990   Stuart Fuess  New EZ routines; more general
C-                                        common blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Arguments
      CHARACTER*(*) MODE
C  Include files
      INCLUDE 'D0$INC:SCCCH_CRACK.INC'
      INCLUDE 'D0$INC:SCCCH_FLOOR.INC'
      INCLUDE 'D0$INC:SCCCH_ENDPLATE.INC'
      INCLUDE 'D0$INC:SCCCH_FRONTPLATE.INC'
      INCLUDE 'D0$INC:SCCCH_BACKPLATE.INC'
      INCLUDE 'D0$INC:SCCCH_MODULE.INC'
      INCLUDE 'D0$INC:CC_MATERIAL_CODES.INC'
      INCLUDE 'D0$INC:MATERIAL.INC'
C  Integers
      INTEGER IER
      INTEGER CODE
      INTEGER N
      INTEGER LSTRING
C  Reals
      REAL WIDTH, INNER_LENGTH, OUTER_LENGTH, THICKNESS
      REAL TOTAL_CRACK_VOL
      REAL  LIQUID_ARGON_VOL
      REAL SUM
C  Characters
      CHARACTER*32 NAME
C----------------------------------------------------------------------
C  If MODE = 'ZERO' then zero sums
C----------------------------------------------------------------------
      IF ( MODE .EQ. 'ZERO' ) THEN
        DO CODE=1,4
          CCCH_CRACK_VOL(CODE) = 0.
        ENDDO
        CCCH_NON_CRACK_VOL = 0.
C----------------------------------------------------------------------
C  If MODE = 'FLOOR' then accumulate floor contributions
C----------------------------------------------------------------------
      ELSE IF ( MODE .EQ. 'FLOOR' ) THEN
        DO CODE=1,4
          CCCH_CRACK_VOL(CODE) = CCCH_CRACK_VOL(CODE) +
     &                           CCCH_FLOOR_VOLUME_EXCESS(CODE)
        ENDDO
        CCCH_NON_CRACK_VOL = CCCH_NON_CRACK_VOL + CCCH_FLOOR_VOLUME
C----------------------------------------------------------------------
C  If MODE = 'ENDPLATE' then subtract endplate volumes
C----------------------------------------------------------------------
      ELSE IF ( MODE .EQ. 'ENDPLATE' ) THEN
        CCCH_NON_CRACK_VOL = CCCH_NON_CRACK_VOL +
     &                       2. * CCCH_ENDPLATE_VOLUME
C----------------------------------------------------------------------
C  If MODE = 'FRONTPLATE' then subtract Front plate volume
C----------------------------------------------------------------------
      ELSE IF ( MODE .EQ. 'FRONTPLATE' ) THEN
        CCCH_NON_CRACK_VOL = CCCH_NON_CRACK_VOL + CCCH_FRONTPLATE_VOLUME
C----------------------------------------------------------------------
C  If MODE = 'BACKPLATE' then subtract Back plate volume
C----------------------------------------------------------------------
      ELSE IF ( MODE .EQ. 'BACKPLATE' ) THEN
        CCCH_NON_CRACK_VOL = CCCH_NON_CRACK_VOL + CCCH_BACKPLATE_VOLUME
C----------------------------------------------------------------------
C  If MODE = 'SKIN' then accumulate module skin contributions
C----------------------------------------------------------------------
      ELSE IF ( MODE .EQ. 'SKIN' ) THEN
        CALL EZGET('CCCH_SIDE_SKIN_WIDTH',WIDTH,IER)
        CALL EZGET('CCCH_SIDE_SKIN_INNER_LENGTH',INNER_LENGTH,IER)
        CALL EZGET('CCCH_SIDE_SKIN_OUTER_LENGTH',OUTER_LENGTH,IER)
        CALL EZGET('CCCH_SIDE_SKIN_THICKNESS',THICKNESS,IER)
        CCCH_CRACK_VOL(2) = CCCH_CRACK_VOL(2) +
     &    WIDTH * (INNER_LENGTH + OUTER_LENGTH) * THICKNESS
C----------------------------------------------------------------------
C  If MODE = 'MATERIAL' compute the properties of the composite
C----------------------------------------------------------------------
      ELSE IF ( MODE .EQ. 'MATERIAL' ) THEN
C----------------------------------------------------------------------
C  Liquid argon fills remainder of module volume
C----------------------------------------------------------------------
        TOTAL_CRACK_VOL = CCCH_MODULE_VOLUME - CCCH_NON_CRACK_VOL
        LIQUID_ARGON_VOL = TOTAL_CRACK_VOL
        DO CODE=1,4
          LIQUID_ARGON_VOL = LIQUID_ARGON_VOL - CCCH_CRACK_VOL(CODE)
        ENDDO
C----------------------------------------------------------------------
C  Get the label, name, and material number to be used for the crack
C----------------------------------------------------------------------
        MATERIAL_LABEL = 'CCCH_CRACK_MATERIAL'
        CALL EZGET('CCCH_CRACK_MATERIAL_NAME',MATERIAL_NAME,IER)
        CALL EZGET('CCCH_CRACK_MATERIAL_CODE',CCCH_CRACK_MATERIAL,IER)
        MATERIAL_CODE = CCCH_CRACK_MATERIAL
C----------------------------------------------------------------------
C  Initialize the number of components
C----------------------------------------------------------------------
        NUMBER_COMPONENTS = 0
C----------------------------------------------------------------------
C  Set the appropriate component values
C  Compute the relative volumes of the different components
C----------------------------------------------------------------------
        N = 0
        SUM = 0.
        DO CODE=1,4
          IF ( CCCH_CRACK_VOL(CODE) .GT. 0. ) THEN
            N = N + 1
            CALL ADDSTR(CODE_CHARACTER(CODE),'_CODE',NAME,LSTRING)
            CALL EZGET(NAME,COMPONENT_CODE(N),IER)
            COMPONENT_FRACTION(N) = CCCH_CRACK_VOL(CODE) /
     &                              TOTAL_CRACK_VOL
            SUM = SUM + COMPONENT_FRACTION(N)
          ENDIF
        ENDDO
        N = N + 1
        CALL EZGET('LIQUID_ARGON_CODE',COMPONENT_CODE(N),IER)
        COMPONENT_FRACTION(N) = 1. - SUM
        NUMBER_COMPONENTS = N
C----------------------------------------------------------------------
C  Write the GEANT SRCP parameters for the material
C----------------------------------------------------------------------
        CALL STORE_MATERIAL
      ENDIF
      RETURN
      END

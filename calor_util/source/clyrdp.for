      SUBROUTINE CLYRDP(IETAC,ILYRC,DEPTH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Converts ILYRC into the hardware DEPTH index,
C-                         using CDPLYR routine (lookup table). 
C-                         (The conversion is independent of phi.)
C-                         NOTE that an invalid DEPTH is returned if this
C-                         routine is called for a massless gap or ICD
C-                         channel;  more information is required to
C-                         deduce the DEPTH index for these channels.
C-
C-   Inputs  : IETAC     physics eta index
C-             ILYRC     physics radial index
C-
C-   Outputs : DEPTH     depth index in the ADC system (0-11)
C-                       if -1, flags invalid input combination
C-
C-   Created  19-DEC-1988   A. P. White
C-   Updated  21-FEB-1989   K. Wyatt Merritt
C-   Updated   7-DEC-1990   Joan Guida, Chip Stewart   - added address fix
C-   Updated  14-AUG-1991   Chip Stewart  - made dependent on CDPLYR  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:CUNFLG.INC'
      INTEGER IETAC,ILYRC,DEPTH
      INTEGER IE,IL,ID,IER
      INTEGER ILD(17,-37:37)    ! Conversion array for ILYRC --> DEPTH:
C                           ! first index = ILYRC [1,17],
C                           ! second index = ABS(IETAC) [1,37],
C                           ! value = DEPTH ([0,11]; -1 invalid flag)
      LOGICAL FIRST
C----------------------------------------------------------------------
      DATA FIRST/.TRUE./
C------------------------------------------------------------------------------
C LOOK-UP TABLES for converting PHYSICS ADDRESSES to ELECTRONICS ADDRESSES
C                       (ILYR,IETA)--> DEPTH
C------------------------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        DO IE= -37,37
          IF (IE.EQ.0) GOTO 10
          DO IL = 1, 17
            ILD(IL,IE) = -1
          END DO
          DO ID = 0, 11
            CALL CDPLYR(ID,IE,IL)
            IF(IL.GT.0.AND.IL.LT.18) THEN
              ILD(IL,IE) = ID
            END IF
          END DO
   10     CONTINUE
        END DO
      END IF
      IE = ABS(IETAC)                   ! Table is indexed on ABS(IETAC)
C
      IF (IE.EQ.0 .OR. IE.GT.37 .OR. ILYRC.LT.1 .OR. ILYRC.GT.17)
     &   THEN ! Check for invalid input variables;
        DEPTH = -2                      ! set variable to invalid value
        GO TO 999                       ! return
      ENDIF
C
      DEPTH = ILD(ILYRC,IETAC)           ! Look up ILYRC in the table
C
  999 RETURN
      END

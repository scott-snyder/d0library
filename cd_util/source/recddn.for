      LOGICAL FUNCTION RECDDN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calls the MC reformatting routines written
C-                         by Chris Klopfenstein for each of the
C-                         Central Detectors: VTX, CDC, FDC.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:             COMMON BLOCK of FLAGS mark detectors
C-                         unpacked and reformatted
C-
C-   Created  31-MAY-1992   Daniel R. Claes
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
      INCLUDE 'D0$LINKS:IZCDD2.LINK'
      INCLUDE 'D0$LINKS:IZCDD3.LINK'
      INCLUDE 'D0$LINKS:IZCDD4.LINK'

      INTEGER LCDD1, LCDD2, LCDD3
      LOGICAL FIRST
      SAVE FIRST
      LOGICAL MC_FLAG,CDD1_FLG, CDD2_FLG, CDD3_FLG
      LOGICAL RAW_DATA
      COMMON /RECDD_FLAGS/ CDD1_FLG, CDD2_FLG, CDD3_FLG
      DATA FIRST / .TRUE. /
C
C----------------------------------------------------------------------
      IF (LHEAD.GT.0) THEN                ! Check if Top bank exists
        RECDDN = .TRUE.
        IF( FIRST ) THEN
          FIRST = .FALSE.
          RAW_DATA = (IQ(LHEAD+1).LT.1000) 
        ENDIF
        IF (RAW_DATA) GO TO 999   !Do not reformat RAW data
C
        LCDD1 = LQ(LHEAD-IZCDD1)          ! Structural link to CDD1
C        IF (LCDD1.LE.0) THEN             ! (raw data bank for VTX)
        CDD1_FLG = .FALSE.
C        ELSE
C          CDD1_FLG = .TRUE.
C          CALL RECDD1 ! reformat the VTX MC data
C        ENDIF
        LCDD2 = LQ(LHEAD-IZCDD2)          ! Structural link to CDD2
        IF (LCDD2.LE.0) THEN              ! (raw data bank for CDC)
          CDD2_FLG = .FALSE.
        ELSE
          CDD2_FLG = .TRUE.
          CALL RECDD2 ! reformat the CDC MC data
        ENDIF
        LCDD3 = LQ(LHEAD-IZCDD3)         ! Structural link to CDD2
        IF (LCDD3.LE.0) THEN             ! (raw data bank for FDC)
          CDD3_FLG = .FALSE.
        ELSE
          CDD3_FLG = .TRUE.
          CALL RECDD3 ! reformat the FDC MC data
        ENDIF
      ELSE
        RECDDN = .FALSE.
      ENDIF
  999 RETURN
      END

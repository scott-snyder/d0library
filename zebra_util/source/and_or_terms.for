      LOGICAL FUNCTION AND_OR_TERMS (IRET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads the AND_OR terms from trigger block.
C-
C-   Inputs  : TRGR bank
C-   Outputs : ENTRY points set to true if bits found
C-             IRET = 0 if success.
C-
C-   Created  23-NOV-1992   Norman A. Amos
C-   Updated   7-DEC-1992   Ulrich Heintz  - use L1EXTRACT_ANDOR_TERM
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL MICRO_BLANK
      LOGICAL MRBS_LOSS
      LOGICAL MR_SCIN_VETO
      LOGICAL TEV_SCN_VETO
      LOGICAL LV0_HALOP
      LOGICAL LV0_HALOPB
      LOGICAL STATE
      INTEGER I_MICRO_BLANK 
      INTEGER I_MRBS_LOSS 
      INTEGER I_MR_SCIN_VETO
      INTEGER I_TEV_SCN_VETO 
      INTEGER I_LV0_HALOP 
      INTEGER I_LV0_HALOPB
      INTEGER IRET,LTRGR_LEVEL1,GZTRGR,GZFIND_CRATE,LOC
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      DATA I_MICRO_BLANK /125/
      DATA I_MRBS_LOSS /121/
      DATA I_MR_SCIN_VETO /120/
      DATA I_TEV_SCN_VETO /122/
      DATA I_LV0_HALOP /127/
      DATA I_LV0_HALOPB /118/
C
C----------------------------------------------------------------------
      AND_OR_TERMS=.FALSE.
C
      ENTRY MICRO_BLANK (IRET)
      LOC = I_MICRO_BLANK
      MICRO_BLANK = .FALSE.
      GOTO 100
      ENTRY MRBS_LOSS (IRET)
      LOC = I_MRBS_LOSS
      MRBS_LOSS = .FALSE.
      GOTO 100
      ENTRY MR_SCIN_VETO (IRET)
      LOC = I_MR_SCIN_VETO
      MR_SCIN_VETO = .FALSE.
      GOTO 100
      ENTRY TEV_SCN_VETO (IRET)
      LOC = I_TEV_SCN_VETO
      TEV_SCN_VETO = .FALSE.
      GOTO 100
      ENTRY LV0_HALOP (IRET)
      LOC = I_LV0_HALOP
      LV0_HALOP = .FALSE.
      GOTO 100
      ENTRY LV0_HALOPB (IRET)
      LOC = I_LV0_HALOPB
      LV0_HALOPB = .FALSE.
      GOTO 100
  100 IRET = 1
      LTRGR_LEVEL1 = GZFIND_CRATE ( 'TRGR', GZTRGR(), 11 )
      IF(LTRGR_LEVEL1.LE.0)RETURN
      CALL L1EXTRACT_ANDOR_TERM(IQ(LTRGR_LEVEL1),LOC,STATE)
      MICRO_BLANK = STATE
      MRBS_LOSS = STATE
      MR_SCIN_VETO = STATE
      TEV_SCN_VETO = STATE
      LV0_HALOP = STATE
      LV0_HALOPB = STATE
      IRET = 0
 999  RETURN
      END

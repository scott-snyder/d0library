      SUBROUTINE L1EXTRACT_JET_LIST(L1_BLOCK, LIST_TYPE,
     &                              COMPLETE, TOT_ENTRY, ENTRY_LIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract from the Level 1 Crate of the TRGR bank the
C-     contents of the Jet List. See also D0 Note 967.
C-
C-   Inputs  : L1_BLOCK [I]
C-
C-             First word of the Level-1 Crate header in the TRGR bank.
C-          -> Use IQ( LTRGR_LEVEL1 )
C-             Where LTRGR_LEVEL1 = GZFIND_CRATE ( 'TRGR', GZTRGR(), 11 )
C-             cf. header of D0$ZEBRA_UTIL$SOURCE:GZFIND_CRATE.FOR for details
C-
C-           +------------------------------------------------------------+
C-           | You must pass the variable IQ(LTRGR_LEVEL1)   DIRECTLY     |
C-           | and NOT A COPY of it.                                      |
C-           |------------------------------------------------------------|
C-           | YES :   L1EXTRACT_JET_LIST ( IQ(LTRGR_LEVEL1),... )        |
C-           |------------------------------------------------------------|
C-           | NO  :   L1_BLOCK = IQ( LTRGR_LEVEL1 )                      |
C-           |         L1EXTRACT_JET_LIST ( L1_BLOCK, ... )               |
C-           +------------------------------------------------------------+
C- 
C-             LIST_TYPE   0 for EM Et Jet List, 1 for TOT Et Jet List
C-
C-   Outputs :  COMPLETE   BOOLEAN flag showing if the list is complete
C-             TOT_ENTRY   If the list is complete, this is the number of
C-                         entries in the list. If the list is incomplete, this
C-                         is the number of entries in the saturated list (16)
C-             ENTRY_LIST  list of the TOT_ENTRY towers 
C-                         ENTRY_LIST(n,0) is the trigger tower ETA
C-                         ENTRY_LIST(n,1) is the trigger tower PHI
C-
C-   Controls: none
C-
C-   Created  13-JUL-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
C
      INTEGER   L1_BLOCK(0:*)
      INTEGER   EM_ET_LIST, TOT_ET_LIST    
      PARAMETER ( EM_ET_LIST = 0, TOT_ET_LIST = 1 )
      INTEGER   LIST_TYPE ! allowed values EM_ET_LIST or TOT_ET_LIST
      LOGICAL   COMPLETE
      INTEGER   TOT_ENTRY
      INTEGER   ETA_COOR, PHI_COOR
      PARAMETER ( ETA_COOR = 0, PHI_COOR = 1 )
      INTEGER   ENTRY_LIST ( 1:16, ETA_COOR:PHI_COOR )
C
      INTEGER JET_OFFSET
      INTEGER JET
      INTEGER ADDRESS, ETA, PHI
C
      INTEGER X8000, X7FFF
      PARAMETER (X8000 = 32768, X7FFF = 32767)
C
      INTEGER JBYT, JBIT
      INTEGER IAND
      EXTERNAL JBYT
C
      INTEGER FIRST_NEG_ETA, FIRST_HD, MAX_ADDRESS
      PARAMETER(FIRST_NEG_ETA = (ETA_MAX-ETA_MIN+1) * 16 * 2)
      PARAMETER(FIRST_HD = FIRST_NEG_ETA * 2)
      PARAMETER(MAX_ADDRESS = FIRST_HD -1)
C
      IF (LIST_TYPE .EQ. EM_ET_LIST) THEN
        JET_OFFSET = TRGR_HEADER_LENGTH + 1 + (EM_ET_JET_LIST-1)/2
      ELSE
        JET_OFFSET = TRGR_HEADER_LENGTH + 1 + (TOT_ET_JET_LIST-1)/2
      ENDIF
C
      TOT_ENTRY = 
     &  IAND(JBYT(L1_BLOCK(JET_OFFSET), FIRST_BYTE, WORD_LENGTH), 
     &       X7FFF)
      IF (IAND(JBYT(L1_BLOCK(JET_OFFSET), FIRST_BYTE, WORD_LENGTH), 
     &         X8000) .EQ. 0) THEN
        COMPLETE = .TRUE.
      ELSE
        COMPLETE = .FALSE.
      ENDIF
C
      IF (TOT_ENTRY .LT. 1) GOTO 999
      DO JET = 1, TOT_ENTRY
        ADDRESS = L1_BLOCK(JET_OFFSET + 2 * JET) 
        ETA = ADDRESS
        IF (IAND(ETA,1) .EQ. 0) THEN
          PHI = 1
        ELSE
          PHI = 17
        ENDIF
        ETA = ETA / 2
C
        PHI = PHI + IAND(ETA, 15)
        ETA = ETA / 16
        ETA = MOD(ETA, 20) + 1
C
        IF (ADDRESS .GE. FIRST_NEG_ETA) THEN
          ETA = - ETA 
        ENDIF
C
        ENTRY_LIST(JET, ETA_COOR) = ETA
        ENTRY_LIST(JET, PHI_COOR) = PHI
C
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END

      SUBROUTINE PRTRGR_JET_LIST_PRINT(LUN, DBLOCK, JET_LIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the contents of the Jet List. This routine
C-     assumes that the contents of the Level 1 Datablock and the Jet List are
C-     packed into 4 byte words, as described in D0 Note 967. The Level 1
C-     Datablock is needed to extract the ADC byte for each tower. As this is
C-     all that is taken from the Datablock, the Jet List need not be from the
C-     given Datablock. This routine supports Jet List Lengths up to 32767
C-     entries.
C-
C-   Inputs  : LUN      The unit number to write to
C-             DBLOCK   The first word of the Level 1 Datablock
C-             JET_LIST The first word of the Jet List
C-   Outputs : none
C-   Controls: none
C-
C-   Created  28-JAN-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
C
      INTEGER LUN
      INTEGER DBLOCK(1:*), JET_LIST(1:*)
C
      INTEGER NUM_ENTRIES
      LOGICAL COMPLETE
C
      INTEGER JET
      INTEGER EM_BYTE, HD_BYTE, ADDRESS, POINTER
      INTEGER TRIG_MASK, ETA, PHI, BIT
      INTEGER ISTAT
C
      INTEGER X8000, X7FFF
      PARAMETER (X8000 = 32768, X7FFF = 32767)
C
      INTEGER FIRST_NEG_ETA, FIRST_HD, MAX_ADDRESS
      PARAMETER(FIRST_NEG_ETA = (ETA_MAX-ETA_MIN+1) * 16 * 2)
      PARAMETER(FIRST_HD = FIRST_NEG_ETA * 2)
      PARAMETER(MAX_ADDRESS = FIRST_HD -1)
C
      NUM_ENTRIES = IAND( JET_LIST(1), X7FFF )
      IF ( IAND( JET_LIST(1), X8000 ) .EQ. 0 ) THEN
        COMPLETE = .TRUE.
      ELSE
        COMPLETE = .FALSE.
      ENDIF
C
  100 FORMAT(' Number of Entries:', I4, ' Complete : ', A)
      IF (COMPLETE .EQV. .TRUE.) THEN
        WRITE(LUN, 100, IOSTAT=ISTAT) NUM_ENTRIES, 'Yes'
      ELSE
        WRITE(LUN, 100, IOSTAT=ISTAT) NUM_ENTRIES, 'No'
      ENDIF
C
      IF (NUM_ENTRIES .LT. 1) GOTO 999
      DO JET = 1, NUM_ENTRIES
        TRIG_MASK = JET_LIST(2*JET)
        ADDRESS = JET_LIST(1 + 2*JET)
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
C       Prevent the program from blowing up if ADDRESS is invalid
C
        IF (ADDRESS .GT. MAX_ADDRESS) THEN
          WRITE(LUN,120,IOSTAT=ISTAT) ADDRESS,
     &      (IBITS(TRIG_MASK, BIT, 1), BIT = TRG_NUM_MIN, TRG_NUM_MAX)
C
        ELSE
          POINTER = (TT_FADC+1)/2 + ADDRESS/4
          EM_BYTE = IBITS( DBLOCK(POINTER), 
     &                     IAND(ADDRESS,3)*BYTE_LENGTH, 
     &                     BYTE_LENGTH)
          HD_BYTE = IBITS( DBLOCK(POINTER+FIRST_HD/4),
     &                     IAND(ADDRESS,3)*BYTE_LENGTH,
     &                     BYTE_LENGTH)
C
          WRITE(LUN, 110, IOSTAT=ISTAT) 
     &      ETA, PHI, EM_BYTE, HD_BYTE, ADDRESS,
     &      (IBITS(TRIG_MASK, BIT, 1), BIT = TRG_NUM_MIN, TRG_NUM_MAX)
        ENDIF
C
  110   FORMAT(' ETA=', SP, I3, ' PHI=', SS, I2, '  EM =', I3, 
     &    ' counts  HD =', I3, ' counts  (Address=', I4,
     &    ') Spec. Trig. Mask (0:31):', 4(' ', 8I1) )
  120   FORMAT(' INVALID TOWER ADDRESS = ', I10,  28X, 
     &      'Spec. Trig. Mask (0:31):', 4(' ', 8I1) )
C
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END

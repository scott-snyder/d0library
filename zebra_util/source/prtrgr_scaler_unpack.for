      CHARACTER*20 FUNCTION PRTRGR_SCALER_UNPACK(OFFSET, DBLOCK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create a string from a five-byte Level 1 Datablock
C-      scaler. If the number is larger than 2**31, then a special format is
C-      used.
C-
C-   Returned value  : String containing the formatted value
C-   Inputs  : OFFSET   The offset into the Lvl 1 Datablock (cf. D0 Note 967)
C-             DBLOCK   An array representing the Datablock.
C-   Outputs : none
C-   Controls: none
C-
C-   Created  24-JAN-1992 Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                          - Moved function type declaration to top of file to
C-                            pass D0FLAVOR.
C-                          - Explicitly initialized return value to pass
C-                            D0FLAVOR.
C-                          - Originally L1UTIL_SCALER_UNPACK
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
      LOGICAL  JBIT
      EXTERNAL JBIT
C
      INTEGER OFFSET
      INTEGER DBLOCK(1:*)
      INTEGER SCALER(2)
      INTEGER BYTE, NUMBER, ISTAT
      LOGICAL HIBIT
C
      PRTRGR_SCALER_UNPACK = ' '
C
C       Put together bits 0:30
      NUMBER = 0
      CALL PRTRGR_FIRST_BYTE_DECODING(1, OFFSET, DBLOCK, BYTE)
      CALL SBYT(BYTE, NUMBER, FIRST_BYTE, BYTE_LENGTH)
      CALL PRTRGR_FIRST_BYTE_DECODING(1, OFFSET+1, DBLOCK, BYTE)
      CALL SBYT(BYTE, NUMBER, SECOND_BYTE, BYTE_LENGTH)
      CALL PRTRGR_FIRST_BYTE_DECODING(1, OFFSET+2, DBLOCK, BYTE)
      CALL SBYT(BYTE, NUMBER, THIRD_BYTE, BYTE_LENGTH)
      CALL PRTRGR_FIRST_BYTE_DECODING(1, OFFSET+3, DBLOCK, BYTE)
      CALL SBYT(BYTE, NUMBER, FOURTH_BYTE, BYTE_LENGTH-1)
      HIBIT = JBIT(BYTE, BYTE_LENGTH)
C
C       Find out if we need to go to the extended format
      CALL PRTRGR_FIRST_BYTE_DECODING(1, OFFSET+4, DBLOCK, BYTE)
      IF ((BYTE .EQ. 0) .AND. (HIBIT .EQV. .FALSE.)) THEN
        WRITE (PRTRGR_SCALER_UNPACK, 100, IOSTAT = ISTAT) NUMBER
  100   FORMAT( I10 )
        GOTO 999
      ELSE
        WRITE (PRTRGR_SCALER_UNPACK, 200, IOSTAT = ISTAT) NUMBER
  200   FORMAT('   *2**31+', I10)
        NUMBER = 0
        CALL PRTRGR_FIRST_BYTE_DECODING(1, OFFSET+3, DBLOCK, BYTE)
        CALL CBYT(BYTE, 8, NUMBER, 1, 1)
        CALL PRTRGR_FIRST_BYTE_DECODING(1, OFFSET+4, DBLOCK, BYTE)
        CALL SBYT(BYTE, NUMBER, FIRST_BYTE+1, BYTE_LENGTH)
        WRITE (PRTRGR_SCALER_UNPACK(1:3), 210, IOSTAT=ISTAT) NUMBER
  210   FORMAT (I3)
        GOTO 999
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END

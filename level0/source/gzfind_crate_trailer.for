      FUNCTION GZFIND_CRATE_TRAILER ( BANK_NAME, LBANK, CRATE_ID )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find the beginning of a given crate's
C-                         trailer words within
C-                         one of the DATA CABLE Zebra Bank:
C-                         TRGR, MUD1, CDD1, CDD2, CDD3, CDD4, CAD1, or CAD2
C-            ****** Note: The crate format must follow the document
C-                        "Raw Data Structure for D0" (6/26/91)
C-
C-   Inputs  :  BANK_NAME [C*4] bank name (e.g. 'TRGR')
C-              LBANK     [I]   zebra link to bank (e.g. GZTRGR())
C-              CRATE_ID  [I]   ID of desired crate (e.g. 11 for Level 1 crate)
C-
C-   Outputs :  Return    [I]   Positive Number = Zebra pointer to the first
C-                                                word of the trailer of the
C-                                                specified crate.
C-                              0 = Desired crate wasn't found
C-                             -1 = Incorrect BANK link argument
C-                             -2 = Bad bank length in IQ(LBANK-1)
C-                             -3 = Invalid Crate ID encountered (corrupt data)
C-                             -4 = Invalid Crate length encountered ( " )
C-
C-            ****** GZFIND_CRATE_TRAILER( 'TRGR', GZTRGR(), 11 ) <=>
C-                        first of the four trailer words for crate 11.
C-
C-   Controls: see entry GZFIND_TRAILER_SET_TRACING for recording tracing info
C-
C-   Created  14-OCT-1992   Jeffrey Bantly   a slight modification of the
C-                                           GZFIND_CRATE routine
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE  'D0$INC:ZEBCOM.INC'
C
      INTEGER   GZFIND_CRATE_TRAILER
      INTEGER   CRATE_ID
      CHARACTER*4 BANK_NAME
      INTEGER   LBANK
      INTEGER   GZFIND_TRAILER_SET_TRACING      ! entry point name
      LOGICAL   STATE                           ! entry point argument
      INTEGER   UNIT                            ! entry point argument
C
      INTEGER   END_BANK, LENGTH_BANK
      INTEGER   END_LAST_CRATE
      INTEGER   BEGIN_THIS_CRATE, END_THIS_CRATE, LENGTH_THIS_CRATE
      INTEGER   THIS_CRATE_ID
      INTEGER   CONTR_WORD_FIRST_CRATE, FIRST_CRATE_ID
      BYTE      FIRST_CRATE_ID_BY_BYTE (1:4)
      BYTE      CONTR_WORD_FIRST_CRATE_BY_BYTE (1:4)
      EQUIVALENCE ( FIRST_CRATE_ID,
     &              FIRST_CRATE_ID_BY_BYTE (1) )
      EQUIVALENCE ( CONTR_WORD_FIRST_CRATE,
     &              CONTR_WORD_FIRST_CRATE_BY_BYTE (1) )
C
      INTEGER   CONTROLLER_WORD_OFFSET
      PARAMETER ( CONTROLLER_WORD_OFFSET = + 2 )
      INTEGER   DATA_CABLE_TRAILER_LENGTH
      PARAMETER ( DATA_CABLE_TRAILER_LENGTH = 16 )
      INTEGER   CRATE_ID_OFFSET, TOTAL_WORD_OFFSET
      PARAMETER ( CRATE_ID_OFFSET = - 2 )
      PARAMETER ( TOTAL_WORD_OFFSET = - 3 )
      INTEGER   ZEBCOM_LIMIT             ! Arbitrary limit for zebra bank size
      PARAMETER ( ZEBCOM_LIMIT = 16777215 ) ! but note that the upper byte is 0.
      INTEGER   CRATE_ID_LIMIT           ! Arbitrary limit for 16-bit crate ID,
      PARAMETER ( CRATE_ID_LIMIT = 255 )   ! but note that the upper byte is 0
C                                          ! and thus sensitive to random data
      INTEGER   LOWER_16_BITS
      PARAMETER ( LOWER_16_BITS = 255 * 256 + 255 ) !  Hex(FFFF)
C
      INTEGER   ERR_NO_BANK, ERR_BANK_LENGTH
      INTEGER   ERR_INVALID_ID, ERR_CRATE_LENGTH
      PARAMETER ( ERR_NO_BANK    = -1, ERR_BANK_LENGTH = -2,
     &            ERR_INVALID_ID = -3, ERR_CRATE_LENGTH = -4 )
C
      INTEGER   CRATE_NUM
      CHARACTER*75   LINE
C
      LOGICAL   TRACING
      INTEGER   TRACING_UNIT
      SAVE      TRACING, TRACING_UNIT
      DATA      TRACING /.FALSE./
C
C----------------------------------------------------------------------
C       Set Default return value, that is no Level 1 crate found.
      GZFIND_CRATE_TRAILER = 0
C
C       verify the specified bank exists
      IF ( LBANK .EQ. 0 ) THEN
        GO TO 999
      END IF
C
C       verify the begin of specified bank pointer is believable
      IF ( ( LBANK .GT. ZEBCOM_LIMIT )
     & .OR.( LBANK .LE. 0 ) ) THEN
        LINE = BANK_NAME // 'not found '
        CALL ERRMSG( LINE, 'GZFIND_CRATE_TRAILER',
     &' Bad link argument ', 'W')
        GZFIND_CRATE_TRAILER = ERR_NO_BANK
        GO TO 999
      END IF
C
C       Try our luck in the first crate of the bank
      IF ( TRACING .EQV. .FALSE. ) THEN
        CONTR_WORD_FIRST_CRATE = IQ( LBANK+1 + CONTROLLER_WORD_OFFSET )
        FIRST_CRATE_ID = 0
        FIRST_CRATE_ID_BY_BYTE (BYTE1)
     &          = CONTR_WORD_FIRST_CRATE_BY_BYTE (BYTE4)
        IF ( FIRST_CRATE_ID .EQ. CRATE_ID ) THEN
          GZFIND_CRATE_TRAILER = LBANK + 1
          GOTO 999
        END IF
      END IF
C
C       find the bottom of the specified bank
      LENGTH_BANK = IQ ( LBANK - 1 )
      END_BANK    = LBANK + LENGTH_BANK
      END_LAST_CRATE   = END_BANK - DATA_CABLE_TRAILER_LENGTH
C
C       verify the end of specified bank pointer is believable
      IF ( ( END_BANK .GT. ZEBCOM_LIMIT )
     & .OR.( END_BANK .LT. LBANK ) ) THEN
        LINE = BANK_NAME // 'bad length '
        CALL ERRMSG( LINE, 'GZFIND_CRATE_TRAILER',
     &             ' Bad length in IQ(LBANK-1) ', 'W')
        GZFIND_CRATE_TRAILER = ERR_BANK_LENGTH
        GO TO 999
      END IF
C
C       Display tracing information, if the option has been selected.
      IF ( TRACING .EQV. .TRUE. ) THEN
        LINE = ' '
        WRITE ( LINE, 1000, ERR = 100 )
     &                      BANK_NAME,
     &                    ' BANK BEGIN :', LBANK,
     &                    ' END :', END_BANK,
     &                    ' LENGTH : ', LENGTH_BANK
 1000   FORMAT ( 1X, A4, 3( A, I7 ) )
  100   CONTINUE
        IF ( TRACING_UNIT .EQ. 0 ) THEN
          CALL INTMSG ( LINE )
        ELSE
          WRITE ( TRACING_UNIT, '(A)' ) LINE
        END IF
      END IF
C
C       ******** Here starts the search *********
C       start with the last crate in the bank and climb back up until reaching
C       the desired crate, or until all crates have been found.
C
      CRATE_NUM = 1
      END_THIS_CRATE   = END_LAST_CRATE
      DO WHILE ( END_THIS_CRATE .GT. LBANK )
C
        THIS_CRATE_ID = IQ( END_THIS_CRATE + CRATE_ID_OFFSET )
        THIS_CRATE_ID = IAND( THIS_CRATE_ID, LOWER_16_BITS )
C
        LENGTH_THIS_CRATE = IQ( END_THIS_CRATE + TOTAL_WORD_OFFSET )
        BEGIN_THIS_CRATE  = END_THIS_CRATE - LENGTH_THIS_CRATE + 1
C
C       Display Tracing Information, if the option has been selected.
        IF ( TRACING .EQV. .TRUE. ) THEN
          LINE = ' '
          WRITE ( LINE, 2000, ERR = 200 )
     &                       ' CRATE :',  CRATE_NUM,
     &                       ' ID :',     THIS_CRATE_ID,
     &                       ' BEGIN :',  BEGIN_THIS_CRATE,
     &                       ' END : ',   END_THIS_CRATE,
     &                       ' LENGTH :', LENGTH_THIS_CRATE
  200     CONTINUE
 2000     FORMAT ( 1X, 5( A, I7 ) )
          IF ( TRACING_UNIT .EQ. 0 ) THEN
            CALL INTMSG ( LINE )
          ELSE
            WRITE ( TRACING_UNIT, '(A)' ) LINE
          END IF
        END IF
C
C       verify crate ID is believable
        IF ( ( THIS_CRATE_ID .GT. CRATE_ID_LIMIT )
     &   .OR.( THIS_CRATE_ID .EQ. 0 ) ) THEN
          LINE = BANK_NAME // 'Bad Crate ID '
          CALL ERRMSG( LINE, 'GZFIND_CRATE_TRAILER',
     &               ' Invalid crate ID found in Bank ', 'W')
          GZFIND_CRATE_TRAILER = ERR_INVALID_ID
          GO TO 999
        END IF
C
C       verify the beginning of crate pointer is believable
        IF ( ( BEGIN_THIS_CRATE .GT. END_THIS_CRATE )
     &   .OR.( BEGIN_THIS_CRATE .LT. LBANK ) ) THEN
          LINE = BANK_NAME // 'Bad Crate length '
          CALL ERRMSG( LINE, 'GZFIND_CRATE_TRAILER',
     &               ' Invalid crate length found in Bank ', 'W')
          GZFIND_CRATE_TRAILER = ERR_CRATE_LENGTH
          GO TO 999
        END IF
C
C       compare the current crate ID to the one looked for
        IF ( THIS_CRATE_ID .EQ. CRATE_ID ) THEN
          GZFIND_CRATE_TRAILER = END_THIS_CRATE + TOTAL_WORD_OFFSET
          IF ( TRACING .EQV. .FALSE. ) GOTO 999
        END IF
C
        CRATE_NUM      = CRATE_NUM + 1
        END_THIS_CRATE = BEGIN_THIS_CRATE - 1
C
      END DO
C
C       The Do While loop should have been exited by finding a crate
C      IF ( GZFIND_CRATE_TRAILER .EQ. 0 ) THEN
C        LINE = ' '
C        WRITE ( LINE, 9000, ERR = 900 ) ' Crate', CRATE_ID,
C     &                                  ' Not Found in ', BANK_NAME
C 9000   FORMAT ( A6, I4, A14, A4 )
C  900   CONTINUE
C        CALL ERRMSG( LINE, 'GZFIND_CRATE_TRAILER',
C     &      ' Couldnt find desired crate ', 'W')
C        GO TO 999
C      END IF
C
C----------------------------------------------------------------------
  999 RETURN
C
      ENTRY GZFIND_TRAILER_SET_TRACING ( STATE, UNIT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : makes GZFIND_CRATE display tracing messages for
C-                         all crates found. Default is no tracing.
C-
C-   Inputs  : STATE logical
C-             UNIT  logical IO unit number for tracing messages.
C-                   if this argument is 0, the routine will call INTMSG instead
C-   Outputs : [I] return value
C-                   -1 = previous state was NO tracing
C-                    0 = previous state was tracing to INTMSG
C-                   +N = previous state was tracing to unit N
C-   Controls:
C-
C-   Created  25-FEB-1992 Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
C
C       return information about previous state
      IF ( TRACING .EQV. .FALSE. ) THEN
        GZFIND_TRAILER_SET_TRACING = -1
      ELSE
        GZFIND_TRAILER_SET_TRACING = UNIT
      END IF
C
C       change to requested state
      TRACING = STATE
      TRACING_UNIT = UNIT
C
      RETURN
      END

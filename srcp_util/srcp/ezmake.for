      SUBROUTINE EZMAKE (LUN,RECSIZ,BKNAME,NBANKS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read an RCP text file containing one or more
C-                         \START ... \STOP blocks and direct each block
C-                         to a different SRCP bank.
C-
C-   Inputs  : LUN         Logical unit number of input file
C-             RECSIZ      Record size in 32-bit words (1-->20)
C-                         If RECSIZ=0 then 20 is assumed (full line).
C-   Outputs : BKNAME(*)   Name as given in \START commands
C-             NBANKS      Number of banks created
C-
C-   Controls: None
C-
C-   Created   9-MAR-1989   Harrison B. Prosper
C-   Updated  30-OCT-1991   Harrison B. Prosper
C-    Better error handling
C-   Updated   9-DEC-1991   Krzysztof L. Genser  
C-    80-->132 characters 
C-   Updated   4-Sep-1993   Herbert Greenlee
C-    Added memory-to-memory option (EZ_GET_FIFO).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER       LUN
      INTEGER       RECSIZ
      CHARACTER*(*) BKNAME(*)
      INTEGER       NBANKS
C
      INTEGER WRDIDS
      INTEGER LBANK
      INTEGER IZLINK
      INTEGER WRDMAX
      INTEGER I,J,N,LP,STATUS,LEVEL,ERROR_LEVEL
      LOGICAL EZERROR,FOUND_START
      INTEGER IER
      CHARACTER*132 RECORD
C
      PARAMETER( LBANK  = 0 )           ! Create stand-alone banks
      PARAMETER( IZLINK = 0 )
      PARAMETER( WRDMAX = 33 )          ! 132-characters maximum
C----------------------------------------------------------------------
C
C ****  Check record size
C
      WRDIDS = RECSIZ
      IF ( (WRDIDS .LE. 0) .OR. (WRDIDS .GT. WRDMAX) ) WRDIDS = WRDMAX
C
      FOUND_START = .FALSE.
      NBANKS = 0
C
    5 CONTINUE
C
C ****  Look for \START command
C
      IF(LUN.GE.0)THEN
        READ (UNIT=LUN,FMT=100,END=90) RECORD
      ELSE
        CALL EZ_GET_FIFO(RECORD, IER)
        IF(IER.NE.0)GO TO 90
      ENDIF
      CALL WORD (RECORD,I,J,N)
      CALL UPCASE (RECORD(I:J),RECORD(I:J))
C
      IF ( RECORD(I:J) .EQ. '\START' ) THEN
C
        FOUND_START = .TRUE.
C
C ****  Get bank name
C
        NBANKS = NBANKS + 1
        BKNAME(NBANKS) = RECORD(J+1:)           ! Get bank name
        CALL WORD (BKNAME(NBANKS),I,J,N)
        BKNAME(NBANKS) = BKNAME(NBANKS)(I:J)
C
C ****  Check if bank exists
C
        CALL EZLOC(BKNAME(NBANKS),LP)
        IF ( LP .LE. 0 ) THEN
C
C ****  Create new bank
C
          CALL EZREAD (LUN,BKNAME(NBANKS),WRDIDS,LBANK,IZLINK)
          IF ( EZERROR(STATUS) ) GOTO 999
C
        ELSE
C
C ****  Bank already present
C
          IF ( ERROR_LEVEL .GT. 0 ) THEN
            RECORD = ' Bank '//BKNAME(NBANKS)(1:N)//' already present'
            CALL ERRMSG('BANKEXISTS','EZMAKE',RECORD,'S')
          ENDIF
C
          IF ( NBANKS .GT. 0 ) THEN
            NBANKS = NBANKS - 1
          ENDIF
        ENDIF
      ENDIF
C
      GOTO 5
C
   90 CONTINUE
      IF ( .NOT. FOUND_START ) THEN
        CALL ERRMSG
     &    ('NOSTART','EZMAKE',' No \START command was found','F')
      ENDIF
C
  100 FORMAT(A)
  999 RETURN
C
      ENTRY EZMAKE_ERROR_LEVEL(LEVEL)
      ERROR_LEVEL = LEVEL
      RETURN
      END

      FUNCTION PBD_GET_NEXT_FLAG(FLAG,VAL,NID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return next PBD flag and its value. Set
C-   NID = 1 to get first flag. Upon exit NID is incremented by 1.
C-   PBD_GET_NEXT_FLAG is set to FALSE if there are no more flags
C-   to return.
C-
C-   Returned value  : FALSE if no more flags; TRUE otherwise
C-   Inputs  : None
C-   Outputs : FLAG     [C*]    Flag name
C-             VAL      [L]     Truth value of flag
C-
C-   Controls: NID      [I]     1 - for first flag
C-
C-   Created  30-APR-1990   Harrison B. Prosper
C-   Updated  25-Feb-1992   Herbert Greenlee
C-      Replace OPEN with D0OPEN
C-   Updated  24-MAY-1992   Boaz Klima  
C-      Set maximum number of packages/flags to 50 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PBD_GET_NEXT_FLAG
C
      CHARACTER*(*) FLAG
      LOGICAL VAL
      INTEGER NID
C
      INTEGER NNAME,I,N,IUNIT,IER
      INTEGER MXNAME,ISTAT
      PARAMETER( MXNAME = 50 )
      CHARACTER*32  NAME(MXNAME),STRING
      LOGICAL ACTIVE,FIRST,OK
      DATA FIRST/.TRUE./
      SAVE NAME,NNAME,FIRST
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL D0OPEN (6,'PBD.FLAGS','OFL',OK)
        IF(.NOT.OK) GOTO 1999
        CALL PBD_DUMP_FLAGS
        CLOSE(UNIT=6)
C
        CALL GTUNIT(34,IUNIT,IER)
        IF(IER.NE.0) GOTO 1999
        CALL D0OPEN (IUNIT,'PBD.FLAGS','M',ACTIVE)
        IF(.NOT.ACTIVE) GOTO 1999
        NNAME = 0
        DO WHILE ( ACTIVE )
          READ (IUNIT,FMT='(1X,A32)',END=100) STRING
          IF ( NNAME .LT. MXNAME ) THEN
            NNAME = NNAME + 1
            NAME(NNAME) = STRING
          ELSE
            ACTIVE = .FALSE.
          ENDIF
        ENDDO
  100   CONTINUE
        CLOSE(UNIT=IUNIT,STATUS='DELETE')
        CALL RLUNIT(34,IUNIT,IER)
      ENDIF
C
C ****  Return current value of NID'th flag
C
      IF ( NID .LE. NNAME ) THEN
        FLAG = NAME(NID)
        CALL PBD_GET_FLAG(FLAG,VAL)
        OK = .TRUE.
        NID = NID + 1
      ELSE
        FLAG = ' '
        VAL  = .FALSE.
        OK   = .FALSE.
      ENDIF
      PBD_GET_NEXT_FLAG = OK
  999 RETURN
 1999 CONTINUE
      PBD_GET_NEXT_FLAG = .FALSE.      
      END

      SUBROUTINE READ_URAN_CDFIX(KEYS,NENT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read the CDFIX DBL3 database
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  14-JAN-1995   Srini Rajagopalan - Stolen from Lars
C-   Modified  1-NOV-1994   Lewis Taylor Goss--modified to work with CDFIX DB
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:D3U.INC'
C
C The first 7 Keys are system keys, 3 is start time, 4 is end time
C
C Key  8 = Uranium run number
C Key  9 = Uranium noise for layer 1
C Key 10 = Uranium noise for layer 2
C Key 11 = Uranium noise for layer 3
C Key 12 = TRD Pressure
C Key 13 = TRD Temperature
C Key 14 = Anode high voltage for layer 1
C Key 15 = Anode high voltage for layer 2
C Key 16 = Anode high voltage for layer 3
C Key 17 = Pot. high voltage for layer 1
C Key 18 = Pot. high voltage for layer 2
C Key 19 = Pot. high voltage for layer 3
C
C Rest of the declarations...
C
      INTEGER MAX_ENT,NKEYS,TEMP,I,J
      PARAMETER (MAX_ENT=1000,NKEYS = 12)
      LOGICAL SORTED
C
      INTEGER KEYS(7+NKEYS,MAX_ENT),IRET,NENT
C----------------------------------------------------------------------
C open DBL3 database....
C
      CALL D3U_INI( IDVSTP,'//DBMFX/TRDFIX','S34',' ',IRET )
      CALL D3U_START( 'DB_FILE', ' ', IRET )
      IF (IRET.NE.0) THEN
        WRITE(*,*)
     &    ' Error opening db_file, Check if file exists, Exiting...'
        GOTO 999
      ENDIF
C
C Read all Keys
C
      CALL RZCDIR( D3_PATH, ' ' )
      CALL RZKEYS( 7+NKEYS, MAX_ENT, KEYS, NENT )
      IF (NENT.LE.0) THEN
        WRITE(*,*) ' No keys found in the database, Exiting...'
        GOTO 999
      ENDIF
C
C close the database
C
      CALL D3U_END
C sort the keys by time-stamp
      SORTED = .FALSE.
      DO WHILE (.NOT.SORTED)
        SORTED = .TRUE.
        DO J = 1,NENT - 1
          IF (KEYS(3,J).GT.KEYS(3,J+1)) THEN
            SORTED = .FALSE.
            DO I = 1, 7 + NKEYS
              TEMP         = KEYS(I,J)
              KEYS(I,J)    = KEYS(I,J+1)
              KEYS(I,J+1 ) = TEMP
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C
  999 CONTINUE
      END

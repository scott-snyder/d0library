      SUBROUTINE FETCH_URAN_CDFIX(KEYS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fetch an element from the CDFIX DBL3 database
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
      INTEGER NKEYS,I,LINK,LINK_KEYS,IRET,MAX_ENT,NENT
      PARAMETER (NKEYS = 12,MAX_ENT=1000)
      LOGICAL FIRST
C
      INTEGER KEY(7+NKEYS),KEYS(7+NKEYS,2),DB_KEYS(7+NKEYS,MAX_ENT)
      INTEGER FIRST_RUN(7+NKEYS),J
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL READ_URAN_CDFIX(DB_KEYS,NENT)
        DO I = 1,7+NKEYS
          FIRST_RUN(I) = DB_KEYS(I,1)
        ENDDO
      ENDIF
C
      IF (FIRST_RUN(3).GT.KEYS(3,1)) THEN
        DO I = 1,7+NKEYS
          DO J = 1,2
            KEYS(I,J) = FIRST_RUN(I)
          ENDDO
        ENDDO
      ELSE
C
C open DBL3 database....
C
        CALL D3U_INI( IDVSTP,'//DBMFX/TRDFIX','S34',' ',IRET )
        CALL D3U_START( 'DB_FILE', ' ', IRET )
        IF (IRET.NE.0) THEN
          WRITE(*,*) ' Error opening db_file, Check if file exists, ',
     &      'Exiting...'
          GOTO 999
        ENDIF
C
C Fetch element
C
        DO I = 1,7+NKEYS
          KEY(I) = KEYS(I,1)
        ENDDO
C
        CALL RZCDIR( D3_PATH, ' ' )
        CALL D3U_FETCH(KEY,1,LINK,IRET)
        IF (IRET.EQ.0) THEN
          WRITE(*,*) 'No matching element found in database, Exiting...'
          CALL D3U_END
          GOTO 999
        ENDIF
C
        LINK_KEYS = LC(LINK + 1)
        DO I = 1,7+NKEYS
          KEYS(I,1) = IC(LINK_KEYS+I)
        ENDDO
C
        IF (KEYS(4,1).NE.999999999) THEN
          CALL D0DBL3_DBINCT(KEYS(4,1),1,KEY(3))
          KEY(4) = KEY(3)
          CALL D3U_FETCH(KEY,1,LINK,IRET)
          IF (IRET.EQ.0) THEN
            WRITE(*,*) 'No matching element found in database, Exiting',
     &        '...'
            CALL D3U_END
            GOTO 999
          ENDIF
C
          LINK_KEYS = LC(LINK + 1)
          DO I = 1,7+NKEYS
            KEYS(I,2) = IC(LINK_KEYS+I)
          ENDDO
        ELSE
          DO I = 1,7+NKEYS
            KEYS(I,2) = KEYS(I,1)
          ENDDO
        ENDIF
C
C close the database
C
        CALL D3U_END
      ENDIF
C
  999 CONTINUE
      END

      SUBROUTINE CREATE_INCLUDE_LIST (DIRECTORY,CMS_ELEMENT,LISMAX,
     &  LIST,NLIST,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Scan CMS element for INCLUDES and return a
C-   list of includes.
C-
C-   Inputs  : DIRECTORY        [C*]    Directory containing CMS lib.
C-             CMS_ELEMENT      [C*]    CMS element (.FOR file)
C-             LISMAX           [I]     Maximum size of LIST
C-
C-   Outputs : LIST(*)          [C*]    List of INCLUDES
C-             NLIST            [I]     Number of includes
C-             ERROR            [I]     0 ... OK
C-                                     -1 ... Error opening file
C-   Controls:
C-
C-   Created  24-AUG-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      CHARACTER*(*) DIRECTORY
      CHARACTER*(*) CMS_ELEMENT
      INTEGER       LISMAX
      CHARACTER*(*) LIST(*)
      INTEGER       NLIST
      INTEGER       ERROR

      INTEGER I,J,K,L,N

      INTEGER FETCH_DATA_BLOCK(5)
      INTEGER STATUS
      INTEGER CMS$FETCH_GET
      INTEGER CMS$FETCH_OPEN
      INTEGER CMS$FETCH_CLOSE
      EXTERNAL CMS$_EOF

      INTEGER NOHISTORY
      PARAMETER( NOHISTORY = 1 )        ! Do not append history

      INTEGER       LENG
      CHARACTER*80 LINE,STRING
      LOGICAL FOUND_AN_INCLUDE,CONTINUE_SCAN

C----------------------------------------------------------------------

      NLIST = 0
      ERROR = 0
      N     = LEN(DIRECTORY)
      L     = LEN(CMS_ELEMENT)
      CALL WORD (DIRECTORY(1:N),I,J,N)
      CALL WORD (CMS_ELEMENT(1:L),I,J,L)
C
C ****  Open CMS element
C
      STATUS = CMS$FETCH_OPEN
     &          (       FETCH_DATA_BLOCK,
     &                  DIRECTORY(1:N),
     &                  CMS_ELEMENT(1:L),
     &                  ,
     &                  NOHISTORY)

      IF ( .NOT. STATUS ) THEN
        ERROR =-1
        GOTO 999
      ENDIF
C
C ****  Loop over lines in file
C
      FOUND_AN_INCLUDE  = .FALSE.
      CONTINUE_SCAN     = .TRUE.

      DO WHILE ( CONTINUE_SCAN )
C
C ****  Extract line from CMS element; check for E-O-F
C
        STATUS = CMS$FETCH_GET (FETCH_DATA_BLOCK,STRING)
        IF ( .NOT. STATUS ) GOTO 50
C
C ****  Skip if end of includes reached; look for first non-comment
C ****  line containing an '=' sign.
C
        IF ( FOUND_AN_INCLUDE ) THEN
          IF ( STRING(1:1) .EQ. ' ' ) THEN
            IF ( INDEX(STRING,'=') .GT. 0 ) THEN
              CONTINUE_SCAN = .FALSE.
            ENDIF
          ENDIF
        ENDIF

        IF ( CONTINUE_SCAN ) THEN

          IF ( STRING(1:1) .EQ. ' ' ) THEN

            IF ( STRING(7:14) .EQ. 'INCLUDE ' ) THEN

              CALL WORD (STRING(15:),I,J,N)     ! Extract include
              I = I + 15
              J = J + 13
              LENG  = N - 2
              LINE  = STRING(I:J)
C
C ****  Accept includes which begin with D0$ or DI$
C
              IF ( (LINE(1:3) .EQ. 'D0$') .OR.
     &             (LINE(1:3) .EQ. 'DI$')  ) THEN

                FOUND_AN_INCLUDE = .TRUE.

                I = INDEX(LINE(1:LENG),'/')     ! Strip off /LIST etc.
                IF ( I .GT. 0 ) THEN
                  LENG = I - 1                  ! Adjust length of string
                ENDIF

                IF ( NLIST .LT. LISMAX ) THEN
                  NLIST = NLIST + 1
                  LIST(NLIST) = LINE(1:LENG)
                ELSE
                  GOTO 50
                ENDIF

              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO

   50 CONTINUE
      STATUS = CMS$FETCH_CLOSE (FETCH_DATA_BLOCK)

  999 RETURN
      END

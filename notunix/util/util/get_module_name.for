      SUBROUTINE GET_MODULE_NAME
     &  (LUN,ELEMENT,LIBRARY,MODULE_NAME,LENMOD,MODULE_TYPE,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Scan a file or a CMS element and extract the
C-   name and type of the module within the file. It is assumed that
C-   there is only one module per file. The file is opened INTERNALLY.
C-
C-   Inputs  : LUN      [I]     Input unit number if > 0
C-             ELEMENT  [C*]    File if LUN > 0; CMS element if LUN = 0
C-             LIBRARY  [C*]    CMS library LUN = 0
C-
C-   Outputs : MODULE_NAME      [C*]    Name of module
C-             LENMOD           [I]     Length of name
C-             MODULE_TYPE      [C*]    Module type
C-                                              PROGRAM
C-                                              SUBROUTINE
C-                                              FUNCTION
C-                                              MODULE
C-             ERROR    [I]     0 ... OK
C-                             -1 ... Error opening file
C-   Controls: None
C-
C-   Notes:
C-
C-      Files with the following file-types can be processed:
C-
C-      .FOR .PAS
C-
C-   Created  24-AUG-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER       LUN
      CHARACTER*(*) ELEMENT
      CHARACTER*(*) LIBRARY
      CHARACTER*(*) MODULE_NAME
      INTEGER       LENMOD
      CHARACTER*(*) MODULE_TYPE
      INTEGER       ERROR

      INTEGER I,J,K,L,N,II,JJ

      INTEGER FETCH_DATA_BLOCK(5)
      INTEGER STATUS
      INTEGER CMS$FETCH_GET
      INTEGER CMS$FETCH_OPEN
      INTEGER CMS$FETCH_CLOSE
      EXTERNAL CMS$_EOF

      INTEGER NOHISTORY
      PARAMETER( NOHISTORY = 1 )        ! Do not append history

      CHARACTER*80 RECORD
      LOGICAL ACTIVE,FOUND,FORTRAN,PASCAL,CC

C----------------------------------------------------------------------

      N    = LEN(LIBRARY)
      L    = LEN(ELEMENT)
      CALL WORD (ELEMENT(1:L),I,J,L)
      FORTRAN = ELEMENT(J-3:J) .EQ. '.FOR'
      PASCAL  = ELEMENT(J-3:J) .EQ. '.PAS'
      CC      = ELEMENT(J-1:J) .EQ. '.C'
C
C ****  Initialize module_name
C
      I = INDEX(ELEMENT(1:L),']')
      IF ( I .GT. 0 ) THEN
        MODULE_NAME = ELEMENT(I+1:L)
        I = INDEX(MODULE_NAME,'.')
        IF ( I .GT. 0 ) THEN
          LENMOD = I - 1
          MODULE_NAME = MODULE_NAME(1:LENMOD)
        ELSE
          CALL WORD (MODULE_NAME,I,J,LENMOD)
        ENDIF
      ENDIF
      MODULE_TYPE = ' '

C
C ****  Open file or CMS element
C
      IF ( LUN .GT. 0 ) THEN
        ERROR =-1
        OPEN (UNIT=LUN,FILE=ELEMENT(1:L),STATUS='OLD',READONLY,ERR=999)
      ELSE
        CALL WORD (LIBRARY(1:N),I,J,N)
        STATUS = CMS$FETCH_OPEN
     &          (       FETCH_DATA_BLOCK,
     &                  LIBRARY(1:N),
     &                  ELEMENT(1:L),
     &                  ,
     &                  NOHISTORY)
        IF ( .NOT. STATUS ) THEN
          ERROR =-1
          GOTO 999
        ENDIF
      ENDIF

      ERROR = 0                         ! Clear error code
C
C ****  Scan element according to type
C
      ACTIVE  = .TRUE.
      DO WHILE ( ACTIVE )
C
C ****  Extract line from element; check for E-O-F
C
        IF ( LUN .GT. 0 ) THEN
          READ (UNIT=LUN,FMT='(A)',END=900) RECORD
        ELSE
          STATUS = CMS$FETCH_GET (FETCH_DATA_BLOCK,RECORD)
          IF ( .NOT. STATUS ) GOTO 900
        ENDIF
        CALL UPCASE (RECORD,RECORD)
C
C ****  Scan according to file type
C
        I = 0
        IF ( FORTRAN ) THEN
C
          IF ((RECORD(1:1).EQ.' ').OR.(RECORD(1:1).EQ.CHAR(9))) THEN
            I = INDEX(RECORD,'SUBROUTINE ')
            IF ( I .LE. 0 ) THEN
              I = INDEX(RECORD,'FUNCTION ')
              IF ( I .LE. 0 ) THEN
                I = INDEX(RECORD,'PROGRAM ')
              ENDIF
            ENDIF
          ENDIF

        ELSEIF ( PASCAL ) THEN

          I = INDEX(RECORD,'MODULE ')
          IF ( I .LE. 0 ) THEN
            I = INDEX(RECORD,'PROGRAM ')
          ENDIF
        ENDIF
        FOUND = I .GT. 0
C
        IF ( FOUND ) THEN
          RECORD = RECORD(I:)
          CALL WORD (RECORD,I,J,K)
          MODULE_TYPE = RECORD(I:J)          ! Get type
          CALL WORD (RECORD(J+1:),II,JJ,K)
          MODULE_NAME = RECORD(J+II:J+JJ)        ! Get module name
          LENMOD = JJ - II + 1          ! Get length
          I = INDEX(MODULE_NAME,'(')    ! Strip off parentheses etc.
          IF ( I .LE. 0 ) THEN
            I = INDEX(MODULE_NAME,';')
          ENDIF
          IF ( I .GT. 0 ) THEN
            LENMOD = I - 1
            MODULE_NAME = MODULE_NAME(1:LENMOD)
          ENDIF
          ACTIVE = .FALSE.
        ENDIF
      ENDDO
C
C ****  Close file
C
  900 CONTINUE
      IF ( LUN .GT. 0 ) THEN
        CLOSE(UNIT=LUN)
      ELSE
        STATUS = CMS$FETCH_CLOSE (FETCH_DATA_BLOCK)
      ENDIF

  999 RETURN
      END

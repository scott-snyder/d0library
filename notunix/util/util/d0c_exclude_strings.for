      SUBROUTINE D0C_EXCLUDE_STRINGS (LIST,LLEN,NN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Scan source file and exclude from the list
C-   LIST the strings in LIST found in the file.
C-
C-   Inputs  : LIST(*)  [C*]
C-             LLEN(*)  [I]
C-             NN
C-   Outputs : NN
C-   Controls: None
C-
C-   Created  11-AUG-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      CHARACTER*(*) LIST(*)
      INTEGER       LLEN(*)
      INTEGER       NN

      INTEGER LUN
      PARAMETER( LUN = 99 )

      CHARACTER*132 FILENAME
      LOGICAL ACTIVE
      INTEGER II,JJ,KK
      CHARACTER*80 RECORD
C----------------------------------------------------------------------
      CALL D0C_GET_FILENAME (FILENAME)

      OPEN (UNIT=LUN,FILE=FILENAME,STATUS='OLD',READONLY)

      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
        READ (UNIT=LUN,FMT='(A)',END=900) RECORD
        CALL STR$UPCASE (RECORD,RECORD)
C
C ****  Set string length to zero if string was found in
C ****  record
C
        DO II =  1,NN
          IF ( LLEN(II) .GT. 0 ) THEN
            IF ( INDEX(RECORD,LIST(II)(1:LLEN(II)) ) .GT. 0 ) THEN
              LLEN(II) = 0
            ENDIF
          ENDIF
        ENDDO

      ENDDO

  900 CONTINUE
      CLOSE(UNIT=LUN)
C
C ****  Exclude strings whose lengths were set to zero.
C
      KK = 0
      DO II =  1,NN
        IF ( LLEN(II) .GT. 0 ) THEN
          KK = KK + 1
          LIST(KK) = LIST(II)
          LLEN(KK) = LLEN(II)
        ENDIF
      ENDDO
      NN = KK

  999 RETURN
      END

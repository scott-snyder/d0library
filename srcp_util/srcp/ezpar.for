      SUBROUTINE EZPAR (IN,OPTION,PARAM,NPAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return parameter names in an RCP TEXT file
C-                         on disk.
C-
C-   Inputs  : IN          Logical Unit number of input data stream
C-             OPTION      'ALL'       Return ALL parameter names
C-                         'ARRAY'     Return ALL array names
C-                         'anything'  Return ALL non-array names
C-   Outputs : PARAM(*)    Array of names (character)
C-             NPAR        Number of parameters
C-
C-   Created 13-OCT-1988   Harrison B. Prosper
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL ARRAY
      INTEGER IN,I,J,N,NPAR
      CHARACTER*(*) OPTION,PARAM(*)
      CHARACTER*32  OPT,NAME
      CHARACTER*132  STRING
C----------------------------------------------------------------------
C
      N = LEN(OPTION)
      OPT = OPTION(1:N)
      CALL UPCASE (OPT(1:N),OPT(1:N))
C
      NPAR = 0
   50 CONTINUE
      READ(IN,100,END=999) STRING
  100 FORMAT(A)
C
C ****  Ignore comments
C
      IF ( STRING(1:1) .EQ. '!' ) GOTO 50
      IF ( STRING(2:2) .EQ. '!' ) GOTO 50
C
C ****  Extract parameter name
C
      CALL WORD (STRING,I,J,N)
C
C ****  Ignore blank lines
C
      IF ( N .LE. 0 ) GOTO 50
C
      NAME = STRING(I:J)
      CALL UPCASE (NAME,NAME)
C
      IF ( NAME(1:N) .EQ. '\SIZE' )  GOTO 50
      IF ( NAME(1:N) .EQ. '\STOP' )  GOTO 50
      IF ( NAME(1:N) .EQ. '\START' ) GOTO 50
C
C ****  Pick up array names
C
      IF ( NAME(1:6) .EQ. '\ARRAY' ) THEN
        ARRAY = .TRUE.
C
        IF ( (OPT(1:5).EQ.'ARRAY') .OR. (OPT(1:3).EQ.'ALL') ) THEN
          STRING = STRING(J+1:)
          CALL WORD (STRING,I,J,N)
          NAME = STRING(I:J)
          NPAR = NPAR + 1
          PARAM(NPAR) = NAME
        ENDIF
        GOTO 50
C
      ELSEIF (NAME(1:4) .EQ. '\END' ) THEN
        ARRAY = .FALSE.
        GOTO 50
      ENDIF
C
      IF ( ARRAY ) GOTO 50
C
C ****  Pick up parameter names
C
      NPAR = NPAR + 1
      PARAM(NPAR) = NAME
      GOTO 50
C
  999 RETURN
      END

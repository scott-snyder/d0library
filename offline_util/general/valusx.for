      SUBROUTINE VALUSX (STR,NUMBER,TYPE,NUMS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract all valid numbers from a string and 
C-                         return values in an array together with 
C-                         number type for each element of the array.
C-                         This routine is like VALUES, but faster.
C-                         However, it will only work correctly on numbers
C-                         delimited by blanks.
C-
C-                         Valid numbers are of type:
C-
C-                         INTEGER, REAL, REAL in E-format, BOOLEAN
C-                         (or LOGICAL) and HOLLERITH.      
C-
C-                         Declarations of the form N*xx are allowed.
C-
C-                         For example 3*10.45
C-
C-   Inputs:   STR         String from which numbers are to be extracted
C-
C-   Outputs : NUMBER(*)   Array of values (given as REAL)
C-             TYPE(*)     Array giving value type:
C-                             1 INTEGER
C-                             2 REAL
C-                             3 REAL in exponential format
C-                             4 BOOLEAN (or LOGICAL)
C-                             5 HOLLERITH
C-                            -1 INVALID number representation
C-             NUMS        Number of values returned
C-
C-   Note : Uses the function VALUEX.
C-
C-   Created  5-JAN-1989   Harrison B. Prosper
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL       ACTIVE
      INTEGER       NUMS
      REAL          NUMBER(*)
      INTEGER       TYPE(*)
      CHARACTER*(*) STR
      INTEGER       CODE,N,I,J,NS,K,II
      REAL          VALUEX,X,Y
C----------------------------------------------------------------------
C
      NS = LEN (STR)
      II = 1
      N  = 0  ! ZERO Item counter
  100 CONTINUE
C
C ****  NOTE: If CODE equals zero then string contains non-numeric characters
C
        X = VALUEX (STR(II:NS),I,J,CODE)
        ACTIVE = CODE .NE. 0
        IF ( ACTIVE ) THEN
          II = II + J
          IF ( STR(II:II) .EQ. '*' ) THEN
            Y = VALUEX (STR(II:NS),I,J,CODE)
            II = II + J
            K = X
            DO 110 I = 1,K  
              N = N + 1
              NUMBER(N) = Y
              TYPE(N) = CODE
  110       CONTINUE
          ELSE
            N = N + 1
            NUMBER(N) = X
            TYPE(N) = CODE
          ENDIF
        ELSE
          ACTIVE = .FALSE.
        ENDIF
      IF ( ACTIVE ) GOTO 100
C
      NUMS = N
  999 RETURN
      END

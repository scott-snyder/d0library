      SUBROUTINE EZZDCD (CHAR1,CHAR2,LENG,KSTART,KEND,KSTEP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decode SRCP parameter. Return parameter name
C-                         in upper case together with its actual length.
C-                         Also for an array return start and end point
C-                         of array index.
C-   Inputs  :
C-             CHAR1       Name of datum or data (CHARACTER string)
C-
C-   Outputs :
C-             CHAR2       Name of parameter in upper case.
C-             LENG        Length of parameter name
C-             KSTART      Lower bound of array index
C-             KEND        Upper bound of array index
C-             KSTEP       Array Step parameter
C-                         The syntax for a sub-range is
C-
C-                         'ARRAY(N:M) K '
C-
C-                         where K is the step size
C-
C-   Controls: None
C-
C-   Created  30-SEP-1988   Harrison B. Prosper
C-   Updated   3-Jan-1996   sss - Compile with g77.
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) CHAR1,CHAR2
      CHARACTER*132  PANAME
      INTEGER LENG,LENARR,TYPE,II,NN,I,J,K,L,N,KSTART,KEND,KSTEP
      REAL    VALUE
C----------------------------------------------------------------------
      ENTRY DCSRCP (CHAR1,CHAR2,LENG,KSTART,KEND,KSTEP)
C----------------------------------------------------------------------
C
C ****  Remove trailing blanks and convert to upper case
C
      L = LEN(CHAR1)
      CALL WORD (CHAR1(1:L),I,J,LENG)
      PANAME = CHAR1(I:J)
      CALL UPCASE(PANAME(1:LENG),PANAME(1:LENG))
C
      KSTART = 1
      KEND   = 0
      KSTEP  = 1  ! Step size
C
C ****  Check for array spec.
C
      LENARR = INDEX (PANAME(1:LENG),'(')
      IF ( LENARR .GT. 0 ) THEN
C
C ****  Get START index
C
        KSTART = VALUE (PANAME(LENARR:LENG),I,L,TYPE)
C
C ****  Get END index
C
        IF ( TYPE .GT. 0 ) THEN
          KEND = VALUE (PANAME(LENARR+L+1:LENG),I,J,TYPE)
          IF ( TYPE .GT. 0 ) THEN
C
C ****  Get STEP size
C
            KSTEP = VALUE (PANAME(LENARR+L+1+J:LENG),I,K,TYPE)
            IF ( KSTEP .EQ. 0 ) KSTEP = 1
          ENDIF
        ENDIF
        LENG = LENARR-1
      ENDIF
      CHAR2 = PANAME(1:LENG)
C
  999 RETURN
      END

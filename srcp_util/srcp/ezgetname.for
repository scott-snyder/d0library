      SUBROUTINE EZGETNAME (LADDR,BKNAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given the address LADDR of an SRCP bank
C-   return the (long) name of that bank.
C-
C-   Inputs  : LADDR    [I]     Address of SRCP bank
C-   Outputs : BKNAME   [C*]    Name of SRCP bank (Maximum of 32 chars.)
C-                              Use EZERR(IER) to check for errors.
C-                              See also EZGET_ERROR_TEXT.
C-   Controls: None
C-
C-   Created  10-MAY-1990   Harrison B. Prosper
C-   Updated  13-JUL-1992   Harrison B. Prosper
C-      Check for CRCP bank
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER LADDR
      CHARACTER*(*) BKNAME
C
      INTEGER I, J, K
      CHARACTER*4  BANK
      CHARACTER*32 BKNAM
      CHARACTER*132 STRING
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
C ****  Clear error flag
C
      ERRSRC = EZS_SUCCESS
C
      IF ( LADDR .GT. 0 ) THEN
C
C ****  Check if this is an SRCP bank
C
        CALL DHTOC (4,IC(LADDR-4),BANK)
C
        IF     ( BANK .EQ.'SRCP' ) THEN
          BKNAM  = ' '
          CALL DHTOC (32,IC(LADDR+JJNAME),BKNAM)
          BKNAME = BKNAM
C
        ELSEIF ( BANK .EQ. 'CRCP' ) THEN
          CALL DHTOC(132,IC(LADDR+1),STRING)
          I = INDEX(STRING,'\START')
          IF ( I .GT. 0 ) THEN
            STRING = STRING(I+6:)
            CALL WORD(STRING,I,J,K)
            BKNAME = STRING(I:J)
          ELSE
            ERRSRC = EZS_NOT_SRCPBANK     ! This s NOT an SRCP bank
          ENDIF
        ELSE
          ERRSRC = EZS_NOT_SRCPBANK     ! This s NOT an SRCP bank
        ENDIF
      ELSE
        ERRSRC = EZS_BAD_ARGUMENT       ! LADDR is zero
        BKNAME = ' '
      ENDIF
C
  999 RETURN
      END

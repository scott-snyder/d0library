      FUNCTION EZHDRC (BANK1,OPTION1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return textual SRCP header information.
C-
C-   Inputs  : BANK1       Name of SRCP bank.
C-             OPTION1     Name of datum required
C-
C-                         'VERSION'          Program version number
C-                         'TIMESTAMP'        Creation date and time
C-
C-   Outputs : None
C-
C-                         Error code:
C-                         0 --- OK
C-                         See EZERR and EZGET_ERROR_TEXT.
C-   Controls: None
C-
C-   Created   9-JAN-1989   Harrison B. Prosper
C-   Updated  15-MAY-1990   Harrison B. Prosper   
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) BANK1,OPTION1
      CHARACTER*1  OPTION
      CHARACTER*32 STRING,EZHDRC
      INTEGER ID,L
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
C
C ****  Clear error flag
C
      ERRSRC = EZS_SUCCESS
C
      L = LEN(BANK1)
      CALL EZZLOC (BANK1(1:L),LSRCP,ID)
      IF ( LSRCP .GT. 0 ) THEN
        ERRSRC = EZS_SUCCESS
        STRING = ' '
C
        CALL UPCASE (OPTION1(1:1),OPTION)
C
        IF     ( OPTION .EQ. 'V' ) THEN
          CALL UHTOC (IC(LSRCP+JJVERS),4,STRING,4)
        ELSEIF ( OPTION .EQ. 'T' ) THEN
          CALL UHTOC (IC(LSRCP+JJTIME),4,STRING,20)
        ELSEIF ( OPTION .EQ. 'N' ) THEN
          CALL UHTOC (IC(LSRCP+JJNAME),4,STRING,32)
        ELSE
          ERRSRC = EZS_BAD_ARGUMENT
        ENDIF
      ELSE
        ERRSRC = EZS_BANK_NOTFOUND
      ENDIF
C
      EZHDRC = STRING
  999 RETURN
      END

      SUBROUTINE EZRNAM (BKNAM1,BKNAM2)
      ENTRY RNSRCP (BKNAM1,BKNAM2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Rename SRCP bank.
C-
C-   Inputs  : BKNAM1      Old name. Up to 32 characters.
C-             BKNAM2      New name
C-   Outputs : None
C-                         Use EZERR to check and return error code
C-                         0 --- OK
C-                         See also EZGET_ERROR_TEXT
C-   Controls: None
C-
C-   Created   3-OCT-1988   Harrison B. Prosper
C-   Updated  15-MAY-1990   Harrison B. Prosper  
C-      Use symbolic constants 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ID,L
      CHARACTER*(*) BKNAM1,BKNAM2
      CHARACTER*32  BKNAME
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:NMSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
C
C       ISRCP is the pointer to address of the
C       selected structure
C
C ****  Clear error flag
C
      ERRSRC = EZS_SUCCESS
C
      CALL EZZLOC (BKNAM1,LSRCP,ID)
      IF ( LSRCP .GT. 0 ) THEN
        ERRSRC = EZS_SUCCESS
        L = LEN(BKNAM2)
        IF ( L .GE. 32 ) L = 32
        CALL UPCASE (BKNAM2(1:L),BKNAME(1:L))
        MSRCP(ID) = BKNAME(1:L)
        CALL UCTOH (MSRCP(ID),IC(LSRCP+JJNAME),4,32) ! Bank name
      ELSE
        ERRSRC = EZS_BANK_NOTFOUND
      ENDIF
C
  999 RETURN
      END

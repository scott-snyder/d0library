      SUBROUTINE EZSHUNT (BKNAME,LSUP,IZLINK)
      ENTRY MVSRCP (BKNAME,LSUP,IZLINK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Move SRCP bank BKNAME from current address
C-                         and hang it beneath the bank whose
C-                         address is given in LSUP. The link bias is
C-                         assumed to be -IZLINK. This routine checks
C-                         to see if link -IZLINK is already occupied
C-                         and signals an error if it is.
C-
C-   Inputs  : BKNAME      Name of SRCP bank to be moved
C-             LSUP        Address of supporting bank
C-             IZLINK      Link from which bank is to be hung
C-
C-   Outputs : NONE        Use EZERR to return error code
C-                         0 ---  OK
C-                         See also EZGET_ERROR_TEXT
C-   Controls: None
C-
C-   Created  23-SEP-1988   Harrison B. Prosper
C-   Modified 13-NOV-1988   Make link an input parameter
C-   Updated  15-MAY-1990   Harrison B. Prosper
C-   Updated  13-JUL-1992   Chip Stewart  - previously EZMOVE 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FOUND
      INTEGER ID,L,N,LSUP,IZLINK
      CHARACTER*(*) BKNAME
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
      IF ( LSUP .LE. 0 ) THEN
        ERRSRC = EZS_BAD_ARGUMENT
        GOTO 999
      ENDIF
C
C ****  Check if link IZLINK is free
C
      IF ( LC(LSUP-IZLINK) .GT. 0 ) THEN
        ERRSRC = EZS_LINK_NOTFREE
        GOTO 999
      ENDIF
C
C ****  Locate bank to be moved
C
      CALL EZZLOC (BKNAME,LSRCP,ID)
      IF ( LSRCP .GT. 0 ) THEN
C
C ****  Move bank and update links in link area
C
        CALL ZSHUNT (IXSTP,LSRCP,LSUP,-IZLINK,0)
        ISRCP = ID
        KSRCP(ISRCP) = LC(LSUP-IZLINK)
        ERRSRC = EZS_SUCCESS
      ELSE
        ERRSRC = EZS_BANK_NOTFOUND
      ENDIF
C
  999 RETURN
      END

      SUBROUTINE EZMOVE (BKNAME,LSUP,IZLINK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Put SRCP under IZLINK of LSUP in zebra structure.
C-                         By default this will put an ASCII copy of SRCP bank
C-                         (CRCP) under the link and leave the original SRCP
C-                         where it was. Subsequent references (EZGETS,
C-                         EZSETS...) to the SRCP bank will still use the 
C-                         original bank and the CRCP bank will be only used 
C-                         for I/O purposes. 
C-                         If the EZMOVE_NOCRCP entry point is called then
C-                         the SRCP (binary) will be ZSHUNTed to the link 
C-                         and subsequent refernences will use the shunted 
C-                         bank. 
C-                         Copy or Shunt SRCP bank BKNAME from current address
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
C-   Updated  15-MAY-1990   Harrison B. Prosper   
C-   Updated  13-JUN-1992   Chip Stewart   - EZCRCP copy to ASCII bank
C-   Updated  13-JUL-1992   Chip Stewart   - ADDED EZMOVE_NOCRCP entry 
C-   Updated  13-JUL-1992   Harrison B. Prosper   
C-      Add SAVE CRCP
C-   
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FOUND,CRCP,OVERWRITE
      INTEGER ID,L,N,LSUP,IZLINK,ISTORE,IER
      CHARACTER*(*) BKNAME
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      SAVE CRCP,OVERWRITE
      DATA CRCP/.TRUE./,OVERWRITE/.FALSE./
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
      IF ((.NOT.OVERWRITE) .AND. (LC(LSUP-IZLINK).GT.0) ) THEN
        ERRSRC = EZS_LINK_NOTFREE
        GOTO 999
      ENDIF
C
C ****  Locate bank to be moved
C
      CALL EZZLOC (BKNAME,LSRCP,ID)
      IF ( LSRCP .GT. 0 ) THEN
        IF (CRCP) THEN
C
C ****  PUT ASCII RCP BANK UNDER LSUP IZLINK
C
          ISTORE = 0  ! ZEBSTP
          CALL EZCRCP (ID,LSUP,IZLINK,ISTORE,IER)
          ERRSRC = IER
        ELSE
C
C ****  Move bank and update links in link area
C
          CALL ZSHUNT (IXSTP,LSRCP,LSUP,-IZLINK,0)
          ISRCP = ID
          KSRCP(ISRCP) = LC(LSUP-IZLINK)
          ERRSRC = EZS_SUCCESS
        END IF
      ELSE
        ERRSRC = EZS_BANK_NOTFOUND
      ENDIF
C
  999 RETURN
      ENTRY EZMOVE_NOCRCP
      CRCP = .FALSE.
 1999 RETURN
      ENTRY EZMOVE_OVERWRITE
      OVERWRITE = .TRUE.
 2999 RETURN
      END

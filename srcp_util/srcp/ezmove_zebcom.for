      SUBROUTINE EZMOVE_ZEBCOM (BKNAME,LSUP,IZLINK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Put SRCP under IZLINK of LSUP in zebra structure.
C-                         This will put an ASCII copy of SRCP bank
C-                         (CRCP) under the link and leave the original SRCP
C-                         where it was. Subsequent references (EZGETS,
C-                         EZSETS...) to the SRCP bank will still use the 
C-                         original bank and the CRCP bank will be only used 
C-                         for I/O purposes. The link bias is
C-                         assumed to be -IZLINK. This routine checks
C-                         to see if link -IZLINK is already occupied
C-                         and signals an error if it is.
C-
C-   Inputs  : BKNAME      Name of SRCP bank to be moved
C-             LSUP        Address of supporting bank in ZEBCOM
C-             IZLINK      Link from which bank is to be hung
C-
C-   Outputs : NONE        Use EZERR to return error code
C-                         0 ---  OK
C-                         See also EZGET_ERROR_TEXT
C-   Controls: None
C-
C-   Created  23-SEP-1988   as EZMOVE Harrison B. Prosper
C-   Updated  15-MAY-1990   Harrison B. Prosper   
C-   Updated  13-JUN-1992   Chip Stewart   - EZCRCP copy to ASCII bank
C-   Updated  13-JUL-1992   Harrison B. Prosper   
C-   Renamed and updated to copy to ZEBCOM  9-SEP-1992   Andrew Brandt 
C_   ISTORE=1 (ZEBCOM bank) LC to LQ for copying to ZEBCOM  

C-   Updated  18-JAN-1993   Vipin Bhatnagar (saving second scan in CRCP)
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FOUND
      INTEGER ID,L,N,LSUP,IZLINK,ISTORE,IER
      CHARACTER*(*) BKNAME
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
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
C ****  Locate bank to be moved
C
      CALL EZZLOC (BKNAME,LSRCP,ID)
      IF ( LSRCP .GT. 0 ) THEN
C
C ****  PUT ASCII RCP BANK UNDER LSUP IZLINK
C
        ISTORE = 1  ! ZEBCOM
        CALL EZCRCP (ID,LSUP,IZLINK,ISTORE,IER)
        ERRSRC = IER
      ELSE
        ERRSRC = EZS_BANK_NOTFOUND
      ENDIF
C
  999 RETURN
      END

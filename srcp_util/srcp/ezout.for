      SUBROUTINE EZOUT (LUNOUT,BKNAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Save named SRCP bank to disk.
C-                         Use EZIN to fetch it back into memory.
C-
C-   Inputs  : LUNOUT      Unit number of output stream
C-             BKNAME      Name of structure to be saved
C-   Outputs : None
C-   Controls: None
C-
C-   Created  20-NOV-1988   Harrison B. Prosper
C-   Updated   3-Jan-1996   sss - Compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER      LUNOUT,I,J,L,N
      CHARACTER*(*) BKNAME
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      ENTRY      SVSRCP(LUNOUT,BKNAME)
C----------------------------------------------------------------------
C
C ****  Write SRCP bank to file
C
      L = LEN (BKNAME)
      CALL EZZLOC (BKNAME(1:L),LSRCP,N) ! Get bank address
      IF ( LSRCP .GT. 0) THEN
        CALL FZOUT (LUNOUT,IDVSTP,LSRCP,1,' ',1,0,0)
        ERRSRC = EZS_SUCCESS
      ELSE
        ERRSRC = EZS_BANK_NOTFOUND
      ENDIF
C
  999 RETURN
      END

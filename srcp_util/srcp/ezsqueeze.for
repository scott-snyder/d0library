      SUBROUTINE EZSQUEEZE(NWORDS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compress the currently selected RCP bank
C-   to the smallest possible size.
C-
C-   Inputs  : None
C-   Outputs : NWORDS [I]   Number of freed up words
C-   Controls: 
C-
C-   Created   8-MAR-1994   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NWORDS
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      INTEGER OLDSIZE, NEWSIZE, WRDIDS, NIDS, NVAL
      INTEGER IPTI,IPTO,IPTV,IPTT
C----------------------------------------------------------------------
      NWORDS = 0
C
C ****  Clear error flag
C
      ERRSRC = EZS_SUCCESS
C
      IF ( ISRCP .LE. 0 ) THEN
        ERRSRC = EZS_BANK_NOTSELECTED
        GOTO 999
      ENDIF
C
C ****  Get base pointers to data within RCP bank
C
      CALL EZZGPT (LSRCP,IPTI,IPTO,IPTV,IPTT)
C
C ****  Get current bank size
C
      OLDSIZE = IC(LSRCP-1)
C
C ****  Compute minimum required size of RCP bank
C
      WRDIDS= IC(LSRCP+JJNWRD)                  ! Number of fullwords/identifier
      NIDS  = IC(LSRCP+JJIDS)                   ! Number of IDS
      NVAL  = IC(LSRCP+JJVAL)                   ! Number of VALUES
C
      NEWSIZE = (JJBASE-1) + (WRDIDS+1)*NIDS + 2*NVAL
      IF ( NEWSIZE .LT. OLDSIZE ) THEN
        NWORDS = OLDSIZE - NEWSIZE
        CALL MZPUSH(IXSTP,LSRCP,0,-NWORDS,'I')
      ENDIF
  999 RETURN
      END

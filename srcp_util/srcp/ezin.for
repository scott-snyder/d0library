      SUBROUTINE EZIN (LUNIN,BKNAME)
      ENTRY    FTSRCP (LUNIN,BKNAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fetch an SRCP bank from unit LUNIN and give
C-                         it the name in BKNAME, if BKNAME is NOT blank;
C-                         otherwise use the name in the header part of
C-                         the SRCP bank. If this bank is at the head of
C-                         a chain of SRCP banks then ALL SRCP banks
C-                         within the chain will be declared automatically
C-                         to the EZ routines.
C-
C-   Inputs  : LUNIN       Logical unit of input stream
C-             BKNAME      ' ' or name to give to Fetched bank plus
C-                         the OPTIONAL qualifier /DROP.
C-                         If /DROP qualifier is used the old rcp-bank
C-                         is dropped and replaced with the new rcp-bank.
C-                         If a name is NOT given then name in the
C-                         header of the RCP-bank is used.
C-   Outputs : None
C-   Controls: None
C-
C-   Created  21-NOV-1988   Harrison B. Prosper
C-   Updated  23-MAY-1990   Harrison B. Prosper
C-      Use EZGET_ERROR_TEXT
C-   Updated   6-MAY-1991   Harrison B. Prosper
C-      Implement optional dropping of old rcp-bank using /DROP
C-      qualifier
C-   Updated  22-OCT-1991   Harrison B. Prosper, Jan Guida
C-    Fix overwrite of first bank name by the last bank name
C-   Updated  24-OCT-1991   Harrison B. Prosper, Jan Guida
C-    Preserve ISRCP pointer
C-   Updated  13-JAN-1992   Harrison B. Prosper   
C-    Fix first bank name bug
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL      EZERROR
      INTEGER      LUNIN,I,J,L,N,IER,NN,LAST_ISRCP
      CHARACTER*(*) BKNAME
      CHARACTER*32 BANK_NAME,FIRST_BANK_NAME
      CHARACTER*132  STRING
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSRCP.LINK'
C----------------------------------------------------------------------
C
C ****  Note currently picked bank
C
      LAST_ISRCP = ISRCP
C
C ****  Read in next SRCP bank as a standalone bank
C       to address LSRCP
C
      CALL FZIN (LUNIN,IDVSTP,LSRCP,2,' ',0,0)
C
C ****  Protect link
C
      KSRCP(MXSRCP) = LSRCP
C
C ****  Check for /DROP qualifier
C
      L = LEN (BKNAME)
      STRING = BKNAME(1:L)
      CALL UPCASE(STRING(1:L),STRING(1:L))
      I = INDEX(STRING(1:L),'/D')
C
C ****  Drop old rcp-banks if requested
C
      NN = 0                                    ! Count banks
      IF ( I .GT. 0 ) THEN                      ! Drop bank
C
C ****  DROP BANKS if PRESENT
C
        DO WHILE ( LSRCP .GT. 0 )
          NN = NN + 1
C
          IF ( I .EQ. 1 ) THEN                  ! Blank name
            CALL EZGETNAME(LSRCP,BANK_NAME)     ! Get name from header
          ELSE
            BANK_NAME = BKNAME(1:I-1)           ! Strip off /DROP
            I = 1                               ! Use EZGETNAME next time around
          ENDIF
C
          KSRCP(MXSRCP-1) = LSRCP               ! Protect link
          CALL EZDROP(BANK_NAME)                ! Drop old RCP-bank
          LSRCP = KSRCP(MXSRCP-1)               ! Restore link
          LSRCP = LC(LSRCP-IZSRCP)              ! Look for next RCP bank
          IF ( NN .EQ. 1 ) THEN
            FIRST_BANK_NAME = BANK_NAME
          ENDIF
        ENDDO
      ELSE
        BANK_NAME = BKNAME(1:L)
        FIRST_BANK_NAME = BANK_NAME
      ENDIF
C
C ****  Declare new RCP-bank at address LSRCP
C
      LSRCP = KSRCP(MXSRCP)             ! Restore original link
      CALL EZNAME (FIRST_BANK_NAME,LSRCP,0)
      IF ( EZERROR  (IER) ) THEN
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL ERRMSG ('RCPERR','EZIN',STRING,'W')
      ENDIF
C
C ****  Restore last ISRCP pointer
C
      ISRCP = LAST_ISRCP
  999 RETURN
      END

      SUBROUTINE EZUNNAME(BKNAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Undeclare an rcp bank to the Ez system.
C-   the specified SRCP bank. EZPICK and
C-   EZRSET is called internally. If the specified SRCP bank is at
C-   the head of a chain of SRCP banks then all banks within that
C-   chain will be dropped.
C-
C-   Inputs  : BKNAME   [C*]    Name of RCP bank.
C-                              Use EZERR(IER) to check for errors:
C-                              0 --- OK
C-                             -1 --- Bank not found
C-   Outputs : None
C-   Controls: None
C-
C-   Updated   9-SEP-1992   Rajendran Raja  based on EZDROP 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) BKNAME
C
      CHARACTER*32  BANK
      INTEGER I,J,L,IER,ID,LADDR
      LOGICAL EZERROR
C
      INCLUDE 'D0$LINKS:IZSRCP.LINK'
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:NMSRCP.INC'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
C
      L = LEN(BKNAME)                   ! get length
      BANK = BKNAME(1:L)
      CALL EZPICK_ERRORS(0)             ! Suppress EZPICK messages
      CALL EZPICK(BANK)
      CALL EZPICK_ERRORS(-1)            ! Re-activate them 
C
      IF ( EZERROR(IER) ) THEN
        GOTO 999
      ELSE
C
C ****  Reset the address in the link area to zero to signify
C ****  that a bank has been dropped.
C
        LSRCP = KSRCP(ISRCP)            ! Get first bank address
        KSRCP(ISRCP) = 0                ! Dropped
        NSRCP = NSRCP - 1               ! Decrement RCP bank count
C
C ****  This may be a chain of SRCP banks; loop over chain
C
        LADDR = LC(LSRCP-IZSRCP)
        DO WHILE ( LADDR .GT. 0 )
          CALL EZGETNAME(LADDR,BANK)
          IF ( .NOT. EZERROR(IER) ) THEN
            CALL EZZLOC(BANK,L,ID)
            IF ( ID .GT. 0 ) THEN
              KSRCP(ID) = 0               ! Dropped
              NSRCP = NSRCP - 1           ! Decrement RCP bank count
            ENDIF
          ENDIF
          LADDR = LC(LADDR-IZSRCP)
        ENDDO
C
C ****  Now drop SRCP bank and its siblings
C
C        CALL MZDROP (IXSTP,LSRCP,' ')   ! Drop SRCP bank(s)
        CALL EZRSET
      ENDIF
  999 RETURN
      END

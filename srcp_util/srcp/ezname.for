      SUBROUTINE EZNAME (BKNAME,LSUPP,IZLINK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-      Declare an SRCP bank to the RCP package. If the SRCP bank is part
C-      of a tree of banks then LSUPP should be the address of the support
C-      bank and IZLINK the link from which the SRCP bank hangs. If the
C-      SRCP bank is a stand-alone bank then IZLINK MUST be set to zero
C-      and LSUPP is then taken to be the address of the SRCP bank.
C-      If BKNAME is ' ' then the SRCP bank will be declared with the name
C-      given in the header, otherwise it is given the name in BKNAME.
C-
C-                         IMPORTANT: This routine should be
C-                         called to declare an SRCP bank to
C-                         the RCP package, otherwise the bank
C-                         will be inaccessible.
C-
C-   Inputs  : BKNAME      Name to assign to bank (up to 32 chars.)
C-             LSUPP       Address of SRCP support bank if IZLINK > zero;
C-                         otherwise it is the address of the SRCP bank
C-                         itself.
C-
C-   Outputs : LSUPP       Changed only if IZLINK = 0. Then it will be
C-                         address of the new stand-alone SRCP bank.
C-
C-                         Error codes
C-                         0 --- OK
C-                         See EZERR and EZGET_ERROR_TEXT.
C-   Controls: IZLINK
C-
C-   Created   4-OCT-1988   Harrison B. Prosper
C-   Modified 21-NOV-1988   Major change: Uses New SRCP bank format
C-   Updated  10-MAY-1990   Harrison B. Prosper
C-      Make compatible with EZDROP
C-   Updated   6-JUN-1990   Harrison B. Prosper
C-      Pad BKNAM1 with blanks
C-   Updated  22-MAR-1991   Harrison B. Prosper
C-      IF BKNAME is not blank then update header
C-   Updated   8-MAY-1991   Harrison B. Prosper
C-      NAME chain of banks
C-   Updated  13-JUN-1992   Chip Stewart   - CHECK for ASCII RCP bank (CRCP)
C-   Updated   3-AUG-1992   Chip Stewart   - redo \START line, VAXELN block
C-   Updated   4-Sep-1993   Herbert Greenlee
C-    Added memory-to-memory option (EZ_PUT_FIFO/EZ_GET_FIFO).
C-   Updated   3-Jan-1996   sss - Compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) BKNAME
      INTEGER LSUPP
      INTEGER IZLINK
C
      INTEGER ID,I,J,L,LNAME,IER,LSTART
      CHARACTER*4   BANK
      CHARACTER*32  BKNAM1
      LOGICAL EZERROR,OK,AUX
C
      INTEGER       NBANKS,NMAX
      PARAMETER( NMAX = 50 )            ! Maximum number of banks/file
      CHARACTER*32  BKNAMES(NMAX)
      INTEGER RECSIZ                    ! Record size in FULL-words
      SAVE RECSIZ
      DATA RECSIZ/20/                   ! Full line
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$LINKS:IZSRCP.LINK'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:NMSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      ENTRY      NASRCP (BKNAME,LSUPP,IZLINK)
C----------------------------------------------------------------------
C
C ****  Clear error flag
C
      ERRSRC = EZS_SUCCESS
C
C ****  Create a permanent link area
C
      CALL EZZCLK
C
C ****  Check address
C
      IF ( LSUPP.LE. 0 ) THEN
        ERRSRC = EZS_BAD_ARGUMENT
        GOTO 999
      ENDIF
C
      IF ( IZLINK .GT. 0 ) THEN
        LSRCP = LC(LSUPP-IZLINK)
      ELSE
        LSRCP = LSUPP
      ENDIF
C
      IF ( LSRCP.LE. 0 ) THEN
        ERRSRC = EZS_BANK_NOTFOUND
        GOTO 999
      ENDIF
C
C ****  Search for bank name if it is not blank
C
      L = LEN (BKNAME)
      IF ( L .GT. 32 ) L = 32
      CALL WORD (BKNAME(1:L),I,J,LNAME)
      IF ( LNAME .GT. 0 ) THEN
        CALL EZZLOC (BKNAME(1:L),I,ID)
      ELSE
        ID = 0                          ! Blank name
      ENDIF
C
      IF ( ID .LE. 0 ) THEN
C
C ****  This may be a chain of SRCP banks; loop over chain
C
        DO WHILE ( LSRCP .GT. 0 )
C
C ****  Check number of declared SRCP banks
C
          IF ( NSRCP .LT. MXSRCP ) THEN
C
C ****  Get name of bank at address LSRCP
C
            CALL UHTOC (IC(LSRCP-4),4,BANK,4)
C
            IF ( BANK .EQ.'SRCP' ) THEN
C
              IF ( LNAME .GT. 0 ) THEN
                LNAME = 0             ! Use internal name next time around
                CALL UPCASE (BKNAME(1:L),BKNAM1(1:L))
                BKNAM1 = BKNAM1(1:L)//
     &              '                                   '
              ELSE
                CALL EZGETNAME (LSRCP,BKNAM1)
              ENDIF
C
C ****  Make sure bank name is unique
C
              CALL EZZLOC (BKNAM1,I,ID)
              IF ( ID .LE. 0 ) THEN

                CALL EZZNID (ISRCP)       ! Get next available ID
                IF ( .NOT. EZERROR(IER) ) THEN
C
C ****  OK; declare to RCP
C
                  MSRCP(ISRCP) = BKNAM1
                  KSRCP(ISRCP) = LSRCP
                  CALL UCTOH (MSRCP(ISRCP),IC(LSRCP+JJNAME),4,32) ! Bank name
                ELSE
                  ERRSRC = EZS_TOOMANY_BANKS  ! Not enough room to 
                                              ! name this bank
                ENDIF
              ELSE
                ERRSRC = EZS_DUPLICATE_BANK     ! Name already in use
              ENDIF
C
C ****  CHECK for ASCII CRCP bank 
C
            ELSE IF ( BANK .EQ.'CRCP' ) THEN
C&IF VAXELN
C&            CALL ERRMSG('CRCP_FOUND','EZNAME',
C&     &         'NO CRCP IN LEVEL2/ELN EXPECTED','F')
C&ELSE
              LSTART = 1
              IF ( LNAME .GT. 0 ) THEN
                LNAME = 0             ! Use internal name next time around
                CALL UPCASE (BKNAME(1:L),BKNAM1(1:L))
                BKNAM1 = BKNAM1(1:L)//
     &              '                                   '
                CALL EZ_PUT_FIFO('\START '//BKNAM1)
                LSTART = -1
              END IF
C
C ****  Make sure bank name is unique
C
              CALL PRCRCP(-1, LSRCP, 0, 'ALL', LSTART )
C
C ****  Read file and split into SRCP banks
C
              CALL EZMAKE (-1,RECSIZ,BKNAMES,NBANKS)
              AUX = EZERROR(IER)                ! Get EZ error code
C&ENDIF
            ELSE
              ERRSRC = EZS_NOT_SRCPBANK
            ENDIF
          ELSE
            ERRSRC = EZS_TOOMANY_BANKS  ! Not enough room to name this bank
          ENDIF
          LSRCP = LC(LSRCP-IZSRCP)
        ENDDO
      ELSE
        ERRSRC = EZS_DUPLICATE_BANK     ! Name already in use
      ENDIF
C
  999 RETURN
      END

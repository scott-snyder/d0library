      SUBROUTINE EZBOOK (BKNAME,WRDIDS,NUMIDS,NUMVAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create a stand-alone SRCP-type bank . Use
C-                         EZMOVE to move the SRCP bank about in store
C-                         /ZEBSTP/. Use EZPICK to select one of possibly
C-                         many SRCP banks so that subsequent calls to
C-                         EZxxxx routines will refer to the selected bank.
C-
C-   Inputs  : BKNAME      Name assigned to SRCP bank. Can
C-                         be up to 32 characters long.
C-             WRDIDS      Number or words per record
C-             NUMIDS      Number of records per SRCP bank
C-             NUMVAL      Number of values per SRCP bank
C-
C-   Outputs : NONE        Error codes returned by logical function
C-                         EZERR (IER). See also EZERR_REPORT(IER,STRING)
C-                         0 --- OK
C-   Controls: None
C-
C-   Created  23-SEP-1988   Harrison B. Prosper
C-   Modified 12-NOV-1988   Harrison B. Prosper
C-                          Changed to NEW SRCP bank format
C-   Updated   7-AUG-1989   Harrison B. Prosper
C-   All SRCP-type banks are again called SRCP
C-   Updated  10-MAY-1990   Harrison B. Prosper
C-      Make compatible with EZDROP
C-   Updated  14-APR-1991   Harrison B. Prosper
C-      Issue warning only if bank exists already
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) BKNAME
      INTEGER       WRDIDS
      INTEGER       NUMIDS
      INTEGER       NUMVAL
C
      LOGICAL FIRST,NEW,WIPED
      INTEGER       I,J,K,L,N
      INTEGER       NL,NS,ND,IOD,NZERO,ID
      REAL          VERS,EZVERS
      CHARACTER*4   VERSN,BANK
      CHARACTER*20  TIMEST
      CHARACTER*32  UCNAME
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:NMSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      CHARACTER*(CHRCRD)  REMARK
C----------------------------------------------------------------------
C
C ****  Clear error flag
C
      ERRSRC = EZS_SUCCESS
C
C ****  Declare a permanent link area to ZEBRA for SRCP banks
C
      CALL EZZCLK
C
C ****  If the addresses of ALL the banks are ZERO then the division
C       has been wiped with MZWIPE. In that case set NSRCP = 0
C
        WIPED = .FALSE.
        DO 10 I =  1,MXSRCP
          IF ( KSRCP(I) .GT. 0 ) GOTO 20
   10   CONTINUE
        WIPED = .TRUE.
   20   CONTINUE
        IF ( WIPED ) NSRCP = 0
C
C ****  Check if there is enough room in link area
C ****  to create a new bank
C
      IF ( NSRCP .GE. MXSRCP  ) THEN
        ERRSRC = EZS_TOOMANY_BANKS
        GOTO 999
      ENDIF
C
C ****  Check if named SRCP bank exists
C
      CALL EZZLOC (BKNAME,LSRCP,ID)
      NEW = LSRCP .LE. 0
C
      IF ( NEW ) THEN
        L = LEN(BKNAME)
        CALL WORD(BKNAME(1:L),I,J,N)
        UCNAME = BKNAME(I:J)
        CALL UPCASE(UCNAME(1:N),UCNAME(1:N))
C
C ****  Some slots may be empty because of previously dropped
C ****  banks. Find next available slot for this bank
C
        CALL EZZNID (ISRCP)
C
C ****  Build a stand-alone SRCP bank
C
        NL  =  1 ! Total number of links
        NS  =  1 ! Number of structural links
        ND  =  JJBASE-1 + (WRDIDS+1)*NUMIDS + 2*NUMVAL  ! Bank length
        IOD =  1 ! MIXED data type
        NZERO = 0                       ! Preset words to zero
C
        BANK = 'SRCP'                   ! All banks called SRCP
        CALL MZBOOK (IDVSTP,LSRCP,0,2,BANK,NL,NS,ND,IOD,NZERO)
        KSRCP(ISRCP) = LSRCP            ! Store address in link area
        MSRCP(ISRCP) = UCNAME           ! Store bank name
C
C ****  Pack HEADER block
C
        CALL UCTOH ('NEW ',IC(LSRCP+JJFLAG),4,4)        ! New SRCP format
C
        VERS = EZVERS()                                 ! Get Version number
        WRITE(UNIT=VERSN,FMT='(''V'',F3.1)') VERS
        CALL UCTOH (VERSN, IC(LSRCP+JJVERS),4,4)        ! Program Version number
C
        CALL CDTIME (TIMEST) ! Get Creation Date and Time
        CALL UCTOH (TIMEST,IC(LSRCP+JJTIME),4,20)       ! Time stamp
C
        CALL UCTOH (UCNAME,IC(LSRCP+JJNAME),4,32)       ! Bank name
C
        IC(LSRCP+JJNIDS) = NUMIDS
        IC(LSRCP+JJNVAL) = NUMVAL
        IC(LSRCP+JJNWRD) = WRDIDS
        IC(LSRCP+JJPIDS) = JJBASE                     ! Pointer to identifiers
        IC(LSRCP+JJPORD) = IC(LSRCP+JJPIDS)+WRDIDS*NUMIDS ! Pointer to ord. list
        IC(LSRCP+JJPVAL) = IC(LSRCP+JJPORD)+NUMIDS    ! Pointer to values
        IC(LSRCP+JJPTYP) = IC(LSRCP+JJPVAL)+NUMVAL    ! Pointer to value type
      ELSE
        ERRSRC = EZS_DUPLICATE_BANK ! SRCP bank already exists
        REMARK =  'The bank '//BKNAME(1:LEN(BKNAME))//' exists already'
        CALL ERRMSG('DUPLICATE_BANK','EZBOOK',REMARK,'W')
      ENDIF
C
  999 RETURN
      END

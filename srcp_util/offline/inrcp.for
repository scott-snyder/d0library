      SUBROUTINE INRCP (FILNAM,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in an .RCP control file using EZMAKE.
C-                         Each \START .. \STOP block in the file will
C-                         be directed to a different SRCP bank whose
C-                         name is specified in the \START statement.
C-                         Note: INRCP creates stand-alone SRCP banks.
C-                         Use EZPICK to select an SRCP bank, and use
C-                         EZMOVE to move a selected bank into some ex-
C-                         isting structure. If only one \START .. \STOP
C-                         block exists in the file then only one bank
C-                         will be created. There should be no more than
C-                         20 \START .. \STOP blocks per file.
C-
C-   Inputs  : FILNAM [C*]      Name of control file
C-
C-   Outputs : IERR   [I]       0 - OK
C-
C-   Controls: None
C-
C-   Created  15-MAR-1989   Harrison B. Prosper
C-   Updated  13-APR-1989   Harrison B. Prosper   
C-   Updated  11-JUL-1991   Harrison B. Prosper   
C-      Return EZ - ERRORs
C-   Updated  15-NOV-1993   Stan M. Krzywdzinski
C-      Added propagation of input value error, ERRVAL, from EZZSTO 
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) FILNAM
      INTEGER       IERR                ! Error return code
C----------------------------------------------------------------------
      INTEGER       NBANKS,NMAX
      PARAMETER( NMAX = 1000 )            ! Maximum number of banks/file
      CHARACTER*32  BKNAME(NMAX)
      LOGICAL OK,SWITCH
      INTEGER L,I,IER
      INTEGER LUNIN                     ! Logical unit number for input
      INTEGER PROGID                    ! Program identifier
      PARAMETER( PROGID = 80 )
C
      CHARACTER*(*) LIST(*)
      INTEGER NLIST
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      INTEGER RECSIZ                    ! Record size in FULL-words
      SAVE RECSIZ
      DATA RECSIZ/20/                   ! Full line
C----------------------------------------------------------------------
C
      ERRVAL = 0                      ! Initialize input value error
      VARNAME = ' '                   ! and variable name
C
      CALL INZSTP                     ! Initialize ZEBSTP
C
C ****  Open the requested file
C
      CALL GTUNIT(PROGID,LUNIN,IERR)
      L = LEN (FILNAM)
      CALL D0OPEN (LUNIN,FILNAM(1:L),'I',OK)
      IF ( OK ) THEN
        IERR = 0
      ELSE
        IERR =-1
        GOTO 999
      ENDIF
C
C ****  Read file and split into SRCP banks
C
      CALL EZMAKE (LUNIN,RECSIZ,BKNAME,NBANKS)
      CALL EZERROR(IERR)                ! Get EZ error code
      IF (IERR .EQ. 0) THEN
        IERR = ERRVAL
      ENDIF
C
C ****  Close and release unit number
C
      CLOSE (UNIT=LUNIN)
      CALL RLUNIT(PROGID,LUNIN,IER)
      RETURN
C
C ****  Provide list of banks
C
      ENTRY INRCP_LIST (LIST,NLIST)
      DO I =  1,NBANKS
        LIST(I) = BKNAME(I)
      ENDDO
      NLIST = NBANKS
      RETURN
C
C ****  Provide compressed form of bank
C
      ENTRY INRCP_COMPRESS_BANK
      RECSIZ = 8                        ! 32 characters
      RETURN
C
C ****  Use this entry point to suppress/activate abort on bad value
C
      ENTRY INRCP_ABORT(SWITCH)
      CALL EZ_ABORT_ON_BAD_VALUE(SWITCH)
  999 RETURN
      END

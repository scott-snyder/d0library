      SUBROUTINE INRCPE (FILNAM,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in an .RCP edit file using EZMAKE and
C-                         perform edits on the corresponding .RCP
C-                         banks.
C-
C-   Inputs  : FILNAM [C*]      Name of RCP EDIT file
C-
C-   Outputs : IERR   [I]       ZERO if open was succesful
C-
C-   Controls: None
C-
C-   Created  13-APR-1989   Harrison B. Prosper
C-   Updated   3-MAY-1993   Harrison B. Prosper
C-    Improve
C-   Updated  23-MAY-1994   sss
C-    Must resort the bank after each addition...
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) FILNAM
      INTEGER       IERR                ! Error return code
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
C----------------------------------------------------------------------
      INTEGER PROGID                    ! Program identifier
      INTEGER MAXBUF
      PARAMETER( PROGID = 80 )
      PARAMETER( MAXBUF = 200)
      INTEGER    NBANKS, NMAX
      PARAMETER( NMAX = 20 )            ! Maximum number of banks/file
C
      CHARACTER*32  BKNAME, BKNAME_EDIT(NMAX)
      CHARACTER*32  PARAM
      CHARACTER*(CHRCRD)  BUFFER(MAXBUF)
      LOGICAL OK, UPDATED
      LOGICAL EZERROR                   ! EZ-error check
      INTEGER EZHDRI                    ! EZ-return header info
      INTEGER NUMIDS
      INTEGER LUNIN                     ! Logical unit number for input
      INTEGER I,J,K,L,N,II,JJ,NN
      INTEGER ITYPE,SIZE,NBUF,ADDRESS,NUPDATE
C----------------------------------------------------------------------
C
      UPDATED = .FALSE.
C
      CALL INZSTP             ! Initialize ZEBSTP
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
      CALL EZMAKE (LUNIN,WRDCRD,BKNAME_EDIT,NBANKS)
C
C ****  Close and release unit number
C
      CLOSE (UNIT=LUNIN)
      CALL RLUNIT(PROGID,LUNIN,IERR)
C
C ****  Loop over edit banks and perform edits on RCP banks
C
      IF ( NBANKS .GT. 0 ) THEN
C
        DO I =  1, NBANKS
C
          CALL WORD (BKNAME_EDIT(I),II,JJ,NN)   ! Get actual length of name
          BKNAME = BKNAME_EDIT(I)(1:NN-1)       ! Strip off E
C
          CALL EZLOC(BKNAME,ADDRESS)
C
          IF ( ADDRESS .GT. 0 ) THEN            ! Check if RCP bank exists
C
            CALL EZPICK(BKNAME_EDIT(I))         ! Select RCP EDIT bank
C
            NUMIDS = EZHDRI(BKNAME_EDIT(I),'IDENTIFIERS') ! Get number of IDs
C
            NUPDATE = 0
C
            DO J =  1,NUMIDS                              ! Loop over IDS
              CALL EZGETT (J,PARAM,L,ITYPE,SIZE)          ! Get id name
C
              IF ( ITYPE .GT. 0 ) THEN                    ! Ignore comments
C
                NUPDATE = NUPDATE + 1
C
                CALL EZFETCH(PARAM,MAXBUF,NBUF,BUFFER,IERR) ! Get parameter
                CALL EZPICK(BKNAME)                       ! Select RCP bank
                CALL EZDELETE(PARAM,IERR)                 ! Delete Parameter
                CALL EZADD(BUFFER,NBUF,IERR)              ! Add Parameter
                CALL EZEND                                ! resort id list
                CALL EZRSET                               ! Reset to Edit bank
              ENDIF
            ENDDO
C
            CALL EZRSET
C
            IF ( NUPDATE .GT. 0 ) THEN
              UPDATED = .TRUE.
            ENDIF
          ENDIF
C
        ENDDO
      ENDIF
C
      IF ( UPDATED ) THEN
        IERR = 0
      ELSE
        IERR =-2
      ENDIF
C
  999 RETURN
      END

      SUBROUTINE D0DAD_CATCHECK_ACTIVE(FCLUN,ECLUN,FILENAME,IOSTRING)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Do dianosis of ACTIVE, UNUSED files in a 
C-      D0DAD event- and file catalog pair.
C-
C-   Inputs  : FCLUN    - Logical unit of file catalog
C-             ECLUN    - Logical unit of event catalog
C-             IR       - Run number
C-             FILENAME - The name of the Active, Unused file
C-             IOSTRING - Initial error message.  Finish and print here.
C-   Outputs :
C-   Controls:
C-
C-   Created  28-OCT-1996   John Hobbs
C-   Updated  18-MAR-2004   sss - compile with g77
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:zebcom.inc'
      INCLUDE  'D0$INC:quest.inc'
C- Input/Output parameters
      INTEGER FCLUN,ECLUN
      CHARACTER*(*)   IOSTRING,FILENAME
C- External functions
      INTEGER LENOCC,ICFILA
      LOGICAL LIB$FIND_FILE
C- Array sizes
      INTEGER NEVMAX,NHMAX,NFIDMAX
      PARAMETER(NEVMAX=40000,NHMAX=1000,NFIDMAX=5)
C- Temporary locals
      INTEGER FLEN,CONTXT,FILESIZE,NFOUND,NLOST,NEV,RUN,EVENT,MASK(2)
      INTEGER IDUM,IERR,I,J,NHEAD,IXWIPE,ZEBLUN,IFID,NFIDS
      INTEGER EVENTS(NEVMAX),IUHEAD(NHMAX),FIDLIST(NFIDMAX)
      LOGICAL LOK,MATCHED
      CHARACTER*256 LOCALFILE,chopt*2,FIDFILE(NFIDMAX)*80,CTEMP*200
C-----------------------------------------------------------------------

C- Define local coefficients

C&IF VAXVMS
      CHOPT='XI'
C&ELSE
C&      CHOPT='LI'
C&ENDIF

C- Parse the run number from the file name (must be standard D0 format)

      RUN=0
      LOCALFILE=' '
      CALL FILENAME_PARSE(FILENAME,'NAM',LOCALFILE,I)
      I=I-3
      J=ICFILA('_',LOCALFILE,1,I)+1
      READ(LOCALFILE(J:I),*) RUN
C
C- First find the file...
C
      CONTXT=0
      FLEN = LENOCC(FILENAME)
      IF( .NOT.LIB$FIND_FILE(FILENAME(1:FLEN),LOCALFILE,CONTXT) ) THEN
        WRITE(*,1001) IOSTRING(1:LENOCC(IOSTRING)),'NOT ON DISK'
 1001   FORMAT(' ',A,' ',A)
        CALL LIB$FIND_FILE_END(CONTXT)
        GOTO 999
      ENDIF
      CALL LIB$FIND_FILE_END(CONTXT)
C
C- Does it have any events?
C
      FLEN=LENOCC(LOCALFILE)
      IF( FILESIZE(LOCALFILE(1:FLEN)).EQ.0 ) THEN
        WRITE(*,1001) IOSTRING(1:LENOCC(IOSTRING)),'EMPTY FILE'
        GOTO 999
      ENDIF
C
C- Has events, is on disk.  Why not referenced?
C-   Are the events it contains actually referenced in the EC by another
C-   file?   Read the list of events in this file.   See if they are in
C-   the EC and count yes/no.  This gives a "missing event" count.
C
C-   Get the list of events in the file
      CALL GTUNIT(567,ZEBLUN,IERR)
      CALL D0OPEN(ZEBLUN,LOCALFILE(1:FLEN),'IX',LOK)
      IF( .NOT.LOK ) THEN
        WRITE(*,8001)
 8001   FORMAT(' D0DAD_CATCHECK_ACTIVE: Error from D0OPEN')
        GOTO 999
      ENDIF
      CALL FZFILE(ZEBLUN,8190,CHOPT)
      IF( IQUEST(1).NE.0 ) THEN
         WRITE(*,8002) IQUEST(1)
 8002    FORMAT(' D0DAD_CATCHECK_ACTIVE: Error ',I4,'from FZFILE')
         GOTO 999
      ENDIF
      NEV=0
      IXWIPE=0
 10   CONTINUE
        NHEAD=NHMAX
        CALL MZWIPE(IXWIPE)
        CALL FZIN(ZEBLUN,IXMAIN,0,0,'S',NHEAD,IUHEAD)
        IERR=IQUEST(1)
        IF( IERR.NE.0 .AND. IERR.NE.1 .AND. IERR.NE.2 ) GOTO 20  ! Done w/file
        IF( IERR.EQ.0 ) THEN    
          IF( NEV.LT.NEVMAX ) THEN                 
            NEV=NEV+1
            EVENTS(NEV)=IUHEAD(9)
          ELSE
            WRITE(*,*) ' D0DAD_CATCHECK_ACTIVE: Too many events in file'
            GOTO 20
          ENDIF
        ENDIF
        GOTO 10
 20   CONTINUE
      CALL FZENDI(ZEBLUN,'TX')
      CLOSE(ZEBLUN)
      CALL RLUNIT(567,ZEBLUN,IERR)
C-   Search the EC for these events
      NLOST=0
      NFOUND=0
      NFIDS=0
      CALL VZERO(FIDLIST,NFIDMAX)
      DO I=1,NFIDMAX
        FIDLIST(I)=4H    
      ENDDO
      DO I=1,NEV
        CALL ECGET(ECLUN,RUN,EVENTS(I),MASK,IFID,IDUM,IDUM,IERR)
        IF( IERR.EQ.0 ) THEN   ! Event references different file
          NFOUND=NFOUND+1
          J=1
          MATCHED=.FALSE.
          DO WHILE( .NOT.MATCHED .AND. J.LE.NFIDS )
            IF( IFID.EQ.FIDLIST(J)) MATCHED=.TRUE.
            J=J+1
          ENDDO
          IF( .NOT.MATCHED .AND. NFIDS.LT.NFIDMAX ) THEN
            NFIDS=NFIDS+1
            FIDLIST(NFIDS)=IFID
            CALL FCGET(FCLUN,IFID,FIDFILE(NFIDS),CTEMP,CTEMP,CTEMP,IERR)
          ENDIF
        ELSE                   ! Event not found
          NLOST=NLOST+1
        ENDIF
      ENDDO
C
C-   What's the answer for files on disk with non-zero size?
C
      WRITE(*,1002) IOSTRING(1:LENOCC(IOSTRING)),NFOUND,NLOST
 1002 FORMAT(' ',A,' Referenced elsewhere:',I6,', not referenced:',I6)
      DO I=1,NFIDS
        WRITE(*,1003) FIDLIST(I),FIDFILE(I)(1:LENOCC(FIDFILE(I)))
 1003   FORMAT('      Referenced FID: ',I6,', File: ',A)
      ENDDO
C
  999 RETURN
      END

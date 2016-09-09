      SUBROUTINE D0DAD_CATCHECK(CATNAME,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Check an EC/FC pair for logical consistancy
C-       NB: This uses the 'DATA' bank used in writing EC's.
C-
C-   Inputs  : CATNAME - directory+file name for catalog pair
C-   Outputs : IERR - 0==> No errors
C-   Controls:
C-
C-   Created  16-Sep-1994   John D. Hobbs
C-   Modified 04-Dec-1994   JDH - Add check of run in file name and
C-     run number referencing file...
C-   Modified 09-Oct-1996   JDH - Restructure for efficiency and add
C-     counts of events for each FID and optional comparison of disk
C-     and file catalog consistancy.
C-   Updated  18-MAR-2004   sss - compile with g77
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:d0dad.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      CHARACTER*(*)  CATNAME,DISKNAMES,FLAG_STRING*20
      CHARACTER*256  ECNAME,FCNAME,TMPNAM,DISK_WILDCARD,NAME1,NAME2
      CHARACTER*1024 DISK_WILDCARD_LIST
      CHARACTER*512 IOSTRING
      INTEGER IERR
      INTEGER NFILES,NRUNS,LOFFS,LWORK,IFID,IRUN,I,J,K,NEVTS,IRECEC,IREC
      INTEGER IOLDRUN,IOLDFID,IOLDEVT,IEVT,NFMAX_IN,CONTEXT,ISTART,IEND
      LOGICAL NOTIFY,REPLACED,LSAME,DELETED,FOUND
C- Flags
      INTEGER ACTIVE_FLAG,DELETED_FLAG,REPLACED_NEW_FLAG,REPLACED_FLAG
      INTEGER REPLACED_SAME_FLAG
      PARAMETER(ACTIVE_FLAG=1,DELETED_FLAG=2,REPLACED_NEW_FLAG=4,
     >  REPLACED_SAME_FLAG=8,REPLACED_FLAG=16)
C- External functions
      INTEGER LENOCC,ICFIND
      LOGICAL LIB$FIND_FILE,LIB$FIND_FILE_END
C- Long term storage
      INTEGER NFMAX
      LOGICAL SCANONLY
      PARAMETER(NFMAX=20000)
      INTEGER IFLAG(NFMAX),ICHECK(NFMAX),ICOUNT(NFMAX)
      DATA IFLAG/NFMAX*0/,ICHECK/NFMAX*0/,ICOUNT/NFMAX*0/,NFMAX_IN/0/
      DATA DISK_WILDCARD/' '/,SCANONLY/.FALSE./
      SAVE ICHECK,ICOUNT,NFMAX_IN,DISK_WILDCARD,SCANONLY
C-----------------------------------------------------------------------
C     
      ECNAME=CATNAME(1:LENOCC(CATNAME))//'.evtcat'
      FCNAME=CATNAME(1:LENOCC(CATNAME))//'.filecat'
      IOSTRING=' '
C     
      IERR=0
      CALL D0DAD_OPEN(JFEC,ECNAME,'R',IDADEC,IERR)
      IF(IERR.NE.0) GOTO 901
      CALL D0DAD_OPEN(JFFC,FCNAME,'R',IDADFC,IERR)
      IF(IERR.NE.0) GOTO 902
C     
      CALL FCHRD(IQ(LFCHD+JLUN),NFILES,IERR)
      IF( NFILES.GT.NFMAX ) THEN
        WRITE(*,8020) NFILES,NFMAX
 8020   FORMAT(' File catalog has ',I6,' files.  Exceeds maximum of',I8)
        IERR = -5
        GOTO 999
      ENDIF

      IF( LDDBG.gt.4 ) THEN
        WRITE(*,*)' Checking '//ECNAME(1:LENOCC(ECNAME))//'/filecat'
        WRITE(*,*) '    File catalog has ',NFILES,' entries.'
        WRITE(*,*) ' '
      ENDIF
C
C- Scan file catalog getting state of each entry.  Possible states are
C-   ACTIVE, DELETED, REPLACED_NEW and REPLACED_SAME
C
      IF( LDDBG.GT.4 ) WRITE(*,*) ' Starting file catalog STATE ',
     >  'definition'
      DO I=1,NFILES
        CALL FCGET(IDADFC,I,CFNAME,CGNAMe,CTAPE,CFCCOM,IERR)
C
        REPLACED = CFNAME(1:LENOCC(FCREPL)).EQ.FCREPL
        DELETED = CFNAME(1:LENOCC(FCDELF)).EQ.FCDELF
        IF( .NOT.REPLACED .AND. .NOT.DELETED ) THEN
          IFLAG(I) = ACTIVE_FLAG
        ELSEIF( DELETED ) THEN
          IFLAG(I) = DELETED_FLAG
        ELSEIF( REPLACED ) THEN
          IFLAG(I) = REPLACED_FLAG
        ENDIF
C
        CALL FILENAME_PARSE(CFNAME,'NAM',TMPNAM,J)
        J=ICFIND('0',TMPNAM,1,J)
        TMPNAM=TMPNAM(J:J+5)
        READ(TMPNAM,*) IRUN
        ICHECK(I)= IRUN
      ENDDO
C
C- For replaced files, define the replacement type: NEW or SAME
C     
      IF( LDDBG.GT.4 ) WRITE(*,*) ' Refining REPLACED state definition'
      DO I=1,NFILES
        IF( IFLAG(I).EQ.REPLACED_FLAG ) THEN
          CALL FCGET(IDADFC,I,CFNAME,CGNAMe,CTAPE,CFCCOM,IERR)
          READ(CFNAME(9:14),*) IFID
          DO WHILE ( IFID.GT.0 .AND. IFID.LE.NFILES )
            CALL FCGET(IDADFC,IFID,TMPNAM,CGNAMe,CTAPE,CFCCOM,IERR)
            REPLACED = TMPNAM(1:LENOCC(FCREPL)).EQ.FCREPL
            IF( REPLACED ) THEN
              READ(TMPNAM(9:14),*) IFID
            ELSE
              IFID=0
            ENDIF
          ENDDO
          CALL FILENAME_PARSE(CFNAME,'NAM+EXT',NAME1,J)
          CALL FILENAME_PARSE(TMPNAM,'NAM+EXT',NAME2,J)
          CALL CLTOU(NAME1)
          CALL CLTOU(NAME2)
          IF( NAME1.NE.NAME2) THEN
            IFLAG(I) = REPLACED_NEW_FLAG
          ELSE
            IFLAG(I) = REPLACED_SAME_FLAG
          ENDIF
        ENDIF
      ENDDO

C- Debugging output

      IF( LDDBG.GT.9 ) THEN
        WRITE(*,1100)
 1100   FORMAT('  FID   FLG    RUN   File Catalog Name Field')
        DO I=1,NFILES
          CALL FCGET(IDADFC,I,TMPNAM,CGNAME,CTAPE,CFCCOM,IERR)
          WRITE(*,1101) I,IFLAG(I),ICHECK(I),TMPNAM(1:LENOCC(TMPNAM))
 1101     FORMAT(I6,2X,I3,2X,I6,2X,A)
        ENDDO
      ENDIF

C- Am I only doing the disk <--> file catalog check or am I also checking
C- the event catalog?

      IF( SCANONLY ) GOTO 801
C
C- Check for consistancy within each run.  Also check for duplicate allocation
C- of file id's and do event counting...
C     
      IF( LDDBG.GT.4 ) WRITE(*,*) ' Checking EVENT catalog internal ',
     > 'consistancy'
      NRUNS=IQ(LECHD+NDEC+JEC_IECRUN)
      IRECEC=IQ(LECHD+NDEC+JEC_IRECEC)
      DO I=1,NRUNS
        LOFFS=LRUNS+IRECEC*(I-1)
        IRUN=IQ(LOFFS+1)
        NEVTS=IQ(LOFFS+4)
        IOLDEVT=0
        CALL ECGRUN(IRUN,IREC,IERR)
        IF( IERR.NE.0 ) GOTO 903
        IF(LDDBG.GT.4) WRITE(*,1002) IRUN,NEVTS
 1002   FORMAT('      Processing run ',I8,' with ',I8,' events')
        DO J=1,NEVTS
          LOFFS=LRDAT+IRECEC*(J-1)
C     Check for monotonically increasing event number (not indicates
C     corruption during PUSH operation.  This should be flagged by the
C     dirty flag in the header)
          IEVT=IQ(LOFFS+JEVNT)
          IF( IEVT.LT.IOLDEVT ) THEN
            WRITE(*,1003) IRUN,I,IOLDEVT,IEVT
 1003       FORMAT(' Event section inconsistancy for run ',I8,' event ',
     >          I8,/,'   Last event number: ',I8,', Current event: ',I8)
            IERR = IERR + 1000*I
          ENDIF
C     Check FID allocation to insure each FID is used by only one run...
C-      and increment event counter
          IFID=IQ(LOFFS+JFID)
          ICOUNT(IFID) = ICOUNT(IFID)+1
          IF( ICHECK(IFID).NE.IRUN ) THEN
            NOTIFY= IOLDRUN.NE.IRUN .OR. IFID.NE.IOLDFID
            IF(NOTIFY) WRITE(*,1001) IFID,ICHECK(IFID),IRUN
 1001       FORMAT('   File ',I6,' assigned to runs ',I8,' and ',I8)
            IERR = IERR+1
            IOLDRUN=IRUN
            IOLDFID=IFID
          ENDIF
          NFMAX_in=MAX(IFID,NFMAX_IN)
        ENDDO
      ENDDO
C     
C- Check for unused ACTIVE files...
C     
      IF( LDDBG.GT.4 ) WRITE(*,*) ' Checking for unused ACTIVE files.'
      DO I=1,NFILES
        IF( IFLAG(I).EQ.ACTIVE_FLAG .AND. ICOUNT(I).EQ.0 ) THEN
          CALL FCGET(IDADFC,I,CFNAME,CGNAMe,CTAPE,CFCCOM,IERR)
          CALL FILENAME_PARSE(CFNAME,'NAM+EXT',TMPNAM,J)
          IF( LDDBG.LE.4 ) THEN
            WRITE(*,1004) TMPNAM(1:J),I
 1004       FORMAT(' Active file: ',A,'(FID=',I8,') is not referenced.')
          ELSE
            WRITE(IOSTRING,1004) TMPNAM(1:J),I
            CALL D0DAD_CATCHECK_ACTIVE(IDADFC,IDADEC,CFNAME,IOSTRING)
          ENDIF
        ENDIF
      ENDDO
C
C- Check for deleted, but referenced files...
C
      IF(LDDBG.GT.4)WRITE(*,*) ' Checking for referenced DELETED files.'
      DO I=1,NFILES
        IF( IFLAG(I).EQ.DELETED_FLAG .AND. ICOUNT(I).GT.0 ) THEN
          CALL FCGET(IDADFC,I,CFNAME,CGNAMe,CTAPE,CFCCOM,IERR)
          CALL FILENAME_PARSE(CFNAME,'NAM+EXT',TMPNAM,J)
          WRITE(*,1005) TMPNAM(1:J),I,ICOUNT(I),ICHECK(I)
 1005     FORMAT(' Deleted file: ',A,'(FID=',I8,') referenced by',
     >       I6,' events in run ',I6)
          IF( LDDBG.GT.4 ) CALL D0DAD_CHECK_BADEVTS(IDADEC,I,ICHECK(I))
        ENDIF
      ENDDO
C     
C- Check for REPLACED (by new or same file) files which are referenced...
C
      IF(LDDBG.GT.4)WRITE(*,*)' Checking for referenced REPLACED files.'
      DO I=1,NFILES
        REPLACED = IFLAG(I).EQ.REPLACED_NEW_FLAG .OR.
     >     IFLAG(I).EQ.REPLACED_SAME_FLAG
        IF(  REPLACED .AND. ICOUNT(I).GT.0 ) THEN
          CALL FCGET(IDADFC,I,CFNAME,CGNAMe,CTAPE,CFCCOM,IERR)
          CALL FILENAME_PARSE(CFNAME,'NAM+EXT',TMPNAM,J)
          IF( REPLACED_NEW_FLAG.ne.0 ) THEN
            WRITE(*,1006) 'new',TMPNAM(1:J),I,ICOUNT(I),ICHECK(I)
          ELSE
            WRITE(*,1006) 'same',TMPNAM(1:J),I,ICOUNT(I),ICHECK(I)
          ENDIF
 1006     FORMAT(' Replaced (',A,') file: ',A,'(FID=',I8,
     >     ') referenced by',I6,' events in run ',I6)
          IF( LDDBG.GT.4 ) CALL D0DAD_CHECK_BADEVTS(IDADFC,I,ICHECK(I))
        ENDIF
      ENDDO
C     
C     Notify if there are file catalog overruns...
C     
      IF( NFMAX_IN.GT.NFILES ) THEN
        WRITE(*,1007) NFILES,NFMAX
 1007   FORMAT(' File catalog has ',I8,' entries.  Event catalog ',
     >       ' references FIDs as high as',I8)
        IERR = 10000+IERR
      ENDIF
C
C- Check to see if we're doing a disk <-> file catalog consistancy check.
C
 801  IF( DISK_WILDCARD_LIST.EQ.' ' ) GOTO 999
      
      IF( LDDBG.GT.4 )WRITE(*,*)' Beginning disk<--> file catalog ',
     > 'consistancy check using wildcard: ',
     >  DISK_WILDCARD(1:LENOCC(DISK_WILDCARD))
C
C- Scan for disk files not in file catalog
C
      IF(LDDBG.GT.4) WRITE(*,*) ' Beginning disk scan for files not in',
     >  ' file catalog.' 
      ISTART=1
      DO WHILE( ISTART.LE.LENOCC(DISK_WILDCARD_LIST) )
        IEND=LENOCC(DISK_WILDCARD_LIST)
        IEND=ICFIND(',',DISK_WILDCARD_LIST,ISTART,IEND)
        DISK_WILDCARD=DISK_WILDCARD_LIST(ISTART:IEND-1)
        CONTEXT=0
        DO WHILE( LIB$FIND_FILE(DISK_WILDCARD,CFNAME,CONTEXT) )
          CALL FILENAME_PARSE(CFNAME,'NAM+EXT',NAME1,J)
          CALL CLTOU(NAME1)
          I=ICFIND('0',NAME1,1,J)
          READ(NAME1(I:I+5),*) IRUN
          FOUND=.FALSE.
          REPLACED=.FALSE.
          FLAG_STRING = ' '
          I=NFILES
          DO WHILE( .NOT.FOUND .AND. I.GT.0 ) 
            IF( ICHECK(I).EQ.IRUN ) THEN
              CALL FCGET(IDADFC,I,CFNAME,CGNAMe,CTAPE,CFCCOM,IERR)
              CALL FILENAME_PARSE(CFNAME,'NAM+EXT',NAME2,J)
              CALL CLTOU(NAME2)
              LSAME = NAME1.EQ.NAME2
              IF( LSAME .AND. IFLAG(I).EQ.ACTIVE_FLAG ) THEN
                FOUND=.TRUE.
              ELSEIF( LSAME .AND. IFLAG(I).EQ.REPLACED_SAME_FLAG ) THEN
                FLAG_STRING = ' (replaced, same)'
              ELSEIF( LSAME .AND. IFLAG(I).EQ.REPLACED_NEW_FLAG ) THEN
                FLAG_STRING = ' (replaced)'
              ELSEIF( LSAME .AND. IFLAG(I).EQ.DELETED_FLAG ) THEN
                FLAG_STRING = ' (deleted)'
              ENDIF
            ENDIF
            I=I-1
          ENDDO
          IF( .NOT.FOUND ) THEN
            J=LENOCC(FLAG_STRING)
            K=LENOCC(NAME1)
            WRITE(*,1009) NAME1(1:K),FLAG_STRING(1:J)
 1009       FORMAT(' ',A,' on disk, but not active',A,' in catalog')
          ENDIF
        ENDDO
        found = LIB$FIND_FILE_END(CONTEXT)
        ISTART=IEND+1
      ENDDO
C
C- Scan for ACTIVE files in file catalog not on disk
C
      IF(LDDBG.GT.4)WRITE(*,*) ' Beginning scan for active files not',
     >  ' on disk.'
      DO I=1,NFILES
        IF( IFLAG(I).EQ.ACTIVE_FLAG ) THEN
          CALL FCGET(IDADFC,I,CFNAME,CGNAMe,CTAPE,CFCCOM,IERR)
          CONTEXT=0
          FOUND = LIB$FIND_FILE(CFNAME,TMPNAM,CONTEXT)
          IF( .NOT.FOUND ) THEN
            WRITE(*,1008) CFNAME(1:LENOCC(CFNAME)),I
 1008       FORMAT(' ',A,' (FID=',I6,') active, but not on disk')
          ENDIF
          FOUND = LIB$FIND_FILE_END(CONTEXT)
        ENDIF
      ENDDO
C     
 999  RETURN
C     
 901  CONTINUE
      IERR = -1
      RETURN
C     
 902  CONTINUE
      IERR = -2
      RETURN
C     
 903  CONTINUE
      IERR = -3
      RETURN
C
C-----------------------------------------------------------------------
      ENTRY D0DAD_CATCHECK_WILDCARD(DISKNAMES)
C-----------------------------------------------------------------------
C
      DISK_WILDCARD_LIST = DISKNAMES
      CALL CLTOU(DISK_WILDCARD_LIST)
      IF( DISK_WILDCARD_LIST.EQ.'RUNIA' ) THEN
        IF( LDDBG.GT.5 ) WRITE(*,*) ' Scanning for Run Ia all stream'
        DISK_WILDCARD_LIST='D0$DATA$DST:ALL_05*.X_MD*.ALL01*,'//
     >     'D0$DATA$DST:ALL_06*.X_MD*.ALL01*'
      ELSEIF( DISK_WILDCARD_LIST.EQ.'RUNIB' ) THEN
        IF( LDDBG.GT.5 ) WRITE(*,*) ' Scanning for Run Ib all stream'
        DISK_WILDCARD_LIST='D0$DATA$DST:ALL_07*.X_MD*.ALL01*,'//
     >     'D0$DATA$DST:ALL_08*.X_MD*.ALL01*,'//
     >     'D0$DATA$DST:ALL_090*.X_MD*.ALL01*,'//
     >     'D0$DATA$DST:ALL_091*.X_MD*.ALL01*,'//
     >     'D0$DATA$DST:ALL_092*.X_MD*.ALL01*,'//
     >     'D0$DATA$DST:ALL_093*.X_MD*.ALL01*'
      ELSEIF( DISK_WILDCARD_LIST.EQ.'RUNIC' ) THEN
        IF( LDDBG.GT.5 ) WRITE(*,*) ' Scanning for Run Ic all stream'
        DISK_WILDCARD_LIST='D0$DATA$DST:ALL_094*.X_MD*.ALL01*,'//
     >     'D0$DATA$DST:ALL_095*.X_MD*.ALL01*,'//
     >     'D0$DATA$DST:ALL_096*.X_MD*.ALL01*,'//
     >     'D0$DATA$DST:ALL_097*.X_MD*.ALL01*,'//
     >     'D0$DATA$DST:ALL_098*.X_MD*.ALL01*,'//
     >     'D0$DATA$DST:ALL_099*.X_MD*.ALL01*'
      ELSE
        DISK_WILDCARD_LIST=DISKNAMES
      ENDIF
C
      RETURN
C
      ENTRY D0DAD_CATCHECK_SCANONLY
      SCANONLY=.TRUE.
      RETURN
C
      END

      SUBROUTINE PX_EVWRIT(IU,SFFLG,DATAFILE,EXTS)
C------------------------------------------------------------
C-                                                          -
C-     handle writing request for single events             -
C-     user must define and set flag PX_WRITE_EVENT         -
C-                                                          -
C-     This is a copy of D0$EVENT_UTIL$EVENT_IO:EVWRIT.FOR  -
C-                                                          -
C-   Created  15-DEC-1992   Vipin Bhatnagar                 -
C-                                                          -
C-   Modified  13-JAN-1993  Vipin Bhatnagar                 -
C-   Separately  Writing Scanned & Unscanned Events         -
C-                                                          -
C-   Return Value IU unit number if SCAN_EVENT.DAT is open  -
C-   Modified  11-FEB-1993  Vipin Bhatnagar
C-   Enable writing an event SCANNED & UNSCANNED both
C-
C-   Modified  19-FEB-1993  Vipin Bhatnagar
C-   Handles writing of events for the Single file input case
C-   Modified  7-MAY-1993   Vipin Bhatnagar
C-    Handling the scanned datafile names according to # of
C-    SCAN banks present(at the most Three scannings)
C-    Picking the output directory name from PIXIE.RCP for the
C-    scanned files
C-------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IU,IERR,N,IUT,NZBANK,NLIBNK,STATUS
      INTEGER ISF,NSF,LSF,IFS,NFS,LFS,ISTOR,NC
      INTEGER GZPROC,LPROC,IZLINK,LADDR,LENGTH
      CHARACTER*80 SCANFILE,DATAFILE,OUTDIR
      CHARACTER*44 FILNAM
      CHARACTER*6  EXT,EXTS
      CHARACTER    NBK
C
      LOGICAL SFFLG
      LOGICAL FIRST,FLGVAL,OK,SECOND
      DATA EXT,FIRST/'.DAT',.TRUE./
C------------------------------------------------------------
C-
C---To Write a single event (not scanned)
C-
      IF (FLGVAL('PX_WRITE_EVENT')) THEN
        CALL GTUNIT(1,IUT,IERR)
        CALL EVENT_FILE(FILNAM,N)
        FILNAM=FILNAM(1:N)//EXT
        CALL D0OPEN(IUT,FILNAM,'OU',OK)
        IF(.NOT.OK) THEN
          CALL INTMSG(' Cannot open '//FILNAM)
          GO TO 999
        ENDIF
        CALL FZFILE(IUT,0,'O')
        CALL WREVNT(IUT)
        CALL FZENDO(IUT,'T')
        CLOSE(IUT)
        CALL INTMSG(' File '//FILNAM//' written.')
        CALL FLGSET('PX_WRITE_EVENT',.FALSE.)
        GO TO 999
      ENDIF
C
C--To Write the Scanned events in a file
C-
      IF (FLGVAL('PX_WRITE_SCAN')) THEN
C-
C----The directory name for scan output file FROM PIXIE.RCP
C-
        CALL EZPICK('PIXIE_RCP')
        CALL EZGETS('PSCAN$OUTDIR',1,OUTDIR,LENGTH,STATUS)
        CALL EZRSET
C-
C----Checking the existance of SCAN bank
C-
        LPROC = GZPROC()
        IF(LPROC.EQ.0) THEN
          CALL ERRMSG('SCANNING','SCAN_START', 'PROC BANK NOT SET UP',
     &      'W')
          GOTO 999
        ENDIF
        ISTOR  = 1
        IZLINK = 8
        LADDR  = LQ(LPROC-IZLINK)
        NLIBNK = NZBANK(ISTOR,LADDR)
C
C----building scanned file extension after checking the # of banks
C
        CALL PXITOC(NLIBNK,1,NBK)
        EXTS = '_SC0'//NBK(1:1)
C-
C----Scan file writing for Single evt./file case
C-
        IF (SFFLG) THEN
          CALL GTUNIT(1,IU,IERR)
          CALL SWORDS(DATAFILE,IFS,NFS,LFS)
          DO NC = IFS,NFS
            IF (DATAFILE(NC:NC).EQ.';') THEN
              LFS = NC-1
            ENDIF
          END DO
          IF ((DATAFILE(LFS:LFS).EQ.'1'.OR.DATAFILE(LFS:LFS).EQ.'2').
     &      AND.( DATAFILE(LFS-3:LFS-3).EQ.'S' )) THEN
            LFS = LFS - 5
          ENDIF
          SCANFILE = DATAFILE(IFS:LFS)//EXTS(1:5)
          SCANFILE = OUTDIR(1:LENGTH)//SCANFILE
          CALL D0OPEN(IU,SCANFILE,'OU',OK)
          IF(.NOT.OK) THEN
            CALL INTMSG(' Cannot open '//SCANFILE)
            GO TO 999
          ENDIF
          CALL FZFILE(IU,0,'O')
          CALL WREVNT(IU)
          CALL FZENDO(IU,'T')
          CLOSE(IU)
          CALL INTMSG(' File '//SCANFILE//' written.')
          CALL FLGSET('PX_WRITE_SCAN',.FALSE.)
C-
C----Scan file writing for the merge file case
C-
        ELSEIF(FIRST) THEN
          CALL GTUNIT(1,IU,IERR)
          SCANFILE = DATAFILE
          CALL SWORDS(SCANFILE,ISF,NSF,LSF)
          DO NC = ISF,NSF
            IF (DATAFILE(NC:NC).EQ.';') THEN
              LSF = NC-1
            ENDIF
          END DO
          IF ((SCANFILE(LSF:LSF).EQ.'1'.OR.SCANFILE(LSF:LSF).EQ.'2').
     &      AND.( SCANFILE(LSF-3:LSF-3).EQ.'S' )) THEN
            LSF = LSF - 5
          ENDIF
          SCANFILE=SCANFILE(ISF:LSF)//EXTS
          SCANFILE = OUTDIR(1:LENGTH)//SCANFILE
          CALL D0OPEN(IU,SCANFILE,'OU',OK)
          IF(.NOT.OK) THEN
            CALL INTMSG(' Cannot open '//SCANFILE)
            GO TO 999
          ENDIF
          CALL INTMSG(' Writing '//SCANFILE//'
     &      now.')
          CALL FZFILE(IU,0,'O')
          FIRST=.FALSE.
          CALL WREVNT(IU)
          CALL FLGSET('PX_WRITE_SCAN',.FALSE.)
        ELSE
C-
C----Writing the next scanned event from merge file
C-
          CALL WREVNT(IU)
          CALL FLGSET('PX_WRITE_SCAN',.FALSE.)
        ENDIF
      ENDIF
C-
  999 RETURN
      END

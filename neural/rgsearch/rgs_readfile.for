      SUBROUTINE RGS_READFILE(IFILE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read Ntuple containing cuts (SWIT=0) or
C-   the IFILEth input ntuple and load data into /RGSCOM/
C-
C-   Inputs  : IFILE    [I]   0   Read CUTFILE
C-                            >0  Read FILE(IFILE)
C-   Outputs : In /RGSCOM/
C-   Controls:
C-
C-   Created  28-APR-1995   Harrison B. Prosper
C-   Updated   5-MAY-1995   Harrison B. Prosper  
C-    Add explicit delete 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IFILE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:RGSCOM.INC'
C----------------------------------------------------------------------
      LOGICAL HEXIST
      INTEGER NENTRIES, STATUS, IPAT, LREC, NTUPLE_ID, NPAT,I,J,K
      CHARACTER*80 REMARK, TITLE, FILENAME
      CHARACTER*4 CHOPT
      REAL    XT(MAXIN), XLO(MAXTAG), XHI(MAXTAG)
C----------------------------------------------------------------------
C
C ****  Get number of events per file, check tags
C
      IF ( IFILE .LE. 0 ) THEN
        NTUPLE_ID = CUT_NTUPLE_ID
        FILENAME  = CUTFILE
        NPAT      = NCUT
      ELSE
        NTUPLE_ID = INP_NTUPLE_ID
        FILENAME  = FILE(IFILE)
        NPAT      = NPATTERN(IFILE)
      ENDIF
C
      IF ( HEXIST(NTUPLE_ID) ) THEN
        CALL HDELET(NTUPLE_ID)
      ENDIF
C
C ****  OPEN FILE
C
      CALL GTUNIT(NETID,IUNIT,STATUS)
      CHOPT = ' '
      LREC = 0
      CALL HROPEN(IUNIT,'INPUT',FILENAME,CHOPT,LREC,STATUS)
      IF ( STATUS .NE. 0 ) THEN
        REMARK = 'Unable to open ntuple file '//FILENAME
        CALL ERRMSG('OPENERR','RGSEARCH_READFILEA',REMARK,'F')
      ENDIF
C
C ****  Get number of entries
C
      CALL HRIN(NTUPLE_ID,9999999,0)
      CALL HNOENT(NTUPLE_ID,NENTRIES)
      NPAT = MIN(NPAT,NENTRIES)
      IF ( NPAT .LE. 0 ) THEN
        NPAT = NENTRIES
      ENDIF
      IF((IFILE.EQ.0).AND.(NPAT.GT.MAXCUT)) THEN
        REMARK = 'You need to bump MAXCUT or trim ntuple '//FILENAME
        CALL ERRMSG('OPENERR','RGSEARCH_READFILE',REMARK,'F')
      ELSE IF((IFILE.GT.0).AND.(NPAT.GT.MAXPAT)) THEN
        REMARK = 'You need to bump MAXPAT or trim ntuple '//FILENAME
        CALL ERRMSG('OPENERR','RGSEARCH_READFILE',REMARK,'F')
      ENDIF

C
C ****  Update pattern count
C
      IF ( IFILE .LE. 0 ) THEN
        NCUT = NPAT
      ELSE
        NPATTERN(IFILE) = NPAT
      ENDIF
C
C ****  Define fields to use for search
C
      NTAG = MAXTAG
      CALL HGIVEN(NTUPLE_ID,TITLE,NTAG,TAGS,XLO,XHI)
      CALL WORD(FILENAME,I,J,K)
      WRITE(6,'(''  Checking field-names for file '',A)') FILENAME(I:J)
      CALL DGN_BEGIN
     &    ('RGSEARCH_RCP','PATTERNS_INPUTS',NTAG,TAGS,STATUS)
C
C ****  Load fields into /RGSCOM/. NOTE: not all may be used for the search
C
      CALL DGN_GET_TAGS(MAXIN,NFIELD,FIELD)
C
C ****  Loop over ntuple
C
      DO IPAT = 1, NPAT
C
C ****  Get fields listed in PATTERNS_INPUTS, including those NOT to be
C ****  used in the search. That is, create a "virtual" ntuple XT(*).
C
        CALL DGN(NTUPLE_ID,IPAT,XT,STATUS)
C
C ****  Copy patterns
C
        IF ( IFILE .LE. 0 ) THEN
          CALL UCOPY(XT(1),PATTERN_CUT(1,IPAT),NFIELD)
        ELSE
          CALL UCOPY(XT(1),PATTERN_IN(1,IPAT),NFIELD)
        ENDIF
      ENDDO
C
C ****  CLOSE FILE
C
      CALL HREND('INPUT')               ! Close the direct access file
      CALL RLUNIT(NETID,IUNIT,STATUS)   ! Return the logical unit
      CLOSE(UNIT=IUNIT)
C
C ****  DELETE NTUPLE
C
      IF ( HEXIST(NTUPLE_ID) ) THEN
        CALL HDELET(NTUPLE_ID)
      ENDIF
  999 RETURN
      END

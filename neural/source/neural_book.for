      SUBROUTINE NEURAL_BOOK(IDN_OUT,FILE,OUTFILE,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book neural network ntuple.
C-
C-   Inputs  : IDN_OUT    [I]   ID of output ntuple
C-             FILE       [C*]  Input ntuple file
C-             OUTFILE    [C*]  Output ntuple file; Use this if
C-                              NOT blank
C-
C-   Outputs : STATUS     [I]   0 -- OK
C-   Controls:
C-
C-   Created  11-MAY-1992   Harrison B. Prosper
C-   Updated  20-JAN-1993   Harrison B. Prosper
C-      Get ntuple_id from RCP file
C-   Updated  15-JAN-1994   Chip Stewart
C-   Updated  10-MAR-1995   Harrison B. Prosper
C-      Generalize a bit
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDN_OUT
      CHARACTER*(*) FILE, OUTFILE
      INTEGER STATUS
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:JETNET.INC'
      INCLUDE 'D0$INC:JNDAT1.INC'
C----------------------------------------------------------------------
      INTEGER MAXV,MAXM,NSELF,NTSELF,ID_WOFF
      REAL O,A,D,T,DT,W,DW,G,ODW,ODT,ETAV
C----------------------------------------------------------------------
C
C ****  JETNET COMMON (with weights)
C
      PARAMETER(MAXV=2000,MAXM=150000)
      COMMON /JNINT1/ O(MAXV),A(MAXV),D(MAXV),T(MAXV),DT(MAXV),
     &                W(MAXM),DW(MAXM),NSELF(MAXM),NTSELF(MAXV),
     &                G(MAXM+MAXV),ODW(MAXM),ODT(MAXV),ETAV(MAXM+MAXV)
C----------------------------------------------------------------------
      INTEGER RECLEN
      PARAMETER( RECLEN = 1024 )
C
      LOGICAL NEWFILE,EZERROR,FOUND
      INTEGER I,J,K,L,N,II,JJ,NTAGS,LFILENAME,NPRIME,LN
      CHARACTER*80 TITLE,REMARK
      CHARACTER*255 FILENAME
      CHARACTER*32 TAG(MAXIN+2*MAXOUT), WIDGET
      CHARACTER CHOPT*4
C----------------------------------------------------------------------
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      SAVE FIRST, ID_WOFF
C----------------------------------------------------------------------
      STATUS = 0
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  Get some RCP stuff
C
        CALL EZPICK('NEURAL_RCP')
        IF ( EZERROR(STATUS) ) THEN
          CALL ERRMSG('NO_RCP_BANK','NEURAL_BOOK','No NEURAL.RCP','F')
        ENDIF
        CALL EZ_GET_CHARS('PATTERNS_TITLE',N,TITLE,STATUS)
        IF ( STATUS .NE. 0 ) THEN
          TITLE = 'Neural Network Patterns'
        ENDIF
C
C ****  GET WEIGHT ID OFFSET
C
        CALL EZGET('WEIGHT_ID_OFFSET',ID_WOFF,STATUS)
        IF ( ID_WOFF.LE. 0 ) ID_WOFF = 10
        CALL EZGETS('OUTPUT_NTUPLE_EXTENSION',1,WIDGET,LN,STATUS)
        IF ( ID_WOFF.LE. 0 )  WIDGET = '_NN'
        CALL EZRSET
      ENDIF
C
C ****  Output File Name
C
      IF ( OUTFILE(1:1) .EQ. ' ' ) THEN
C
C ****  Get full pathname of input RZ file
C
        CALL FIND_FILE('ZZ.ZZ',5,FILENAME,LFILENAME,FOUND)  !To force reset
        CALL FIND_FILE(FILE,LEN(FILE),FILENAME,LFILENAME,FOUND)
        IF ( .NOT. FOUND ) THEN
          REMARK = ' File '//FILE(1:LEN(FILE))//' NOT FOUND'
          CALL ERRMSG('OPEN_ERROR','NEURAL_BOOK',REMARK,'F')
        ENDIF
C
C ****  POSTFIX with _NN
C
        FILENAME = FILENAME(1:LFILENAME)//WIDGET
        LFILENAME= LFILENAME+3
      ELSE
        CALL WORD(OUTFILE(1:LEN(OUTFILE)),II,JJ,LFILENAME)
        FILENAME = OUTFILE(II:JJ)
      ENDIF
C
      WRITE(6,'(''  '')')
      WRITE(6,'('' ************ '')')
      WRITE(6,'('' Writing file '',A)') FILENAME(1:LFILENAME)
C
C ****  DO EVERYTHING IN PAWC
C
      CALL HCDIR('//PAWC',' ')
C
C ****  OPEN RZ-FILE
C
      NEWFILE = .TRUE.
C
      CALL GTUNIT(NETID,OUNIT,STATUS)
      CHOPT = 'N'
      CALL HROPEN
     &  (OUNIT,'OUTPUT',FILENAME(1:LFILENAME),CHOPT,RECLEN,STATUS)
C
C ****  BOOK NTUPLE
C ****  Set tag names
C
      CALL DGN_GET_TAGS(MAXIN,NTAGS,TAG)
      DO I =  1, NLABO
        NTAGS = NTAGS + 1
        CALL WORD(LABO(I),J,K,L)
        TAG(NTAGS) = LABO(I)(J:K)
        NTAGS = NTAGS + 1
        TAG(NTAGs) = 'JN_'//LABO(I)(J:K)
      ENDDO
C
C ****  BOOK NTUPLE
C
      NPRIME = 1000
      CALL HBOOKN (IDN_OUT,TITLE,NTAGS,'OUTPUT',NPRIME,TAG)
  999 RETURN
      END

      SUBROUTINE BOOK_NTUPLE(RCPBANK,TOPDIR,NTAGS,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book neural network ntuple.
C-
C-   Inputs  : RCPBANK    [C*]  Name of RCP-bank
C-   Outputs : TOPDIR     [C*]  Top directory
C-             NTAGS       [I]   Maximum number of outputs
C-             STATUS     [I]   0 -- OK
C-   Controls: 
C-
C-   Created  26-FEB-1993   Pushpa Bhat
C-
C----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C----------------------------------------------------------------------
      CHARACTER*(*) RCPBANK, TOPDIR
      INTEGER STATUS
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:FIT_TWO.INC'
C----------------------------------------------------------------------
      INTEGER RECLEN, MAXFILE
      PARAMETER( RECLEN    = 8191 )
C
      LOGICAL OK,NEWFILE,EZERROR,EZERR
      INTEGER ID,I,J,NIN,NOUT,N,II,JJ,III,JJJ,NTAGS
      INTEGER NTAG, IER, NINTAG
      CHARACTER*80 TITLE,FILENAME,REMARK,STRING
      CHARACTER*8 TAG(MAXPAR),INTAG(MAXPAR)
C----------------------------------------------------------------------
C
C ****  PICK RCP BANK
C
      CALL EZPICK(RCPBANK)
      IF ( EZERR(IER) ) THEN
        CALL EZGET_ERROR_TEXT(STATUS,STRING)
        WRITE(6,*)STRING
        REMARK = 'Unable to pick bank '//RCPBANK(1:LEN(RCPBANK))
        CALL ERRMSG('NO_RCP_BANK','BOOK_NTUPLE',REMARK,'F')
      ELSE
        CALL EZ_GET_CHARS('NTUPLE_FILE',N,FILENAME,STATUS)
        IF ( STATUS .EQ. 0 ) THEN
C
C ****  OPEN RZ-FILE
C
          NEWFILE = .TRUE.
          TOPDIR  = FILENAME
          CALL NTUPLE_FILE_OPEN
     &      (NETID,NEWFILE,FILENAME,RECLEN,TOPDIR,STATUS)
        ELSE
          TOPDIR  = ' ' ! Use first opened RZ file
        ENDIF
C
C ****  BOOK NTUPLE
C
        CALL EZ_GET_CHARS('NTUPLE_TITLE',N,TITLE,STATUS)
        IF ( STATUS .NE. 0 ) THEN
          TITLE = 'ZKIN PARAMETERS'
        ENDIF
C
        CALL EZ_GET_CHARS('NTUPLE_ARRAY',NINTAG,INTAG,STATUS)
        IF ( STATUS .NE. 0 ) THEN
          CALL ERRMSG('NO_INPUTS','BOOOK_NTUPLE',
     &        'Unable to find array NTUPLE_ARRAY','F')
        ENDIF
C
C ****  GET NTUPLE ID
C
        CALL EZGET('NTUPLE_ID',NTUPLE_ID,STATUS)
        IF ( NTUPLE_ID .LE. 0 ) THEN
          NTUPLE_ID = 2
        ENDIF
C
C ****  Set tag names/skip possible *
C
        DO I =  1, NINTAG
          IF ( INTAG(I)(1:1) .EQ. '*' ) THEN
            TAG(I) = INTAG(I)(2:)
          ELSE
            TAG(I) = INTAG(I)
            NTAG=NTAG+1
          ENDIF
        ENDDO
C
C ****  Return number of inputs/outputs
C
        NTAGS= NTAG
        NTUP_NPAR = NTAG
C
C ****  Book ntuple in //PAWC
C
        CALL NTUPLE_SAVE_DIRECTORY
        CALL DHDIR(' ','//PAWC',STATUS,' ')
        CALL NTUPLE_SET_ID(NTUPLE_ID,1)
        CALL NTUPLE_BOOK(TOPDIR,NTAGS,TAG,TITLE,ID,STATUS)
        CALL NTUPLE_RESTORE_DIRECTORY
C
        CALL EZRSET
      ENDIF
  999 RETURN
      END

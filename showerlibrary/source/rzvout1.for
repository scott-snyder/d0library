      SUBROUTINE RZVOUT1(VECT,NOUT,RKEY,ICYCLE,CHOPT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EMULATES RZVOUT1
C-
C-   Inputs  : VECT(*) =  Array to be output
C-             NOUT    = Number of words in VECT to be output
C-             RKEY(*)  = Key Vector
C-             CHOPT   = Not implemented
C-   Outputs : ICYCLE  = Cycle number associated with Key entered
C-             =1 if KEY not present.
C-             =one larger than previous cycle associated with KEY otherwise
C-   Controls:
C-
C-   Created  11-APR-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    VECT(*)
      INTEGER NOUT,RKEY(*),ICYCLE
      CHARACTER*(*) CHOPT
      INCLUDE 'D0$INC:SHLCON.INC'
      INCLUDE 'D0$INC:QUEST.INC/LIST'
      INTEGER LREC,K
      INTEGER IOSTAT_WRITE
C
      CHARACTER*80 DIRECTORY,FILE
      INTEGER IOSTAT_OPEN
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      LOGICAL KEYED
      INTEGER IER
      INTEGER INDX_KEY
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('SHOWERLIBRARY_RCP')              ! select SHOWERLIB bank
        CALL EZGET('KEYED_ACCESS',KEYED,IER)
        CALL EZRSET
      ENDIF
C
      ICYCLE = CYCLES(INDX_KEY(RKEY)) + 1
C
      IQUEST(6) = ICYCLE
      IQUEST(1) = 0
      IF ( KEYED ) THEN
        CALL PACK_REC(RKEY,ICYCLE,LREC)
        WRITE(ISUNIT,ERR=997,IOSTAT = IOSTAT_WRITE)
     &  LREC,NOUT,(VECT(K),K=1,NOUT)
      ENDIF
      RETURN
  998 CALL ERRMSG('SHOWERLIBRARY','RZVOUT1',
     &  'ERROR OPENING FILE ','W')
      GO TO 996
  997 CALL ERRMSG('SHOWERLIBRARY','RZVOUT1',
     &  'ERROR IN WRITING RECORD ','W')
  996 CONTINUE
      IQUEST(1) = 999
  999 RETURN
      END

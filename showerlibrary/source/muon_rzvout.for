      SUBROUTINE MUON_RZVOUT(VECT,NOUT,RKEY,ICYCLE,CHOPT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EMULATES MUON_RZVOUT
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
      INCLUDE 'D0$INC:MULCON.INC'
      INCLUDE 'D0$INC:MULDAT.INC'
      INCLUDE 'D0$INC:QUEST.INC/LIST'
      INTEGER LREC,K,NO_GMUH
      INTEGER IOSTAT_WRITE
C
      CHARACTER*80 DIRECTORY,FILE
      INTEGER IOSTAT_OPEN
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      LOGICAL KEYED
      INTEGER IER
      INTEGER MUON_KEY
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('MUONLIBRARY_RCP')              ! select MUONLIB bank
        CALL EZGET('KEYED_ACCESS',KEYED,IER)
        CALL EZRSET
      ENDIF
C
      ICYCLE = CYCLES(MUON_KEY(RKEY)) + 1
C
      IQUEST(6) = ICYCLE
      IQUEST(1) = 0
      IF ( KEYED ) THEN
        CALL MULIB_PACK_REC(RKEY,ICYCLE,LREC)
        WRITE(ISUNIT,ERR=997,IOSTAT = IOSTAT_WRITE)
     &  LREC,NOUT,(VECT(K),K=1,NOUT)
      ENDIF
      RETURN
  998 CALL ERRMSG('MUONLIBRARY','MUON_RZVOUT',
     &  'ERROR OPENING FILE ','W')
      GO TO 996
  997 CALL ERRMSG('MUONLIBRARY','MUON_RZVOUT',
     &  'ERROR IN WRITING RECORD ','W')
  996 CONTINUE
      IQUEST(1) = 999
  999 RETURN
      END

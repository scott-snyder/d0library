      SUBROUTINE EVOPIN(INPUT_FILE,XOPT,INUNIT,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Open a ZEBRA input file
C-   Inputs  :
C-     FILE_NAME= name of input file
C-     XOPT= ' ' native mode, 'X' exchange mode, 'G' Geant mode, 'Z' ZZIP
C-   Outputs :
C-     INUNIT = allocated unit number
C-     OK = true if ok
C-
C-   Created   7-SEP-1989   Serban D. Protopopescu
C-   Updated   8-MAR-1993   Kirill Denisenko and Hailin Li 
C-                          Added Parallel Reco
C-   Updated  26-NOV-1994   sss - added ZZIP support.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER,INUNIT,ILEN
      CHARACTER*(*) INPUT_FILE
      CHARACTER*(*) XOPT
      CHARACTER*3 CHOPT,CFTYPE,CFRUN,CFTEMP*128
      CHARACTER*1 CH
      INTEGER I,L,LEN,IERR
      LOGICAL OK,OPTX,OPTG,OPTT,OPTZ
      LOGICAL FLGVAL
      SAVE CFTYPE
C----------------------------------------------------------------------
C
      CALL GTUNIT(87,INUNIT,IER)
C
C  Check for a d0dad file...
C
      CALL D0DAD_DFTYPE(INPUT_FILE,IER)
      IF( IER.EQ.0 ) THEN
        OK=.TRUE.
        CALL D0DAD_SYSOPEN(INPUT_FILE,INUNIT,IERR)
        IF( IERR.NE.0 ) OK=.FALSE.
        RETURN
      ENDIF
C
      CHOPT='IU'
      OPTX = .FALSE.
      OPTG = .FALSE.
      OPTT = .FALSE.
      OPTZ = .FALSE.
      L=LEN(XOPT)
      DO 1 I=1,L
        CALL UPCASE(XOPT(I:I),CH)
        IF(CH.EQ.'X')OPTX = .TRUE.
        IF(CH.EQ.'G')OPTG = .TRUE.
        IF(CH.EQ.'T')OPTT = .TRUE.
        IF(CH.EQ.'Z')OPTZ = .TRUE.
    1 CONTINUE
      IF ( OPTT ) THEN
        CHOPT='TUI'
        IF(OPTX) CHOPT='TXI'
        IF(OPTG) CHOPT='TGI'
        IF(OPTZ) CHOPT='TZI'
      ELSE
        IF(OPTX) CHOPT='XI'
        IF(OPTG) CHOPT='GI'
        IF(OPTZ) CHOPT='ZI'
      ENDIF
C&IF VAXVMS,VAXELN
      CALL D0OPEN(INUNIT,INPUT_FILE,CHOPT,OK)
      CALL XZRECL(ILEN,CHOPT)
      IF (ILEN .GE. 0) CALL FZFILE(INUNIT,ILEN,CHOPT)
C&ELSE
C&      IF (FLGVAL('PARALLEL')) THEN
C&        CALL CLI_INI(INUNIT,ILEN,0,OK)
C&        IF (FLGVAL('DBL3SERVER')) THEN
C&          CALL DBL3INIT_RSRVR('CAL', 'C')
C&          CALL DBL3INIT_RSRVR('MUO', 'C')
C&          CALL DBL3INIT_RSRVR('SAM', 'C')
C&          CALL DBL3INIT_RSRVR('CDC', 'C')
C&          CALL DBL3INIT_RSRVR('FDC', 'C')
C&          CALL DBL3INIT_RSRVR('VTX', 'C')
C&          CALL DBL3INIT_RSRVR('TRD', 'C')
C&          CALL DBL3_COM_INI
C&        ENDIF
C&      ELSE
C&        CALL D0OPEN(INUNIT,INPUT_FILE,CHOPT,OK)
C&        CALL XZRECL(ILEN,CHOPT)
C&        IF (ILEN .GE. 0) CALL FZFILE(INUNIT,ILEN,CHOPT)
C&      ENDIF
C&ENDIF
C
  999 CONTINUE
      CFTYPE='   '
      IF( OK ) THEN
        CALL FILENAME_PARSE(INPUT_FILE,'NAM',CFTEMP,ILEN)
        CFTYPE=CFTEMP(1:3)
      ENDIF
      RETURN
C
      ENTRY GET_STREAM_TYPE(CFRUN)
      CFRUN=CFTYPE
      RETURN
C
      ENTRY SET_STREAM_TYPE(INPUT_FILE)
      CFTYPE='   '
      CALL FILENAME_PARSE(INPUT_FILE,'NAM',CFTEMP,ILEN)
      CFTYPE=CFTEMP(1:3)
C
      END


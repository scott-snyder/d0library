      SUBROUTINE D0RECO_FILES(NFILES,INPUT_FILES,STA_FILES,DST_FILES,
     &  NO_EVENTS,TAPE_LABEL,RCP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-        Fill array of event data file names for processing
C-   
C-   Outputs : 
C-   NFILES             = number of files
C-   INPUT_FILES(NFILES)= input files
C-   STA_FILES(NFILES)  = output STA files
C-   DST_FILES(NFILES)  = output DST files
C-   NO_EVENTS(NFILES)  = events to be read per input file 
C-                        if 0 or less read all
C-   TAPE_LABEL         = tape label (used for summary files)
C-   RCP                = files read from FILES_RCP 
C-
C-   Created  10-DEC-1991   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NFILES
      CHARACTER*(*) INPUT_FILES(*),STA_FILES(*),DST_FILES(*)
      CHARACTER*(*) TAPE_LABEL
      INTEGER NO_EVENTS(*)
      LOGICAL RCP
      CHARACTER*80 INPUT_AREA,STA_AREA,DST_AREA
      CHARACTER*48 MESG
      INTEGER N,NSTA,NDST,I,INPLEN,STALEN,DSTLEN,LEN,TRULEN,IER
C----------------------------------------------------------------------
C
      CALL INRCP('FILES_RCP',IER)  ! read in file list
C
      IF ( IER.NE.0 ) THEN  ! no RCP file provided
        NFILES=1
        STA_FILES(1)='STA_OUTPUT_DATA'
        DST_FILES(1)='DST_OUTPUT_DATA'
        INPUT_FILES(1)='EVENT_DATA'
        NO_EVENTS(1)=-1
        TAPE_LABEL='SUMMARY_OUTPUT'
        RCP=.FALSE.
C
      ELSE
C
C              read input files
        RCP=.TRUE.
        CALL EZPICK('FILES_RCP')
        CALL EZ_GET_CHARS('TAPE_NAME',N,TAPE_LABEL,IER)
        CALL EZ_GET_CHARS('INPUT_AREA',N,INPUT_AREA,IER)   
        IF(IER.NE.0) INPUT_AREA=' '
        CALL EZ_GET_CHARS('INPUT_FILES',NFILES,INPUT_FILES,IER)   
        IF(IER.NE.0) THEN
          NFILES=1
          INPUT_FILES(1)='EVENT_DATA'
        ENDIF
        DO I=1,NFILES
          NO_EVENTS(I)=-1
        ENDDO
        N=0
        CALL EZ_GET_CHARS('NUMBER_OF_EVENTS',N,NO_EVENTS,IER)   
        IF(N.GT.0.AND.N.NE.NFILES) THEN
          CALL ERRMSG(' NUMBER_OF_EVENTS error','D0RECO_FILES'
     &      ,'NUMBER_OF_EVENTS does not match INPUT_FILES','F')
        ENDIF
C
C             read STA files
        CALL EZ_GET_CHARS('STA_AREA',N,STA_AREA,IER)   
        IF(IER.NE.0) STA_AREA=' '
        CALL EZ_GET_CHARS('STA_FILES',NSTA,STA_FILES,IER)   
        IF(IER.NE.0) THEN
          NSTA=1
          STA_FILES(1)='STA_OUTPUT_DATA'
        ENDIF
        IF(NSTA.NE.1.AND.NSTA.NE.NFILES) THEN
          WRITE(MESG,FMT=100) NFILES,NSTA
          CALL ERRMSG(' Mismatched no. of  STA files'
     &      ,'D0RECO_NEW_FILES',MESG,'F')
        ENDIF
C
C             read DST files
        CALL EZ_GET_CHARS('DST_AREA',N,DST_AREA,IER)   
        IF(IER.NE.0) DST_AREA=' '
        CALL EZ_GET_CHARS('DST_FILES',NDST,DST_FILES,IER)   
        IF(IER.NE.0) THEN
          NDST=1
          DST_FILES(1)='DST_OUTPUT_DATA'
        ENDIF
        IF(IER.NE.0) THEN
          NDST=1
          DST_FILES(1)='DST_OUTPUT_DATA'
        ENDIF
        IF(NDST.NE.1.AND.NDST.NE.NFILES) THEN
          WRITE(MESG,FMT=100) NFILES,NDST
          CALL ERRMSG(' Mismatched no. of  DST files'
     &      ,'D0RECO_NEW_FILES',MESG,'F')
        ENDIF
C
C         construct file names
        INPLEN=TRULEN(INPUT_AREA)
        STALEN=TRULEN(STA_AREA)
        DSTLEN=TRULEN(DST_AREA)
        DO I=1,NFILES
          LEN=TRULEN(INPUT_FILES(I))
          INPUT_FILES(I)=INPUT_AREA(1:INPLEN)//INPUT_FILES(I)(1:LEN)
          IF(STA_AREA.NE.' '.AND.I.LE.NSTA) THEN
            LEN=TRULEN(STA_FILES(I))
            STA_FILES(I)=STA_AREA(1:STALEN)//STA_FILES(I)(1:LEN)
          ELSE
            STA_FILES(I)=STA_FILES(I-1)
          ENDIF
          IF(DST_AREA.NE.' '.AND.I.LE.NDST) THEN
            LEN=TRULEN(DST_FILES(I))
            DST_FILES(I)=DST_AREA(1:DSTLEN)//DST_FILES(I)(1:LEN)
          ELSE
            DST_FILES(I)=DST_FILES(I-1)
          ENDIF
        ENDDO
C
        CALL EZRSET
      ENDIF
C
  999 RETURN
  100 FORMAT(' Inputs',I3,', Outputs',I3)
      END

       SUBROUTINE PFHITS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Action routine to display an FDC Half with  
C-                         hits and tracks if requested.
C-
C-   Inputs  : none
C-   Outputs : display FDC
C-   Controls: 
C-
C-   Created  24-OCT-1988   Jeffrey Bantly
C-   Updated   7-FEB-1990   Jeffrey Bantly  general cleanup 
C-   Updated  23-JAN-1991   Jeffrey Bantly  add Hits bank check 
C-   Updated  21-FEB-1991   Lupe Howell  Implementing PIXIE using COMPACK 
C-   Updated  30-APR-1991   Jeffrey Bantly  make better use of Compack 
C-   Updated   8-NOV-1991   Robert E. Avery  Also check hit in FHIT bank. 
C-   Updated  22-MAY-1992   Robert E. Avery  Impose maximum hit number.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER HALF,NHIT,NFHIT
      INTEGER IDX,LENGTH,IER
      INTEGER MAX_HITS
C
      CHARACTER*4 PATH,FPATH
C
      LOGICAL EZERROR,FIRST
      EXTERNAL EZERROR
C
      SAVE FIRST,PATH
      DATA FIRST /.TRUE./
      DATA HALF /0/
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        CALL EZPICK('FTRAKS_RCP')
        IF( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE','PFHITS','Can not find FTRAKS_RCP','W')
        ELSE
          CALL EZGETS('FPATH',IDX,FPATH,LENGTH,IER)
          CALL EZRSET
          PATH=FPATH
        ENDIF
      ENDIF
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFDCRZ','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETV('FDC MAX HITS', MAX_HITS)
      CALL EZRSET
C
      CALL PATHST(PATH)
      CALL GTFDCH(NHIT)
      CALL GTFHIT(0,NFHIT)
      NHIT = MAX(NHIT,NFHIT)
      IF (NHIT .LE. 0) THEN
        CALL INTMSG(' No FDC Hits currently present')
      ELSEIF (NHIT .GT. MAX_HITS) THEN
        CALL INTMSG(' Too many FDC Hits to display')
      ELSE
        CALL PFHLF4(HALF)
      ENDIF
      CALL PATHRS()
C----------------------------------------------------------------------
  999 RETURN
      END

      SUBROUTINE SSOPEN(NAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-    Open the standard summary output file
C-   Inputs  : 
C-     NAME = file name
C-   
C-   ENTRY SSCLOS  : close file opened with SSOPEN
C-
C-   Created  16-MAY-1989   Serban D. Protopopescu
C-   Updated  21-JUN-1992   James T. McKinley - Allow logical name (VAX ONLY)
C-                          SUMMARY_FILE to supersede default file name.
C-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME
      CHARACTER*80 NAMEFIL,LOGNAME,LOGTRANS
      DATA LOGNAME/'SUMMARY_FILE'/
      CHARACTER*132 CTEMP
      INTEGER OUT,ERR,TRULEN,TRNLNM,LENGTH,ISTAT
      LOGICAL OK
C----------------------------------------------------------------------
      CALL GTUNIT (3,OUT,ERR)    ! get a unit number
      NAMEFIL = NAME(1:MIN(64,TRULEN(NAME)))
C&IF VAXVMS
      ISTAT=TRNLNM(LOGNAME,LOGTRANS,LENGTH)
      IF(ISTAT.AND.(LENGTH.GT.0))THEN
        WRITE(NAMEFIL,1000)
      ENDIF
C&ENDIF
      CALL D0OPEN(OUT,NAMEFIL,'OFL',OK)
      IF(.NOT.OK) THEN
        OUT=0
        CTEMP = ' cannot open '//NAME//', SSUNIT set to 0'
        CALL ERRMSG(' Cannot open file','SSOPEN',CTEMP,'W')
      ENDIF
      CALL STSSUN(OUT)           ! set SSUNIT to OUT
C
      RETURN
C
      ENTRY SSCLOS
      CALL D0CLOSE(OUT,' ',OK)
      CALL RLUNIT(3,OUT,ERR)
      CALL STSSUN(0)             ! set SSUNIT to 0
  999 RETURN
 1000 FORMAT('SUMMARY_FILE')
      END


      SUBROUTINE L2_VERTEX_CDC(GOOD_VERTEX,ZVERTX) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to find vertex's Z position from CDC hits
C-
C-   Inputs  : none
C-   Outputs : good vertex; true if successful, false if failed
C-             zvertx  vertex position
C-
C-   Created  27-FEB-1990   Qizhong Li-Demarteau
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated   6-AUG-1992   Qizhong Li-Demarteau  to be used in L2 
C-   Updated  29-APR-1993   Daniel R. Claes       obsolete L2_CDCPAR call
C-   Updated  1-nov-1993    Tom Fahland       
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDPDH.LINK'
      INCLUDE 'D0$LINKS:IZDTMH.LINK'
      INCLUDE 'D0$LINKS:IZDGEH.LINK'
      INCLUDE 'D0$LINKS:IZDALH.LINK'
C
      REAL    BIGSGM, CDCRES
      REAL    ITRLMT, SGMFCT, SGMFC2, TOLDST
      REAL    ZCERMX, ZERROR, ZSIGMA, ZVERTX
      INTEGER L2_GZSCDC, RUNID, EVTID, RUNSAV, EVTSAV
      INTEGER IER, ERR, LL0VT, gzl0vt
      LOGICAL GOOD_VERTEX, EZERROR, OK
      LOGICAL FIRST, MORETK
C
      COMMON/VERTEX_CUTS/ BIGSGM, CDCRES, ITRLMT, MORETK,
     &  SGMFCT, SGMFC2, TOLDST, ZCERMX, ZSIGMA
C
      SAVE RUNSAV, EVTSAV
      SAVE FIRST
      DATA RUNSAV/-1/, EVTSAV/-1/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C make sure only build VERT bank once per event
C
      RUNID = IQ(LHEAD+6)
      EVTID = IQ(LHEAD+9)
      IF (RUNID .NE. RUNSAV .OR. EVTID .NE. EVTSAV) THEN
        RUNSAV = RUNID
        EVTSAV = EVTID
      ELSE
        GOOD_VERTEX = .TRUE.
        GOTO 999
      ENDIF
C
C   get pointors for CDC STP banks
C   (formerly from CALL L2_CDCPAR)
C
      LSCDC = L2_GZSCDC()
      LDPDH = LC(LSCDC - IZDPDH)
      LDTMH = LC(LSCDC - IZDTMH)
      LDGEH = LC(LSCDC - IZDGEH)
      LDALH = LC(LSCDC - IZDALH)
C
C Move EZPICK tp _PARAMETERS
C      Now need to be passed ZCERMX
C
      GOOD_VERTEX = .FALSE.
C
C  unpack part of CDC hits
C
      CALL L2_ZCDCHT
C
C  histogramming CDC information to find vertex
C
      CALL L2_ZCDCHS(ZVERTX,ZERROR)
      IF (ABS(ZERROR) .LE. ZCERMX) THEN
        GOOD_VERTEX = .TRUE.
    
C
C  book and fill VERT bank
C
        CALL L2_ZCDCFL(ZVERTX,ZERROR)
      ENDIF
C
  

  999 RETURN
      END

      SUBROUTINE CDROAD(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,NC,IDC) 
C------------------------------------------------------------------
C 
C  Find CDC tracks in a road 
C 
C-   Inputs  : 
C-        ZVTX : vertex position in Z
C-        PHIMIN: minimum phi of the road
C-        PHIMAX: maximum phi of the road
C-        THEMIN: minimum theta of the road
C-        THEMAX: maximum theta of the road
C-   Outputs : NC:     number of CDC tracks in the road
C-             IDC(I): CDC track ID
C
C  Daria Zieminska JAN.,1989
C-   Updated  15-DEC-1989   Qizhong Li-Demarteau  remove unused argument  
C-                                                and wrong calls
C-   Updated   3-JAN-1990   Qizhong Li-Demarteau  added dE/dx 
C-   Updated   6-FEB-1991   Daria Zieminska: check if full-tracking 
C-                                           is already done 
C-   Updated  26-FEB-1991   Qizhong Li-Demarteau   use flag in DTRH to
C-                                       check if full-tracking is done
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated   3-MAR-1992   Qizhong Li-Demarteau  removed machine block 
C-   Updated  20-JUL-1992   Qizhong Li-Demarteau  added an input argument ZVTX 
C                            
C------------------------------------------------------------------
      IMPLICIT NONE  
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER NC,IDC(*)
      INTEGER LDTRH, GZDTRH, IFULL, IER, IPATH
      REAL PHIMIN,PHIMAX,THEMIN,THEMAX, ZVTX
      LOGICAL EZERROR
      LOGICAL  FIRST
      CHARACTER*4 DPATH
      EQUIVALENCE (IPATH, DPATH)
C
      SAVE  FIRST
      DATA  FIRST/.TRUE./
C------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','CDROAD',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('DPATH',IPATH,IER)
        CALL EZGET('IFULL',IFULL,IER)
        CALL EZRSET
      ENDIF
C
C  Check if full tracking results already exist
C
      CALL PATHST(DPATH)
      LDTRH = GZDTRH()
      CALL PATHRS
      IF ((LDTRH .GT. 0) .AND. (IBITS(IQ(LDTRH),IFULL,1) .NE. 0))
     &   GOTO 10
C
      CALL CRHITS(PHIMIN,PHIMAX)
      CALL CRTSEG
      CALL CDTRAK
      CALL DCDEDX
   10 CALL NCROAD(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,NC,IDC) 
C
  999 RETURN 
      END       

      SUBROUTINE ZCDCFL(ZVERTX,ZERROR,WEIGHT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book and fill VERT bank
C-
C-   Inputs  : ZVERTX: vertex's Z position
C-             ZERROR: error on vertex's Z position
C-             WEIGHT: percentage of the track number used for this vertex
C-   Outputs : none
C-
C-   Created  12-MAR-1990   Qizhong Li-Demarteau
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  29-OCT-1991   Qizhong Li-Demarteau  added "weight" for vertex
C-   Updated  21-DEC-1993   Qizhong Li-Demarteau  removed filling HSTR 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL    ZVERTX, ZERROR
      INTEGER WEIGHT
      INTEGER LVERH, LVERT, GZVERH
      INTEGER IER, BIT
      PARAMETER( BIT = 27 )
      CHARACTER*4 VPATH
      INTEGER NR, ERR, IPATH, METHOD, METHOD_BIT
      EQUIVALENCE (IPATH,VPATH)
      LOGICAL FIRST
      LOGICAL EZERROR
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VERTEX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZCDCFL',
     &    'Unable to find bank VERTEX_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('VPATH',IPATH,ERR)
        CALL EZGET('NR',NR,ERR)
        CALL EZGET('METHOD',METHOD,ERR)
        CALL EZRSET
      ENDIF
      CALL PATHST(VPATH)
C
C  book and fill VERH bank, if VERH bank does not exist
C      
      LVERH = GZVERH()
      IF (LVERH.LE.0) THEN
        CALL BKVERH(LVERH)  
        IQ(LVERH+2) = 0        
      ENDIF
C
C  book and fill VERT bank 
C      
      CALL BKVERT(LVERT,NR)
      IF (LVERT .LE. 0) GOTO 999
      LVERH = GZVERH()
      IQ(LVERH + 2) = IQ(LVERH + 2) + 1
      IQ(LVERT+1) = 0                     
      CALL MVBITS(WEIGHT,0,8,IQ(LVERT+2),0)
      IF (IQ(LVERH+2) .EQ. 1) THEN
        IQ(LVERT+2) = IBSET(IQ(LVERT+2),31)  ! main primary vertex
      ELSE
        IQ(LVERT+2) = IBSET(IQ(LVERT+2),30)  ! additional primary vertex
      ENDIF
      IF (METHOD .LE. 1) THEN
        IQ(LVERT+2) = IBSET(IQ(LVERT+2),BIT)
      ELSE
        METHOD_BIT = BIT - METHOD
        IQ(LVERT+2) = IBSET(IQ(LVERT+2),METHOD_BIT)
      ENDIF
      Q(LVERT+5) = ZVERTX
      Q(LVERT+8) = ZERROR
      CALL PATHRS
C
  999 RETURN
      END

      SUBROUTINE ZFDCFL(ZVERTX,ZERROR,WEIGHT,NUSED)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book and fill VERT bank
C-
C-   Inputs  : ZVERTX: vertex's Z position
C-             ZERROR: error on vertex's Z position
C-             WEIGHT: percentage of the number of tracks used for this vertex
C-             NUSED: number of tracks used for this vertex
C-   Outputs : none
C-
C-   Created  13-SEP-1990   Jeffrey Bantly
C-   Updated  10-DEC-1992   Robert E. Avery  Put in EZRSET.
C-   Updated  19-FEB-1993   Robert E. Avery   added "weight" and "nused"
C-   Updated  21-DEC-1993   Qizhong Li-Demarteau  removed filling HSTR 
C-                                           (it is filled in BKVERH now)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C  Input:
      REAL    ZVERTX, ZERROR
      INTEGER WEIGHT, NUSED
C
C  Local:
      INTEGER LVERH, LVERT, GZVERH
      CHARACTER*4 VPATH
      INTEGER NR, ERR
      INTEGER LENGTH
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VERTEX_RCP')
        CALL EZGETS('VPATH',0,VPATH,LENGTH,ERR)
        CALL EZGET('NR',NR,ERR)
        CALL EZRSET
      ENDIF
      CALL PATHST(VPATH)
C
C  book and fill VERH bank, if VERH bank does not exist
C      
      LVERH = GZVERH()
      IF (LVERH.LE.0) THEN
        CALL BKVERH(LVERH)  
        IQ(LVERH+1) = 0                       
        IQ(LVERH+2) = 1        
      ENDIF
C
C  book and fill VERT bank 
C      
      CALL BKVERT(LVERT,NR)
      IQ(LVERT+1) = 0                     
      CALL MVBITS(WEIGHT,0,8,IQ(LVERT+2),0)
      CALL MVBITS(NUSED,0,8,IQ(LVERT+2),8)
      IQ(LVERT+2) = IBSET(IQ(LVERT+2),31)       ! marks as primary vertex
      IQ(LVERT+2) = IBSET(IQ(LVERT+2),26)       ! vertex found by FDC
      Q(LVERT+5) = ZVERTX                       ! Z of vertex
      Q(LVERT+8) = ZERROR                       ! Error on Z
      CALL PATHRS
C-------------------------------------------------------------------------
  999 RETURN
      END

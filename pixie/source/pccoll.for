      SUBROUTINE PCCOLL(NX,XMIN,XMAX,NY,YMIN,YMAX,ECUT,ZMAX,TITLE,
     &  XLAB,YLAB,ZLAB,ARRAY,IARRAY,NXMIN,NYMIN,IXG,IYG,N,ZSCAL,IMARK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Colored Lego plot OF ARRAY( Ref. P1LEGO.FOR )
C-
C-   Inputs  :
C-     NX     - Number of x elements of ARRAY that will be displayed
C-     XMIN   - Minimum value for X
C-     XMAX   - Maximum value for X
C-     NY     - Number of y elements of ARRAY that will be displayed
C-     YMIN   - Minimum value for Y
C-     YMAX   - Maximum value for Y
C-     ECUT   - Minimum energy to draw blocks
C-     ZMIN   - Minimum value for Z
C-     ZMAX   - Maximum value for Z
C-              (if ZMAX is .LT. 0, prog. will calculate ZMAX from data)
C-     TITLE  - Plot title
C-     XLAB   - X label
C-     YLAB   - Y label
C-     ZLAB   - Z label
C-     ARRAY  - Array of data
C-     IARRAY - Array for DTRK and FDCT
C-     IMARK  - 0 if no marks, 1 if special marks (ex. JET center or missing PT)
C-
C-   Created  29-APR-1994   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C    Argument Declarations:
C    ======================
      CHARACTER*(*) XLAB,YLAB,ZLAB
      CHARACTER*(*) TITLE
      INTEGER NX,NY,NXMIN,NYMIN,N,IXG,IYG,IMARK
      INTEGER IARRAY(1:N,1:*)
      REAL    ARRAY(1:N,1:*),XMIN,XMAX,YMIN,YMAX,
     X        ZMIN,ZMAX,ECUT
      REAL    ZSCAL
C=====================================================================
C    Local Declaration:
C    ==================
      REAL TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,ZDEL,
     X     XDIR,YDIR,ZDIR,TEMP,UMIN,UMAX,VMIN,VMAX,
     X     WX,WY,WZ,PLZMAX,ZMED,ZDIV
      REAL ERANGE,ESTEP
      REAL TXMID,TYMID,TXARROW,TYARROW,TZARROW
      INTEGER I,J,IETA,IPHI,XNUM,YNUM,SEGNUM
      CHARACTER*3  COL(10),COLOR,TCOL(2)
      CHARACTER*24 IMESS1
      CHARACTER*7 LABEL(10),TLABE(2)
C    Data Statements:
C    ================
      DATA IMESS1/' No data inside bounds'/
      DATA COL   /'FOR','BLU','CYA','GRE','DPU','PUR','YEL','ORA',
     &            'DRE','RED'/
      DATA LABEL /' .2-.4',' .4-.6',' .6-.8', ' .8-1','  1-1.2',
     &            '1.2-1.4','1.4-1.6','1.6-1.8','1.8-2','2 GeV <'/
      DATA TCOL  /'RED','CYA'/
      DATA TLABE /'CDC-TK','FDC-TK'/
C=====================================================================
      ZMIN=0.
C--- DETERMINING ZMAX

      IF(ZMAX.LE.0)THEN
        ZMAX=PLZMAX(NXMIN,NX,NYMIN,NY,ARRAY,ARRAY,ZMAX,N,0)
        IF(ZMAX.LE.0)THEN
          CALL PUMESS(IMESS1)
          GO TO 999
        ENDIF
      ENDIF
C--- Define Color Range
      ERANGE = ZMAX - ECUT
      ESTEP  = ERANGE/10.
C--- Set viewing parameters
      CALL PLSETV(NX,NY,UMIN,UMAX,VMIN,VMAX)
C---  MAKING GRID
      CALL PLGRID(NXMIN,NX,NYMIN,NY,IXG,IYG,XMIN,XMAX,
     X        YMIN,YMAX)
C--- DRAW Z-AXIS
      ZMED=(XMAX+ZMIN)/2.
      CALL PLZAXS(NXMIN,NYMIN,ZMAX,ZMIN,ZMED,ZSCAL)
      CALL PUOPEN
C-
C--- BUILDING BLOCKS
      DO 100 IPHI=NXMIN,NX
        J = NX - IPHI + NXMIN
        DO 100 IETA=NYMIN,NY
C-   DEFINE BAR
          CALL PXCOLR('BLA')
          TXMIN=FLOAT(J)
          TXMAX=TXMIN+1.0
          TYMIN=FLOAT(IETA)
          TYMAX=TYMIN+1.
          TZMIN=0.
          CALL JPEDGE(0)
          TZMAX=.0001
          COLOR = 'FOR'
          IF (ARRAY(J,IETA) .LT. .2)   GO TO 60
          IF (ARRAY(J,IETA) .GE. .2) THEN
            COLOR = 'FOR'
          ENDIF
          IF (ARRAY(J,IETA) .GE. .4) THEN
            COLOR = 'BLU'
          ENDIF
          IF (ARRAY(J,IETA) .GE. .6) THEN
            COLOR = 'CYA'
          ENDIF
          IF (ARRAY(J,IETA) .GE. .8) THEN
            COLOR = 'GRE'
          ENDIF
          IF (ARRAY(J,IETA) .GE. 1.) THEN
            COLOR = 'DPU'
          ENDIF
          IF (ARRAY(J,IETA) .GE. 1.2) THEN
            COLOR = 'PUR'
          ENDIF
          IF (ARRAY(J,IETA) .GE. 1.4) THEN
            COLOR = 'YEL'
          ENDIF
          IF (ARRAY(J,IETA) .GE. 1.6) THEN
            COLOR = 'ORA'
          ENDIF
          IF (ARRAY(J,IETA) .GE. 1.8) THEN
            COLOR = 'DRE'
          ENDIF
          IF (ARRAY(J,IETA) .GE. 2.) THEN
            COLOR = 'RED'
          ENDIF
          CALL PXCOLFILL(COLOR)
          CALL PLDBAR(3,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,1)
   60     CONTINUE
C-
C--- Draw tracks
C-
          IF (IARRAY(J,IETA) .GT. 0) THEN
            TXMIN = FLOAT(J)
            TXMAX = TXMIN + 1.
            TYMIN = FLOAT(IETA)
            TYMAX = TYMIN + 1.
            TZMIN = 0.
            TZMAX = 5.
            TXMID = TXMIN + (TXMAX-TXMIN)/2.
            TYMID = TYMIN + (TYMAX-TYMIN)/2.
            CALL PXCOLR('RED')
            IF (IARRAY(J,IETA) .EQ. 2)   CALL PXCOLR('CYA')
            CALL J3MOVE(TXMID,TYMID,TZMIN)
            CALL J3DRAW(TXMID,TYMID,TZMAX)
            TXARROW = (TXMAX-TXMIN)/4.
            TYARROW = (TYMAX-TYMIN)/4.
            TZARROW = TZMAX*.9
            CALL J3MOVE(TXMID,TYMID,TZMAX)
            CALL J3DRAW(TXMID+TXARROW,TYMID+TYARROW,TZARROW)
            CALL J3MOVE(TXMID,TYMID,TZMAX)
            CALL J3DRAW(TXMID-TXARROW,TYMID-TYARROW,TZARROW)
          ENDIF
C-
  100 CONTINUE
C-
      CALL JRCLOS
C-
C   PRINTING AXIS
      CALL PLABEL(VMIN,XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,XLAB,YLAB,ZLAB,
     X       TYMAX,TXMAX,TZMAX,NX,NXMIN,NY,NYMIN,ZSCAL)
C  PRINTING TITLE
C NOTE: THIS ROUTINE SETS VIEWING PARAMETES TO X-Y VIEW
      CALL PXTITL(TITLE)
C  PRINTING LEGEND
      CALL LEGEND_LEFT(COL,LABEL,10)
      CALL LEGEND_LINE(TCOL,TLABE,2)
C-
  999 RETURN
      END

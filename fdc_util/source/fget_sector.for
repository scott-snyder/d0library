      SUBROUTINE FGET_SECTOR(XPOS,YPOS,HALF,LAYER,QUAD,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given XPOS,YPOS,HALF,LAYER, find out what
C-   QUAD and SECTOR we are in.
C-
C-   Inputs  : XPOS,YPOS,HALF,LAYER 
C-   Outputs : QUAD,SECTOR
C-
C-   Created  14-MAY-1991   Robert E. Avery
C-   Updated  12-JUL-1991   Susan K. Blessing  Make a couple of integer
C-    numbers real.
C-   Updated  21-OCT-1991   Robert E. Avery  Add check on X0,Y0 (not 0,0) 
C-   Updated  12-DEC-1991   Robert E. Avery  Add radius check for Phi cell.
C-   Updated  13-MAR-1992   Susan K. Blessing  Rearrange a little to speed
C-    up.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC' 
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C  INPUT:
      REAL    XPOS,YPOS
      INTEGER HALF,LAYER
C  OUTPUT:
      INTEGER QUAD,SECTOR
C  LOCAL:
      INTEGER SECT 
      INTEGER OFFSET
      INTEGER LFDPH
      INTEGER QTYPE
C
      REAL    COS45
      REAL    SIN45
      REAL    DIMEN0(6)
      REAL    DIMEN1(6)
      REAL    XINNER(0:MXSECT)
      REAL    XOUTER(0:MXSECT) 
      REAL    YSIZE(0:MXSECT,2) 
      REAL    CELL_SIZE 
      REAL    PHI 
      REAL    X_ROT 
      REAL    Y_ROT 
      REAL    LENGTH 
      REAL    R_SQ,R_SQ_MIN,R_SQ_MAX
C
      LOGICAL FIRST 
C
C  FUNCTION:
      INTEGER FDC_QUADTYPE,GZFDPH
C
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
C
C   Set up sector size arrays
        DO SECT =  0, MXSECT
          CALL GTFWTX(0,4,SECT,DIMEN0)
          IF ( SECT .EQ. 0 ) THEN
            CALL GTFWTX(0,4,SECT+1,DIMEN1)
            CELL_SIZE = (DIMEN1(4)-DIMEN0(4))/2.
          ELSEIF ( SECT .EQ. 3 ) THEN
            CALL GTFWTX(0,4,SECT+1,DIMEN1)
            CELL_SIZE = (DIMEN1(4)-DIMEN0(4))/2.
          ENDIF
          XINNER(SECT) = DIMEN0(4) - CELL_SIZE
          XOUTER(SECT) = DIMEN0(4) + CELL_SIZE
C  B type
          YSIZE(SECT,2) = DIMEN0(2) 
C  A type
          CALL GTFWTX(0,5,SECT,DIMEN0)
          YSIZE(SECT,1) = DIMEN0(1) 
        ENDDO
        COS45=COS(PI/4.)
        SIN45=SIN(PI/4.)
C
C  Phi inner and outer radius
        LFDPH = GZFDPH()
        R_SQ_MIN = C(LFDPH+69+2)**2.
        R_SQ_MAX = C(LFDPH+69+3)**2.
        FIRST = .FALSE.
      ENDIF
C
      IF ( LAYER .EQ. 2 ) THEN
        R_SQ = XPOS**2 + YPOS**2
        IF ( R_SQ.NE.0.0 ) THEN
          PHI = ATAN2(YPOS,XPOS)
        ELSE
          PHI = 0.0
        ENDIF
        IF ( PHI .LT. 0. ) THEN
          PHI = PHI+TWOPI
        ENDIF
        SECTOR = PHI * 36./TWOPI
        QUAD = 0
        IF (   (R_SQ.LT.R_SQ_MIN) 
     &    .OR. (R_SQ.GT.R_SQ_MAX)  ) THEN
          SECTOR = -1
          QUAD = -1
        ENDIF
      ELSE
        IF ( LAYER .EQ. 0 ) THEN
          X_ROT =  XPOS * COS45 + YPOS * SIN45
          Y_ROT = -XPOS * SIN45 + YPOS * COS45
          OFFSET=0
        ELSEIF ( LAYER .EQ. 1 ) THEN
          X_ROT =  XPOS 
          Y_ROT =  YPOS 
          OFFSET=4
        ENDIF
C
C First try odd quadrants
        SECTOR = -1
        IF ( Y_ROT .GT. 0. ) THEN
          QUAD = 1+OFFSET
        ELSE
          QUAD = 3+OFFSET
        ENDIF
        QTYPE = FDC_QUADTYPE(QUAD,HALF)
C
        DO SECT = 0,MXSECT
          LENGTH = YSIZE(SECT,QTYPE)
          IF (   (ABS(Y_ROT).GE.XINNER(SECT) )
     &      .AND.(ABS(Y_ROT).LE.XOUTER(SECT) )
     &      .AND.(ABS(X_ROT).LE.LENGTH) )   THEN
            SECTOR = SECT
          ENDIF
        ENDDO
C
C If no success, try even quadrants
        IF ( SECTOR .EQ. -1 ) THEN
          IF ( X_ROT .GT. 0. ) THEN
            QUAD = OFFSET
          ELSE
            QUAD = 2+OFFSET
          ENDIF
          QTYPE = FDC_QUADTYPE(QUAD,HALF)
C
          DO SECT = 0,MXSECT
            LENGTH = YSIZE(SECT,QTYPE)
            IF ( (ABS(X_ROT).GE.XINNER(SECT) )
     &      .AND.(ABS(X_ROT).LE.XOUTER(SECT) )
     &      .AND.(ABS(Y_ROT).LE.LENGTH) )   THEN
              SECTOR = SECT
            ENDIF
          ENDDO
        ENDIF
C
        IF ( SECTOR .EQ. -1 ) THEN
          QUAD = -1
        ENDIF
C
      ENDIF
  999 RETURN
      END

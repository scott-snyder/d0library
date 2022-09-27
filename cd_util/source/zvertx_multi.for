      SUBROUTINE ZVERTX_MULTI(ZVERTX,ZERROR,WEIGHT,NTRK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : reconstructe the Z position of the primary 
C-                         vertex more accurately by using reconstructed 
C-                         track information to handle multi-vertex
C-
C-   Inputs  : none
C-   Outputs : ZVERT: array of the vertices's Z position
C-             ZERR:  array of the error on vertices's Z position
C-             WEIGHT: array of the "weight" for the vertices
C-             NTRK: array of the number of tracks used for the vertex
C-             
C-                                   # of tracks used for THIS vertex
C-                     "weight(I)" = -------------------------------- * 100
C-                                      total number of the tracks 
C-
C-   Created  21-JUL-1991   Qizhong Li-Demarteau  handle multi-vertices 
C-   Updated   1-NOV-1993   Srini Rajagopalan   Added call to constrained Z
C-                vertex fit. Call PATHST to set path name from vertex_rcp.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDRFT.LINK'
      INTEGER NOENT, I, ERR, IER, NEWHID, NENTRY, WEIGHT(3), IPATH
      INTEGER GZDGEH, LDRFT
      INTEGER IL1, IL2, IL3, IT1, IT2, IT3, IVERT, CUTWEI, IHIS
      INTEGER ISORT, INDEX(3), MIN_ENTRY, NTRK(3)
      REAL    ZVERTX(3), ZERROR(3), ENTRY(3)
      REAL    ZVERT(3), ZERR(3), ZENTRY(3)
      REAL    ZPEAK, TOTTRK, HSUM, CDCRES, R1, R2
      REAL    MEAN, SIGMA
      REAL    CONTEN(100), LEAD, TAIL
      REAL    HSTATI
      CHARACTER*4 VPATH
      EQUIVALENCE (IPATH, VPATH)
      LOGICAL EZERROR, FLAT2, FLAT3
      LOGICAL FIRST, SNGLE1, HIST, HEXIST
      SAVE FIRST
      DATA    FIRST/.TRUE./, SNGLE1/.TRUE./
C----------------------------------------------------------------------
C
C         Create/Set HBOOK directory for VERTEX
C
      CALL DHDIR('VERTEX_RCP','HBOOK_DIRECTORY',IER,' ')
      IF (IER.NE.0) THEN
        CALL ERRMSG('VERTEX','ZVERTX_MULTI',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('VERTEX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('VERTEX','ZVERTX_MULTI',
     &    'Unable to find bank VERTEX_RCP','W')
          GOTO 999
        ENDIF
C
        CALL EZGET_i('VPATH',IPATH,ERR)
        CALL EZGET('CDCRES',CDCRES,ERR)
        CALL EZGET_i('CUTWEI',CUTWEI,ERR)
        CALL EZGET_i('MIN_ENTRY',MIN_ENTRY,ERR)
        IF (ERR .NE. 0) MIN_ENTRY = 3
        CALL EZRSET
        CALL HBOOK1(1099,' VERTEX IN Z$',100,-100.,100.,0.)
        CALL HIDOPT(0,'STAT')     !  get the statistics for all histograms
        LDGEH = GZDGEH()
        LDRFT = LC(LDGEH - IZDRFT)
        IF (LDGEH .GT. 0 .AND. LDRFT .GT. 0) THEN
          R1 = ((C(LDRFT+17) + C(LDRFT+25)) - 
     &         (C(LDRFT+11) + C(LDRFT+19))) / 2
          R2 = C(LDGEH+10) + (C(LDGEH+12) - C(LDGEH+10)) / 2
        ENDIF
      ENDIF
      CALL PATHST(VPATH)
C
      DO 300 I = 1, 3
        ZVERTX(I) = 9999.9
        ZERROR(I) = 9999.9
        ZENTRY(I) = 0.0
        WEIGHT(I) = 0
        ZVERT(I) = 9999.9
        ZERR(I) = 9999.9
        ENTRY(I) = 0.0
 300  CONTINUE
C
      CALL ZVERTX_MKHST(1099,-100.0,100.0)
      TOTTRK = HSUM(1099)              
      IF (TOTTRK .EQ. 0.0) GOTO 999
C
C Perform constrained z vertex fit...
C
      CALL ZVERTX_CONST
C
C
C Perform Standard Z-vertex fit ...
C
C  find the first peak
C
      CALL ZVERTX_PEAK(1099,1,100,ZPEAK,IL1,IT1,NEWHID)
      IF (NEWHID .GT. 0) THEN
        MEAN = HSTATI(NEWHID,1,'HIST',0)
        ZVERT(1) = MEAN
        ENTRY(1) = HSUM(NEWHID)              
        IF (ABS(ZVERT(1)) .GT. 100.0) THEN
          ZERR(1) = 9999.9
          GOTO 999
        ELSE
          ZERR(1) = HSTATI(NEWHID,2,'HIST',0)
          IF (ENTRY(1) .EQ. 0.0) ENTRY(1) = 1.0
          ZERR(1) = ZERR(1) / SQRT(ENTRY(1))
        ENDIF
      ENDIF
      IF (NEWHID .LT. 0) THEN
        ZVERT(1) = ZPEAK
        ENTRY(1) = FLOAT(IT1 - IL1)
        IF (ENTRY(1) .EQ. 1.0) THEN
          IF (R1 .NE. 0.0) THEN
            ZERR(1) = CDCRES * R2 / R1
          ELSE
            ZERR(1) = 1.4
          ENDIF
        ELSE
          CALL HIX(1099,IL1,LEAD)
          CALL HIX(1099,IT1,TAIL)
          ZERR(1) = SQRT((TAIL - LEAD)**2 / 12.)
        ENDIF
        GOTO 302
      ENDIF
      FLAT2 = .FALSE.
      FLAT3 = .FALSE.
C
C now, try to find the second one
C
      CALL ZVERTX_PEAK(1099,1,IL1,ZPEAK,IL2,IT2,NEWHID)
      IF (NEWHID .GT. 0) THEN
        MEAN = HSTATI(NEWHID,1,'HIST',0)
        ZVERT(2) = MEAN
        ENTRY(2) = HSUM(NEWHID)
        IF (ABS(ZVERT(2)) .LE. 100.0) THEN
          ZERR(2) = HSTATI(NEWHID,2,'HIST',0)
          IF (ENTRY(2) .EQ. 0.0) ENTRY(2) = 1.0
          ZERR(2) = ZERR(2) / SQRT(ENTRY(2))
        ENDIF
      ENDIF
      IF (NEWHID .LT. 0) THEN
        ZVERT(2) = ZPEAK
        CALL HIX(1099,IL2,LEAD)
        CALL HIX(1099,IT2,TAIL)
        ZERR(2) = SQRT((TAIL - LEAD)**2 / 12.)
        ENTRY(2) = FLOAT(IT2 - IL2)
        FLAT2 = .TRUE.
      ENDIF
C
C now, try to find the third one
C
      CALL ZVERTX_PEAK(1099,IT1,100,ZPEAK,IL3,IT3,NEWHID)
      IF (NEWHID .GT. 0) THEN
        MEAN = HSTATI(NEWHID,1,'HIST',0)
        ZVERT(3) = MEAN
        ENTRY(3) = HSUM(NEWHID)
        IF (ABS(ZVERT(3)) .LE. 100.0) THEN
          ZERR(3) = HSTATI(NEWHID,2,'HIST',0)
          IF (ENTRY(3) .EQ. 0.0) ENTRY(3) = 1.0
          ZERR(3) = ZERR(3) / SQRT(ENTRY(3))
        ENDIF
      ENDIF
      IF (NEWHID .LT. 0) THEN
        ZVERT(3) = ZPEAK
        CALL HIX(1099,IL3,LEAD)
        CALL HIX(1099,IT3,TAIL)
        ZERR(3) = SQRT((TAIL - LEAD)**2 / 12.)
        ENTRY(3) = FLOAT(IT3 - IL3)
        FLAT3 = .TRUE.
      ENDIF
C
C  sort the verties 
C
 302  CONTINUE
      CALL SORTZV(ENTRY,INDEX,3,1,1,0)
      DO 600 ISORT = 1, 3
        IF (ISORT .GT. 1 .AND. ENTRY(INDEX(ISORT)) .LT. MIN_ENTRY) 
     &    GOTO 301
        ZVERTX(ISORT) = ZVERT(INDEX(ISORT))
        ZERROR(ISORT) = ZERR(INDEX(ISORT))
        ZENTRY(ISORT) = ENTRY(INDEX(ISORT))
 600  CONTINUE
C
 301  CONTINUE
      DO 400 IVERT = 1, 3
        IF (ZENTRY(IVERT) .GT. 0.0) THEN
          WEIGHT(IVERT) = NINT(100 * ZENTRY(IVERT) / TOTTRK)
          NTRK(IVERT) = ZENTRY(IVERT)
        ENDIF
 400  CONTINUE
      IF (ZERROR(2) .LT. 999.9 .AND. WEIGHT(1) .GT. CUTWEI) THEN
        ZERROR(2) = 9999.9
        ZERROR(3) = 9999.9
      ENDIF
C
      DO 500 IHIS = 1099, 1096, -1
        HIST = HEXIST(IHIS)
        IF (HIST) THEN
          CALL HRESET(IHIS,' ')
        ELSE
          GOTO 999
        ENDIF
  500 CONTINUE
C      
  999 RETURN
      END

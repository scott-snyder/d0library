      FUNCTION L0CDHIS()
C----------------------------------------------------------------------
C-
C-  Purpose and Methods : Booking and filling histograms for L0-CD vertex
C-
C-   Updated  25-JAN-1993   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L0CDHIS
C
      INTEGER IER
      INTEGER NVMAX, NVER, I
      INTEGER MIFLAG
      INTEGER MIFLAG2
      PARAMETER (NVMAX=5)
      LOGICAL FIRST, EZERROR
      REAL ZVER(NVMAX), EZVER(NVMAX)
      REAL FASTZ, SLOWZ
      LOGICAL FGOOD, SGOOD
      REAL FASTZ2, SLOWZ2
      LOGICAL FGOOD2, SGOOD2
      LOGICAL EVTOK
C
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      L0CDHIS=.TRUE.
C
C Create/set HBOOK directory VERTEX
C
      CALL DHDIR('VERTEX_RCP','HBOOK_DIRECTORY',IER,' ')
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('VERTEX','L0CDHIS',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
      IF (FIRST) THEN
        FIRST = .FALSE.
C
        CALL HBOOK1(110,'FASTZ-CD Z',100,-100.0,100.0,0.)
        CALL HBOOK1(120,'FASTZ-CD Z (no jet trig)',100,-100.0,100.0,0.)
        CALL HBOOK1(130,'Slow Z-CD Vertex',100,-100.0,100.0,0.)
        CALL HBOOK1(140,'Slow Z-CD Vertex (no jet trig)',100,
     &                                                 -100.0,100.0,0.)
        CALL HBOOK2(150,'MI Flag vs Num CD Vtx',18,-0.5, 5.0,
     &                                          12, 0.5, 4.5, 0.)
C        CALL HBOOK1(160,'MI Flag',5, -0.5, 4.5, 0.)
      ENDIF
      CALL HIDOPT(0,'STAT')
C
C  extract z coordinate of primary vertex {ZVER(1)} and fill histogram
C
      DO I = 1, NVMAX
        EZVER(I) = 9999.9
      ENDDO
      CALL ZVERTE(NVER, ZVER, EZVER)
      IF (NVER .LE. 0) GOTO 999
C
C  extract Level 0 z coordinate information
C
      CALL L0_VERTICES(FASTZ,FGOOD,SLOWZ,SGOOD,MIFLAG)
C
C  check if event is a jet trigger
C
      CALL ZCOGNG(EVTOK)
      IF ( FGOOD ) THEN
        CALL HFILL(110,FASTZ-ZVER(1),0.,1.)
        IF ( EVTOK ) CALL HFILL(120,FASTZ-ZVER(1),0.,1.)
      ENDIF
      IF ( SGOOD ) THEN
        CALL HFILL(130,SLOWZ-ZVER(1),0.,1.)
        IF ( EVTOK ) CALL HFILL(140,SLOWZ-ZVER(1),0.,1.)
        CALL HFILL(150,FLOAT(NVER),FLOAT(MIFLAG),1.)
C        CALL HFILL(160,FLOAT(MIFLAG),0.,1.)
      ENDIF
C
C------------------------------------------------------------------------------
  999 RETURN
      END

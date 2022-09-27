      SUBROUTINE VERTEX_FIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Vertex Fitting using CDC tracks.
C-
C-   Inputs  : DTRK track banks.
C-   Outputs : VERT banks
C-   Controls: 
C-
C-   Created   5-AUG-1995   Srini Rajagopalan
C-   Updated  27-SEP-1995   Srini Rajagopalan  MOve setting of RCP beam
C-                          positions/errors outside all IF blocks. 
C-   Updated   3-OCT-1995   Srini Rajagopalan  VERT expansion for all VERT's
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVERT.LINK'
C
      INTEGER   I,IER
      INTEGER   LVERH,LVERT,GZVERH,GZVERT
      INTEGER   IVERT,NVERT,MAX_NUM_VERT
      PARAMETER (MAX_NUM_VERT=3)
      INTEGER    WEIGHT(MAX_NUM_VERT),NTRK(MAX_NUM_VERT)
C
      REAL      BEAM_POS(3),BEAM_SLOP(2),BEAM_ERR(2),ZCERMX
      REAL      ZVERTX(MAX_NUM_VERT),ZERROR(MAX_NUM_VERT)
C
      LOGICAL   REDO_VERTFIT,DO_CONSTRAINED_FIT,DO_LIKELIHOOD_FIT
      LOGICAL   EZERROR, FIRST
      DATA      FIRST /.TRUE./
      
C      
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VERTEX_FIX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('BAD RCP','VERTEX_FIX',
     &                'Unable to find bank VERTEX_FIX_RCP','F')
        ENDIF
        CALL EZGET_l('REDO_VERTFIT',REDO_VERTFIT,IER)
        CALL EZGET_l('DO_CONSTRAINED_FIT',DO_CONSTRAINED_FIT,IER)
        CALL EZGET_l('DO_LIKELIHOOD_FIT',DO_LIKELIHOOD_FIT,IER)
        CALL EZGET('ZCERMX',ZCERMX,IER)
        CALL EZRSET
      ENDIF
C
C Copy X,Y from VERH to RCP
C       
      LVERH = GZVERH()
      IF (LVERH.GT.0) THEN
        CALL EZPICK('VERTEX_RCP')
        CALL UCOPY(Q(LVERH+4),BEAM_POS(1),3)
        CALL UCOPY(Q(LVERH+7),BEAM_SLOP(1),2)
        CALL UCOPY(Q(LVERH+9),BEAM_ERR(1),2)
        CALL EZSETA('BEAM_POS',  1, 3, 1, BEAM_POS,  IER)
        CALL EZSETA('BEAM_SLOP', 1, 2, 1, BEAM_SLOP, IER)
        CALL EZSETA('BEAM_ERR',  1, 2, 1, BEAM_ERR,  IER)
        CALL EZRSET
      ENDIF
C
      IF (REDO_VERTFIT) THEN
C
C drop VERT banks
C
        LVERH = GZVERH()
        IF (LVERH.GT.0) THEN
          NVERT = IQ(LVERH+2)
          DO IVERT = NVERT,1,-1
            LVERT = GZVERT(IVERT)
            IF (LVERT.GT.0 .AND. BTEST(IQ(LVERT+2),24)) THEN
              CALL MZDROP(IXCOM,LVERT,' ')
              IQ(LVERH+2) = IQ(LVERH+2) - 1
            ENDIF
          ENDDO
        ENDIF
C
C redo vertex finding
C
        CALL CDC_VERTEX(ZVERTX,ZERROR,WEIGHT,NTRK)
C
C fill VERT banks
C
        DO 100 I = 1, MAX_NUM_VERT
          IF (ABS(ZERROR(I)) .LE. ZCERMX) THEN
            CALL ZVERTFL_CDC(ZVERTX(I),ZERROR(I),WEIGHT(I),NTRK(I))
          ENDIF
  100   CONTINUE
C
        CALL XYVERT             ! Redo x,y vertex
C
      ENDIF
C
C Expand all existing VERT banks to accomodate new word
C
      LVERT = GZVERT(1)
      DO WHILE (LVERT.GT.0)
        IF (IQ(LVERT-1).EQ.18) CALL MZPUSH(IXCOM,LVERT,0,1,' ')
        Q(LVERT+19) = 0.
        LVERT = LQ(LVERT)
      ENDDO
C
C Constrained fit?
C
      IF (DO_CONSTRAINED_FIT) THEN
      ENDIF
C
C Likelihood fit
C
      IF (DO_LIKELIHOOD_FIT) THEN
      ENDIF
C
  999 RETURN
      END

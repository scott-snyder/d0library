      SUBROUTINE PMWIND(VIEW,NMOD,QUAD,XMAGC,YMAGC,ZMAGC,
     C                  XCOS,YCOS,ZCOS)
C======================================================================
C 
C    Purpose and Methods : To set the window and port size for the
C                          blow up view of the track 
C 
C    Inputs  : VIEW - tells if bend or nonbend view desired
C              NMOD - tells the module number containing the track
C              QUAD - tells quadrant of the chamber the track is in
C 
C    Created  14-JAN-1990   Carol Francis
C    DH 4/90 CLEANUP 
C======================================================================
      IMPLICIT NONE
C======================================================================
C    Variable Declarations
C    =====================
      INTEGER IVIEW
      INTEGER QUAD                      ! Quadrant number of module
      INTEGER VIEW                      ! Tells if nonbend or bend
      INTEGER NSPAR,NBUF,IBUF           ! Outputs of MUMODU
      INTEGER NMOD                      ! Module number
      REAL SPAR(3),XPAR(3),ROTM(3,3)    ! Outputs of MUMODU
      REAL XMAGC,YMAGC,ZMAGC            ! Values for calculations
      REAL XCOS,YCOS,ZCOS               !    from GTMUOT
      REAL RPT,XTRK,YTRK,ZTRK           ! Point on track
      REAL XWMIN,XWMAX,YWMIN,YWMAX      ! Window around view
      REAL XWINDO,YWINDO
      CHARACTER*4 HSHAPE                ! Outputs of MUMODU
      DATA NBUF/0/
      DATA XWINDO/30./
      DATA YWINDO/60./
C
C    Executable Program
C    ==================
      CALL MUMODU(NMOD,HSHAPE,NSPAR,SPAR,XPAR,ROTM,
     X            NBUF,IBUF)

C    Determine the center points of the window
C    =========================================
      IF (QUAD.EQ.1 .OR. QUAD.EQ.3) THEN
        RPT = (XPAR(1)-XMAGC)/XCOS
        XTRK = XPAR(1)
        YTRK = (RPT*YCOS)+YMAGC
        ZTRK = (RPT*ZCOS)+ZMAGC
      ELSEIF (QUAD.EQ.0.OR.QUAD.EQ.2.OR.QUAD.EQ.4) THEN
        RPT = (XPAR(2)-YMAGC)/YCOS
        XTRK = (RPT*XCOS)+XMAGC
        YTRK = XPAR(2)
        ZTRK = (RPT*ZCOS)+ZMAGC
      ELSE                              ! For quad. > 4
        RPT = (XPAR(3)-ZMAGC)/ZCOS
        XTRK = (RPT*XCOS)+XMAGC
        YTRK = (RPT*YCOS)+YMAGC
        ZTRK = XPAR(3)
      ENDIF
C
C    By using quad, calculate viewport for blowup view
C    ===================================================
C    Bend view
C    =========
      IF (VIEW.EQ.1) THEN      
        IF (QUAD.EQ.1) THEN
          IVIEW = 6
          XWMIN = ZTRK - XWINDO
          XWMAX = ZTRK + XWINDO
          YWMIN = XTRK - YWINDO
          YWMAX = XTRK + YWINDO
        ELSE IF (QUAD.EQ.3) THEN
          IVIEW = 12
          XWMIN = ZTRK - XWINDO
          XWMAX = ZTRK + XWINDO
          YWMIN = -XTRK - YWINDO
          YWMAX = -XTRK + YWINDO
        ELSE IF (QUAD.EQ.0.OR.QUAD.EQ.2) THEN
          IVIEW = 1
          XWMIN = ZTRK - XWINDO
          XWMAX = ZTRK + XWINDO
          YWMIN = YTRK - YWINDO
          YWMAX = YTRK + YWINDO
        ELSE IF (QUAD.EQ.4) THEN
          IVIEW = 7
          XWMIN = ZTRK - XWINDO
          XWMAX = ZTRK + XWINDO
          YWMIN = -YTRK - YWINDO
          YWMAX = -YTRK + YWINDO
        ELSEIF (QUAD.EQ.5. OR. QUAD.EQ.7) THEN 
          IVIEW = 9
          XWMIN = XTRK - XWINDO
          XWMAX = XTRK + XWINDO
          YWMIN = -ZTRK - YWINDO
          YWMAX = -ZTRK + YWINDO
        ELSEIF (QUAD.EQ.6. OR. QUAD.EQ.8) THEN 
          IVIEW = 10
          XWMIN = YTRK - XWINDO
          XWMAX = YTRK + XWINDO
          YWMIN = -ZTRK - YWINDO
          YWMAX = -ZTRK + YWINDO
        ELSEIF (QUAD.EQ.9. OR. QUAD.EQ.11) THEN 
          IVIEW = 3
          XWMIN = XTRK - XWINDO
          XWMAX = XTRK + XWINDO
          YWMIN = ZTRK - YWINDO
          YWMAX = ZTRK + YWINDO
        ELSEIF (QUAD.EQ.10. OR. QUAD.EQ.12) THEN 
          IVIEW = 4
          XWMIN = YTRK - XWINDO
          XWMAX = YTRK + XWINDO
          YWMIN = ZTRK - YWINDO
          YWMAX = ZTRK + YWINDO
        ENDIF
      ENDIF
C
C    Nonbend view
C    ============
      IF (VIEW.EQ.2) THEN       
        IF (QUAD.EQ.1) THEN
          IVIEW = 5
          XWMIN = YTRK - XWINDO
          XWMAX = YTRK + XWINDO
          YWMIN = XTRK - YWINDO
          YWMAX = XTRK + YWINDO
        ELSE IF (QUAD.EQ.3) THEN
          IVIEW = 11
          XWMIN = YTRK - XWINDO
          XWMAX = YTRK + XWINDO
          YWMIN = -XTRK - YWINDO
          YWMAX = -XTRK + YWINDO
        ELSE IF (QUAD.EQ.0.OR.QUAD.EQ.2) THEN
          IVIEW = 2
          XWMIN = XTRK - XWINDO
          XWMAX = XTRK + XWINDO
          YWMIN = YTRK - YWINDO
          YWMAX = YTRK + YWINDO
        ELSE IF (QUAD.EQ.4) THEN
          IVIEW = 8
          XWMIN = XTRK - XWINDO
          XWMAX = XTRK + XWINDO
          YWMIN = -YTRK - YWINDO
          YWMAX = -YTRK + YWINDO
        ELSEIF (QUAD.EQ.5. OR. QUAD.EQ.7) THEN 
          IVIEW = 10
          XWMIN = YTRK - XWINDO
          XWMAX = YTRK + XWINDO
          YWMIN = -ZTRK - YWINDO
          YWMAX = -ZTRK + YWINDO
        ELSEIF (QUAD.EQ.6. OR. QUAD.EQ.8) THEN 
          IVIEW = 9
          XWMIN = XTRK - XWINDO
          XWMAX = XTRK + XWINDO
          YWMIN = -ZTRK - YWINDO
          YWMAX = -ZTRK + YWINDO
        ELSEIF (QUAD.EQ.9. OR. QUAD.EQ.11) THEN 
          IVIEW = 4
          XWMIN = YTRK - XWINDO
          XWMAX = YTRK + XWINDO
          YWMIN = ZTRK - YWINDO
          YWMAX = ZTRK + YWINDO
        ELSEIF (QUAD.EQ.10. OR. QUAD.EQ.12) THEN 
          IVIEW = 3
          XWMIN = XTRK - XWINDO
          XWMAX = XTRK + XWINDO
          YWMIN = ZTRK - YWINDO
          YWMAX = ZTRK + YWINDO
        ENDIF
      ENDIF
      CALL JWINDO(XWMIN,XWMAX,YWMIN,YWMAX)
      CALL PMDDET(-IVIEW)
      CALL PMEVNT(IVIEW,VIEW)
C
  999 RETURN
      END

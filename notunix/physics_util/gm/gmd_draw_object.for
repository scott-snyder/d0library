      SUBROUTINE GMD_DRAW_OBJECT(IOBJECT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Project 4-vector on (eta, phi).
C-
C-   Inputs  : IOBJECT  [I]   OBJECT ID
C-   Outputs : none
C-   Controls: none
C-
C-   Created  11-DEC-1991   Harrison B. Prosper
C-   Updated   6-MAY-1993   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IOBJECT
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:GM_4VECT.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:GM_PARAMS.DEF'
C----------------------------------------------------------------------
      INTEGER I, J, N, LP, IRADIUS, PNUT
      REAL    ETA, PHI, THETA, JET_RADIUS, PARTON_RADIUS, PARENT_RADIUS
      REAL    PT, RADIUS, X, Y, T, RNDM
      REAL    LABEL_RADIUS
      REAL    ETA_CHAR, PHI_CHAR, ETARANGE, PHIRANGE
      REAL    ETAMIN, ETAMAX, PHIMIN, PHIMAX, PSCALE
      REAL    PP(4)
C
      LOGICAL ADD_LABELS, PARTON_OBJECT, DRAW_ALLCONES, DRAW_ALLPNUTS
C
      EXTERNAL LABEL      ! ISAJET FUNCTION
      EXTERNAL GMD_LABEL
C
      CHARACTER*3 COLOR, PCOLOR
      CHARACTER*8 LABEL, GMD_LABEL, PARTICLE
      CHARACTER*32 PARAM
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL PUGETV('ETA CHAR', ETA_CHAR)
        CALL PUGETV('PHI CHAR', PHI_CHAR)
        CALL PUGETV('PHIMIN', PHIMIN)
        PHIMIN = PHIMIN*PI
C
        CALL PUGETV('PHIMAX', PHIMAX)
        PHIMAX = PHIMAX*PI
C
        CALL PUGETV('ETAMIN', ETAMIN)
        CALL PUGETV('ETAMAX', ETAMAX)
C
        ETARANGE  = ETAMAX-ETAMIN
        PHIRANGE  = PHIMAX-PHIMIN
        ETA_CHAR  = 0.01*ETA_CHAR*ETARANGE
        PHI_CHAR  = 0.01*PHI_CHAR*PHIRANGE
        PSCALE    = 1.01
      ENDIF
C
C ****  Check Object ID
C
      IF ( (IOBJECT .LT. 1) .OR. (IOBJECT .GT. NOBJECT) ) THEN
        GOTO 999
      ENDIF
C
      PARTON_OBJECT = IOBJECT .LE. NPART
C
C ****  Copy 4-Vect into local buffer
C
      CALL UCOPY(P(1, IOBJECT), PP, 4)
C
C ****  Get particle label
C
      IF ( PARTON_OBJECT ) THEN
        PARTICLE  = LABEL(PARTID(IOBJECT))
        CALL PUGETV('PARTON RADIUS', PARTON_RADIUS)
        CALL PUGETV('PARENT RADIUS', PARENT_RADIUS)
C
      ELSE
C
C ****  RECO OBJECTS
C
        PARTICLE  = GMD_LABEL(IOBJECT)
        CALL PUGETV('DEFAULT RADIUS', JET_RADIUS)
C
C ****  Special processing for JETS
C
        IF ( PARTID(IOBJECT) .EQ. ID_JET ) THEN
C
C ****  Get ConeSize
C
          IF ( ORIGIN(IOBJECT) .GT. 0 ) THEN
            JET_RADIUS = FLOAT(ORIGIN(IOBJECT))/10.0
          ENDIF
C
C ****  Check whether to draw all conesizes
C
          CALL PUGETV('DRAW ALLCONES', DRAW_ALLCONES)
C
          IF ( .NOT. DRAW_ALLCONES ) THEN
            CALL PUGETV('JET RADIUS', RADIUS)
            IRADIUS = 10.*RADIUS
            IF ( IRADIUS .NE. ORIGIN(IOBJECT) ) THEN
              GOTO 999
            ENDIF
          ENDIF
C
        ELSEIF ( PARTID(IOBJECT) .EQ. ID_ETMISS ) THEN
C
C ****  Special processing for PNUTs
C
C ****  Check whether to draw all PNUTS
C
          CALL PUGETV('DRAW ALLPNUTS', DRAW_ALLPNUTS)
C
          IF ( .NOT. DRAW_ALLPNUTS ) THEN
            CALL PUGETV('PNUT NUMBER', PNUT)
            IF ( PNUT .NE. PARENT(IOBJECT) ) THEN
              GOTO 999
            ENDIF
          ENDIF
C
        ENDIF
      ENDIF
C
C ****  Get Color
C
      CALL PUGETA('DEFAULT COLOR', COLOR)
C
      CALL WORD(PARTICLE, I, J, LP)
      PARAM = PARTICLE(I:J)//' COLOR'
      CALL PUGETA(PARAM, COLOR)
C
      CALL PUGETV('ADD LABELS', ADD_LABELS)
C
C ****  Compute eta, phi
C
      CALL ETOETA(PP, PHI, THETA, ETA)
C
C ****  Draw circle and fill if a parton
C
      IF ( PARTON_OBJECT ) THEN
        CALL PXCOLR('WHI')
        CALL JCIRCL(ETA, PHI, 0., PSCALE*PARTON_RADIUS, 1)
C
        CALL PXCOLFILL(COLOR)
        CALL JCIRCL(ETA, PHI, 0., PARTON_RADIUS, 1)
C
C ****  Label according to parent
C
        IF ( (PARENT_RADIUS .GT. 0.0) .AND.
     &       (PARENT_RADIUS .LT. PARTON_RADIUS) .AND.
     &       (PARENT(IOBJECT) .NE. 0)     ) THEN
C
          PARAM = LABEL(PARENT(IOBJECT))
          CALL WORD(PARAM, I, J, N)
          PARAM = PARAM(I:J)//' COLOR'
          PCOLOR = COLOR
          CALL PUGETA(PARAM, PCOLOR)
          CALL PXCOLFILL(PCOLOR)
          CALL JCIRCL(ETA, PHI, 0., PARENT_RADIUS, 1)
        ENDIF
C
      ELSE
        CALL PXCOLR(COLOR)
        CALL JCIRCL(ETA, PHI, 0., JET_RADIUS, 1)
        CALL JMOVE(ETA-JET_RADIUS, PHI)
        CALL JDRAW(ETA+JET_RADIUS, PHI)
        CALL JMOVE(ETA, PHI-JET_RADIUS)
        CALL JDRAW(ETA, PHI+JET_RADIUS)
      ENDIF
C
C ****  Label it
C
      IF ( ADD_LABELS ) THEN
        PARAM = ' '
        N = 0
C
        PT = SQRT(PP(1)*PP(1)+PP(2)*PP(2))
        WRITE(UNIT=PARAM, FMT=1000, ERR=900) PT
        CALL WORD(PARAM, I, J, N)
        PARAM = '|'//PARAM(I:J)
        N = N + 1
C
  900   CONTINUE
C
        PARAM = '['//PARTICLE(1:LP)//PARAM(1:N)//']'
        N = 1 + LP + N + 1
C
        CALL JSIZE(ETA_CHAR, PHI_CHAR)
C
        CALL PUGETV('LABEL RADIUS', LABEL_RADIUS)
        T = PI*RNDM(0)
        Y = LABEL_RADIUS*COS(T)
        X = LABEL_RADIUS*SIN(T)
C
        IF ( PARTON_OBJECT ) THEN
          CALL JJUST(RIGHT, CENTER)
          CALL JMOVE(ETA-PARTON_RADIUS, PHI)
          CALL JDRAW(ETA-PARTON_RADIUS-X, PHI+Y)
          CALL PUSTRG(ETA-PARTON_RADIUS-X, PHI+Y, PARAM(1:N))
C
        ELSE
          CALL JJUST(LEFT, CENTER)
          CALL JMOVE(ETA+JET_RADIUS, PHI)
          CALL JDRAW(ETA+JET_RADIUS+X, PHI+Y)
          CALL PUSTRG(ETA+JET_RADIUS+X, PHI+Y, PARAM(1:N))
        ENDIF
      ENDIF
C
  999 RETURN
 1000 FORMAT(F5.1)
      END

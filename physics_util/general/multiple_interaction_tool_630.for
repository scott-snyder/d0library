      FUNCTION MULTIPLE_INTERACTION_TOOL_630()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determines as best possible if an event is
C-                         a single or a multiple interaction event.
C     Based on MI_RUN1, retuned for 630 GeV by John Krane 1997
C-                    
C-   Inputs  : none
C-   Outputs : function set to 1 : if most likely single interaction
C-                             2 : if only likely single interaction
C-                             3 : if only likely multiple interaction
C-                             4 : if most likely multiple interaction
C-                             5 : if likely more than two interactions
C-                             0 : if inadequate data is available
C-                            -1 : likely single, but zero CD vertices
C-
C-   Created  07-JUL-1997   John Krane
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:VERTEX_MULTI.PARAMS'
      INTEGER MULTIPLE_INTERACTION_TOOL_630
C
      INTEGER NVER,TYPVER(MAXVER),WGTVER(MAXVER)
      INTEGER METVER(MAXVER),NVERTRK(MAXVER)
      REAL    ZVER(MAXVER),DZVER(MAXVER),CZZVER(MAXVER)
C
      INTEGER LKPLV0,GZPLV0
      INTEGER MI_FLAG,MI_FLAG_JPK
      REAL    FZPOS, SZPOS
      LOGICAL FGOOD, SGOOD, SZINT, GOODINT
      INTEGER ILEVEL
C
      INTEGER IVERS,IQUAL,NTRK,NCEL,IER
      REAL ETSUMS(3),ESUMS(9)
      REAL    TOTAL_ENERGY,EECN,EECC,EECS
C
      REAL    DELZ
      REAL    VTX_WGT,FLAG_WGT,ETOT_WGT,ECE_WGT,DELZ_WGT,TRK_WGT
      REAL    WEIGHT
      REAL    WEIGHT_CUT(2)
C-----Krane's mods:  add vars for MI_FLAG reset and change Wgts from .37, .6
      DATA WEIGHT_CUT/0.43,0.65/
C
      INTEGER LPLV0
      REAL MI_QUAL,X,Y
      REAL TRKCRIT,TRKCRIT2
C-----Necessary for histogramming
      INTEGER LUMINDEX,VAXTIME(2),ERR,TEMPVAR
      REAL LUM(2),AGE(2),INSTLUM,EC_MAX,EC_MIN
C----------------------------------------------------------------------
      WEIGHT=0.
      ILEVEL = 0
      CALL VZERO(TYPVER,MAXVER)
      CALL VZERO(WGTVER,MAXVER)
      CALL VZERO(METVER,MAXVER)
      CALL VZERO(NVERTRK,MAXVER)
      CALL VZERO(ZVER,MAXVER)
      CALL VZERO(DZVER,MAXVER)
      CALL VZERO(CZZVER,MAXVER)
C
C ****  Fetch CD Z information
C
      CALL CD_VERTEX_INFO(NVER,ZVER,DZVER,TYPVER,WGTVER,
     &        METVER,CZZVER,NVERTRK)
C
C ****  Fetch Level 0 information
C
      CALL GTPLV0_ZONLY( FZPOS, FGOOD, SZPOS, SGOOD, MI_FLAG )
C
C
C-----Krane's mods: reset mi_flag with my new version
C
C     Usually the cuts are at 0.6, 0.7, and 1.0
C
C
      LPLV0 = GZPLV0()
      IF ( LPLV0 .GT. 0 ) MI_QUAL = Q( LPLV0 + 4)
      MI_FLAG_JPK=1
      IF (MI_QUAL .GT. 0.5) MI_FLAG_JPK=2
      IF (MI_QUAL .GT. 1.0) MI_FLAG_JPK=3
      IF (MI_QUAL .GT. 1.5) MI_FLAG_JPK=4
      IF (MI_QUAL .GT. 3.0) MI_FLAG_JPK=5
C
C ***   Fetch more Level 0 information (the interaction bit)
C
      LKPLV0=GZPLV0()
      IF ( LKPLV0.GT.0 ) THEN
        IF ( IBITS(IQ(LKPLV0+1),12,1).EQ.1 ) THEN
          SZINT = .TRUE.
        ELSE
         SZINT = .FALSE.
        ENDIF
      ENDIF
C
C  Good slowz requires good interaction bit and slowz good flag:
C
      GOODINT = SZINT.AND.SGOOD
C
C ***   Get calorimeter energies:
C
      TOTAL_ENERGY = 0.0
      CALL GTGLOB(IVERS,IQUAL,NTRK,NCEL,ETSUMS,ESUMS,IER)
      IF (IER.EQ.0 ) THEN
        TOTAL_ENERGY = ESUMS(1)
        EECS         = ESUMS(2)
        EECC         = ESUMS(3) + ESUMS(4)
        EECN         = ESUMS(5)
      ELSE
        CALL CGET_CALENERGY(TOTAL_ENERGY,EECN,EECC,EECS)
      ENDIF
C
C ****  Standard decisions:
C
C...Tool 4 or 5:
C
      EC_MAX=MAX(EECN,EECS) ! define these here in case we goto 900
      EC_MIN=MIN(EECN,EECS)
      IF ( TOTAL_ENERGY.GT.630.0.OR.EECN.GT.315.0.OR.
     &     EECS.GT.315. ) THEN
        IF ( TOTAL_ENERGY.GT.1260.0.OR.EECN.GT.630.0.OR.
     &       EECS.GT.630. ) THEN
          ILEVEL = 5
        ELSE
          ILEVEL = 4
        ENDIF
        WEIGHT=0.99
        GOTO 900
      ENDIF
C
C...Tool -1:
C
      IF ( NVER.EQ.0.AND.GOODINT ) THEN
        ILEVEL = -1
        WEIGHT = 0.0
        GOTO 900
      ENDIF
C
C...Tool 0: (no interaction)
C
      IF ( (.NOT.GOODINT).AND.NVER.LE.0 ) THEN
        ILEVEL = 0
        WEIGHT=0.1
        GOTO 900
      ENDIF
C
C ...Tool 1:
C
      IF ( NVER.EQ.1.AND.MI_FLAG_JPK.EQ.1 ) THEN
        ILEVEL = 1
        WEIGHT = 0.2
        GOTO 900
      ENDIF
C
C ...If FDC finds vertex, can only find one:
C
      IF ( METVER(1).EQ.3 ) THEN
        ILEVEL = MI_FLAG_JPK
        IF (ILEVEL .EQ. 5) ILEVEL=4
        WEIGHT = 0.3
        IF (MI_FLAG_JPK.GT.2) WEIGHT=0.97
        GOTO 900
      ENDIF
C
C ...Use a weighted decision for Tool 2,3,or 4
C
C... Weight based on number of vertices:
C    Krane's mod: wgts were .2, .6, .9, .99
      IF ( NVER.EQ.1 ) THEN
        VTX_WGT = 0.1
      ELSEIF ( NVER.EQ.2 ) THEN
        VTX_WGT = 0.5
      ELSEIF ( NVER.EQ.3 ) THEN
        VTX_WGT = 0.9
      ELSE
        VTX_WGT = 0.99
      ENDIF
C...weight based on flag value
C
C    Krane's mod:  wgts were .2, .4, .6, .85
C
      IF (GOODINT) THEN
        IF ( MI_FLAG_JPK.EQ.1 ) THEN
          FLAG_WGT = 0.01
        ELSEIF ( MI_FLAG_JPK.EQ.2 ) THEN
          FLAG_WGT = 0.3
        ELSEIF ( MI_FLAG_JPK.EQ.3 ) THEN
          FLAG_WGT = 0.5
        ELSEIF ( MI_FLAG_JPK.EQ.4 ) THEN
          FLAG_WGT = 0.9
        ELSEIF ( MI_FLAG_JPK.EQ.5 ) THEN
          FLAG_WGT = 0.99
        ENDIF
      ELSE
        FLAG_WGT = WEIGHT_CUT(1)
      ENDIF
C...weight based on total energy in cal.
C
C    Krane's mod:  cuts were 350, and 525
C
      IF ( TOTAL_ENERGY.LT.400. ) THEN
        ETOT_WGT = 0.1
      ELSEIF (TOTAL_ENERGY.GE.400..AND.TOTAL_ENERGY.LT.500.) THEN
        ETOT_WGT = .3
      ELSEIF (TOTAL_ENERGY.GE.500..AND.TOTAL_ENERGY.LT.590.) THEN
        ETOT_WGT = .5
      ELSEIF ( TOTAL_ENERGY.GE.590. ) THEN
        ETOT_WGT = .9
      ENDIF
C...weight based on EC energy
C
C    Krane's mod:  cuts were 210, 280, and 315
C
      IF ( EECN.LT.180. .AND. EECS.LT.180. ) THEN
        ECE_WGT = 0.1
      ELSEIF (( EECN.GE.180..AND.EECN.LT.275. ) .OR.
     &            ( EECS.GE.180..AND.EECS.LT.275. )) THEN
        ECE_WGT = 0.5
      ELSEIF (( EECN.GE.275..AND.EECN.LT.315. ) .OR.
     &            (EECS.GE.275..AND.EECS.LT.315. )) THEN
        ECE_WGT = 0.9
      ELSE
        ECE_WGT = 0.99
      ENDIF
C...weight based on difference between CD and Level0 z
C
C     Kranes mod:  shift zero point by 3 cm, cuts were 5 and 10
C                      wgts were .2, .6, .9
      DELZ = ABS(SZPOS+3 - ZVER(1))
      IF ( DELZ.LT.25. ) THEN
        DELZ_WGT = 0.1
      ELSEIF ( DELZ.GE.25. .AND. DELZ.LT.37. ) THEN
        DELZ_WGT = 0.5
      ELSEIF ( DELZ.GE.37. ) THEN
        DELZ_WGT = 0.9
      ENDIF
      IF (SZPOS.EQ.0.0) DELZ_WGT=0.43 ! neutral value if no slowz vertex
C...weight based on weight per vertex and ntracks per vertex
C
C     Krane's mod, used to be ...3*NVERTRK.GE.25...
C    
      TRKCRIT  = 0.0001
      TRKCRIT2 = 0.0001
      IF (NVERTRK(1).GT.0) THEN
       Y=FLOAT(WGTVER(1))
       X=FLOAT(NVERTRK(1))
       TRKCRIT = Y - ( 40*LOG(X+6) - 62 )
       TRKCRIT2= Y - ( 40*LOG(X+6) - 65 )
      ENDIF
      TRK_WGT=0.1
      IF ( TRKCRIT .LE. 0.0 ) TRK_WGT = 0.5
      IF ( TRKCRIT2.LE. 0.0 ) TRK_WGT = 0.99
C
C*** Add up weights for total and get answer
C
C Krane's mods:  change wgt scheme from .33, .33, .055, .111, .111
C
      WEIGHT = (0.10)*VTX_WGT + (0.35)*FLAG_WGT +
     &     (0.025)*(ETOT_WGT + ECE_WGT) + (0.30)*DELZ_WGT +
     &     (0.20)*TRK_WGT
C
C
      IF ( WEIGHT.LT.WEIGHT_CUT(1) ) THEN
        ILEVEL = 2
      ELSEIF (WEIGHT.GE.WEIGHT_CUT(1).AND.WEIGHT.LT.WEIGHT_CUT(2) ) THEN
        ILEVEL = 3
      ELSE
        ILEVEL = 4
      ENDIF
C
C... Separate Tool 4 into Tool 4 or 5
C
      IF ( ILEVEL.EQ.4 ) THEN
        IF ( NVER.GT.2.AND.NVERTRK(3).GT.4 ) THEN
          ILEVEL = 5
        ENDIF
      ENDIF
C
C
  900 CONTINUE
C
      MULTIPLE_INTERACTION_TOOL_630 = ILEVEL
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END

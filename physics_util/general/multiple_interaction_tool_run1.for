      FUNCTION MULTIPLE_INTERACTION_TOOL_RUN1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determines as best possible if an event is
C-                         a single or a multiple interaction event.
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
C-   Created  25-FEB-1993   Jeffrey Bantly
C-   Updated  29-MAR-1993   Qizhong Li-Demarteau  modified
C-   Updated   4-DEC-1995   Tracy L. Taylor  modified for use with Run 1b data
C-                                           2 new values added (-1,5)
C-   Updated  10-JAN-1995   Andrew G. Brandt Fix GLOB bug
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:VERTEX_MULTI.PARAMS'
      INTEGER MULTIPLE_INTERACTION_TOOL_RUN1
C
      INTEGER NVER,TYPVER(MAXVER),WGTVER(MAXVER)
      INTEGER METVER(MAXVER),NVERTRK(MAXVER)
      REAL    ZVER(MAXVER),DZVER(MAXVER),CZZVER(MAXVER)
C
      INTEGER LKPLV0,GZPLV0
      INTEGER MI_FLAG
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
      DATA WEIGHT_CUT/0.37,0.6/
C----------------------------------------------------------------------
      ILEVEL = 0
      CALL VZERO_i(TYPVER,MAXVER)
      CALL VZERO_i(WGTVER,MAXVER)
      CALL VZERO_i(METVER,MAXVER)
      CALL VZERO_i(NVERTRK,MAXVER)
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
      IF ( TOTAL_ENERGY.GT.1800.0.OR.EECN.GT.900.0.OR.
     &     EECS.GT.900. ) THEN
        IF ( TOTAL_ENERGY.GT.3600.0.OR.EECN.GT.1800.0.OR.
     &       EECS.GT.1800. ) THEN
          ILEVEL = 5
        ELSE
          ILEVEL = 4
        ENDIF
        GOTO 900
      ENDIF
C
C...Tool -1:
C
      IF ( NVER.EQ.0.AND.GOODINT ) THEN
        ILEVEL = -1
        GOTO 900
      ENDIF
C
C...Tool 0: (no interaction)
C
      IF ( (.NOT.GOODINT).AND.NVER.LE.0 ) THEN
        ILEVEL = 0
        GOTO 900
      ENDIF
C
C ...Tool 1:
C
      IF ( NVER.EQ.1.AND.MI_FLAG.EQ.1 ) THEN
        ILEVEL = 1
        GOTO 900
      ENDIF
C
C ...If FDC finds vertex, can only find one:
C
      IF ( METVER(1).EQ.3 ) THEN
        ILEVEL = MI_FLAG
        GOTO 900
      ENDIF
C
C ...Use a weighted decision for Tool 2,3,or 4
C
C... Weight based on number of vertices:
      IF ( NVER.EQ.1 ) THEN
        VTX_WGT = 0.2
      ELSEIF ( NVER.EQ.2 ) THEN
        VTX_WGT = 0.6
      ELSEIF ( NVER.EQ.3 ) THEN
        VTX_WGT = 0.9
      ELSE
        VTX_WGT = 0.99
      ENDIF
C...weight based on flag value
      IF (GOODINT) THEN
        IF ( MI_FLAG.EQ.1 ) THEN
          FLAG_WGT = 0.2
        ELSEIF ( MI_FLAG.EQ.2 ) THEN
          FLAG_WGT = 0.4
        ELSEIF ( MI_FLAG.EQ.3 ) THEN
          FLAG_WGT = 0.6
        ELSEIF ( MI_FLAG.EQ.4 ) THEN
          FLAG_WGT = 0.85
        ENDIF
      ELSE
        FLAG_WGT = WEIGHT_CUT(1)
      ENDIF
C...weight based on total energy in cal.
      IF ( TOTAL_ENERGY.LT.1000. ) THEN
        ETOT_WGT = 0.2
      ELSEIF (TOTAL_ENERGY.GE.1000..AND.TOTAL_ENERGY.LT.1500.) THEN
        ETOT_WGT = .65
      ELSEIF ( TOTAL_ENERGY.GE.1500. ) THEN
        ETOT_WGT = .9
      ENDIF
C...weight based on EC energy
      IF ( EECN.LT.600..OR.EECS.LT.600. ) THEN
        ECE_WGT = 0.2
      ELSEIF (( EECN.GE.600..AND.EECN.LT.800. ) .OR.
     &            ( EECS.GE.600..AND.EECS.LT.800. )) THEN
        ECE_WGT = 0.65
      ELSEIF (( EECN.GE.800..AND.EECN.LT.900. ) .OR.
     &            (EECS.GE.800..AND.EECS.LT.900. )) THEN
        ECE_WGT = 0.9
      ENDIF
C...weight based on difference between CD and Level0 z
      DELZ = ABS(SZPOS - ZVER(1))
      IF ( DELZ.LT.5. ) THEN
        DELZ_WGT = 0.2
      ELSEIF ( DELZ.GE.5. AND. DELZ.LT.10. ) THEN
        DELZ_WGT = 0.6
      ELSEIF ( DELZ.GT.10. ) THEN
        DELZ_WGT = 0.9
      ENDIF
C...weight based on weight per vertex and ntracks per vertex
      IF ( (WGTVER(1) - 3*NVERTRK(1)).GE.25 ) THEN
         TRK_WGT = 0.2
      ELSE
         TRK_WGT = 0.8
      ENDIF
C
C*** Add up weights for total and get answer
C
      WEIGHT = (1./3.)*VTX_WGT + (1./3.)*FLAG_WGT +
     &     (1./18.)*(ETOT_WGT + ECE_WGT) + (1./9.)*DELZ_WGT +
     &     (1./9.)*TRK_WGT
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
      MULTIPLE_INTERACTION_TOOL_RUN1 = ILEVEL
C----------------------------------------------------------------------
  999 RETURN
      END

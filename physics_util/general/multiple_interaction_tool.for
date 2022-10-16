      FUNCTION MULTIPLE_INTERACTION_TOOL()
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
C-                             0 : if inadequate data is available
C-
C-   Created  25-FEB-1993   Jeffrey Bantly
C-   Updated  29-MAR-1993   Qizhong Li-Demarteau  modified 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:VERTEX_MULTI.PARAMS'
      INTEGER MULTIPLE_INTERACTION_TOOL
C
      INTEGER NVER,TYPVER(MAXVER),WGTVER(MAXVER)
      INTEGER METVER(MAXVER),NVERTRK(MAXVER)
      REAL    ZVER(MAXVER),DZVER(MAXVER),CZZVER(MAXVER)
      REAL    WGTDIF
C
      INTEGER MI_FLAG
      REAL    FZPOS, SZPOS
      LOGICAL FGOOD, SGOOD
      INTEGER ILEVEL
C
      INTEGER LKGLOB, GZGLOB
      EXTERNAL GZGLOB
      REAL    TOTAL_ENERGY,EECN,EECC,EECS
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
C ****  Make likelihood determination of number of vertices
C
C...no CD vertex
      IF ( NVER.LE.0 ) THEN
        IF ( SGOOD ) THEN
          ILEVEL = MI_FLAG
        ELSE
          ILEVEL = 0
        ENDIF
C...one CD vertex
      ELSEIF ( NVER.EQ.1 ) THEN
        IF ( .NOT.SGOOD ) THEN
          ILEVEL = 2
        ELSEIF ( MI_FLAG.EQ.1 ) THEN
          ILEVEL = 1
        ELSEIF ( MI_FLAG.EQ.2 ) THEN
          ILEVEL = 2
        ELSEIF ( MI_FLAG.EQ.3 ) THEN
          ILEVEL = 3
        ELSE
          IF ( MI_FLAG.EQ.4 ) THEN
            IF ( METVER(1).EQ.1 ) THEN
              ILEVEL = 3
            ELSE
              ILEVEL = 4
            ENDIF
          ELSE
            ILEVEL = 2
          ENDIF
        ENDIF
C...two CD vertices
      ELSEIF ( NVER.EQ.2 ) THEN
        IF ( .NOT.SGOOD ) THEN
          ILEVEL = 3
        ELSEIF ( MI_FLAG.EQ.1 ) THEN
          IF ( ABS(ZVER(1)-SZPOS).LE.8.0 ) THEN
            ILEVEL = 2
          ELSE 
            ILEVEL = 3
          ENDIF
        ELSEIF ( MI_FLAG.EQ.2 ) THEN
          IF ( ABS(ZVER(1)-SZPOS) .LE. 8.0 ) THEN
            IF ( WGTVER(1) .GE. 70 ) THEN
              ILEVEL = 2
            ELSE
              ILEVEL = 3
            ENDIF
          ELSE
            WGTDIF=100.0
            IF ( WGTVER(1).GT.0.0 ) 
     &            WGTDIF = (WGTVER(1) - WGTVER(2)) / WGTVER(1)
            IF (WGTDIF .LE. 25) THEN
              ILEVEL = 4
            ELSE 
              ILEVEL = 3
            ENDIF
          ENDIF
          ELSEIF ( MI_FLAG.EQ.3 ) THEN
            ILEVEL = 4
          ELSEIF ( MI_FLAG.EQ.4 ) THEN
            ILEVEL = 4
          ELSE
            ILEVEL = 3
          ENDIF
C...three or more CD vertices
        ELSEIF ( NVER.GE.3 ) THEN
          IF ( .NOT.SGOOD ) THEN
            ILEVEL = 4
          ELSEIF ( MI_FLAG.EQ.1 ) THEN
            IF ( ABS(ZVER(1)-SZPOS).LE.8.0 ) THEN
              ILEVEL = 2
            ELSE 
              ILEVEL = 3
            ENDIF
          ELSEIF ( MI_FLAG.EQ.2 ) THEN
            IF ( ABS(ZVER(1)-SZPOS).LE.8.0 .AND. WGTVER(1).GE.70 ) THEN
              ILEVEL = 2
            ELSE 
              ILEVEL = 3
            ENDIF
          ELSEIF ( MI_FLAG.EQ.3 ) THEN
            ILEVEL = 4
          ELSEIF ( MI_FLAG.EQ.4 ) THEN
            ILEVEL = 4
          ELSE
            ILEVEL = 4
          ENDIF
        ENDIF
C
C...cut on total energy in the calorimeter
C
        TOTAL_ENERGY = 0.0
        LKGLOB = GZGLOB()
        IF ( LKGLOB.GT.0 ) THEN
          TOTAL_ENERGY = Q(LKGLOB+8)
        ELSE
          CALL CGET_CALENERGY(TOTAL_ENERGY,EECN,EECC,EECS)
        ENDIF
        IF ( TOTAL_ENERGY.GT.1800.0 ) ILEVEL = 4
C
        MULTIPLE_INTERACTION_TOOL = ILEVEL
C----------------------------------------------------------------------
  999   RETURN
        END

      FUNCTION GMD_LABEL(ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return a label for specified reco object ID.
C-
C-   Returned value  : Label  C*8
C-   Inputs  : ID [I] Object ID
C-   Outputs :
C-   Controls:
C-
C-   Created   5-MAY-1993   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ID
      CHARACTER*8 GMD_LABEL
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:GM_4VECT.INC'
      INCLUDE 'D0$PARAMS:GM_PARAMS.DEF'
C----------------------------------------------------------------------
      INTEGER MAXOBJ
      PARAMETER( MAXOBJ = 10 )
      CHARACTER*8 NAME(MAXOBJ)
      INTEGER JET,PNUT
      REAL    CONESIZE
      LOGICAL FIRST
      SAVE FIRST,NAME
C----------------------------------------------------------------------
      DATA FIRST  /.TRUE./
      DATA NAME   /MAXOBJ*'    '/
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        NAME(ID_PHOTON)   = 'PHOTON'
        NAME(ID_ELECTRON) = 'ELECTRON'
        NAME(ID_MUON)     = 'MUON'
        NAME(ID_TAU)      = 'TAU'
        NAME(ID_JET)      = 'JT'
        NAME(ID_ETMISS)   = 'ETMISS'
      ENDIF
C
      IF ( (ID .GE. 1) .AND. (ID .LE. NOBJECT) ) THEN
C
        IF     ( PARTID(ID) .EQ. ID_JET ) THEN
          CONESIZE = FLOAT(ORIGIN(ID))/10.0
          JET      = PARENT(ID)
          IF ( CONESIZE .GT. 0.0 ) THEN
            WRITE(NAME(ID_JET),'(''JT'',F3.1,''_'',I2.2)') CONESIZE,JET
          ELSE
            WRITE(NAME(ID_JET),'(''JETNN_'',I2.2)') JET
          ENDIF
C
        ELSEIF ( PARTID(ID) .EQ. ID_ETMISS ) THEN
          PNUT = PARENT(ID)
          WRITE(NAME(ID_ETMISS),'(''ETMISS_'',I1)') PNUT
        ENDIF
C
        GMD_LABEL = NAME(PARTID(ID))
      ELSE
        GMD_LABEL = 'UNKNOWN'
      ENDIF
      RETURN
      END

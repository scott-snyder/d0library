      FUNCTION GB_READ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read from (GM) Ntuple file into buffer
C-   GM_EVENT.
C-
C-      Requirements:
C-
C-          (1) Ntuple file defined as the logical NTUPLE
C-
C-   Returned value  : FALSE IF END-OF-DATA (FOR NTUPLE)
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  11-JAN-1993   Harrison B. Prosper, Seung-chan Ahn
C-   Updated  28-FEB-1993   Harrison B. Prosper
C-      Update muon object
C-   Updated   8-MAR-1993   Harrison B. Prosper, Stan Krzywdzinski
C-      Add parton objects
C-   Updated  29-APR-1993   Harrison B. Prosper
C-      Add GLOBAL object
C-   Updated   6-MAY-1993   Harrison B. Prosper
C-   Updated   7-MAY-1993   Harrison B. Prosper
C-   Updated  21-MAY-1993   Stan M. Krzywdzinski, Harrison B. Prosper
C-   Updated  23-JUL-1993   Stan M. Krzywdzinski, Harrison B. Prosper
C-      Corrected IEVENT assignment bug; change RDUM to REAL
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:GM_PARAMS.DEF'
      INCLUDE 'D0$INC:GM_EVENT.INC'
      INCLUDE 'D0$INC:GM_4VECT.INC'
C----------------------------------------------------------------------
      LOGICAL GB_READ_BEGIN
      LOGICAL GB_READ
      LOGICAL GB_READ_END
      LOGICAL GB_READ_GOTO_EVENT
      LOGICAL READ_FROM_NTUPLE
C----------------------------------------------------------------------
      INTEGER KTRIGGER
      REAL    TRIGGER
      EQUIVALENCE (KTRIGGER,TRIGGER)
C
      INTEGER KQUALITY
      REAL    QUALITY
      EQUIVALENCE (KQUALITY,QUALITY)
C
      INTEGER STATUS
      INTEGER LUN, LUNOUT, SSUNIT, DEBUG_LEVEL, MAX_IPRINT
      INTEGER IRUN, IEVENT, IOBJECT, I, J, K, L, IPRINT
      INTEGER NTUPLE_ID, IJET,IALLJET,IREC,IEVT,JEVT,KEVT
      PARAMETER( NTUPLE_ID= 1 )
C
      INTEGER INUMBER
      CHARACTER*8  LABEL,NAME
      CHARACTER*80 STRING
C
      CHARACTER*(*) FILENAME, TOP_DIRECTORY
      PARAMETER( FILENAME = 'NTUPLE' )
      PARAMETER( TOP_DIRECTORY = 'NTUPLE' )
      INTEGER USER_ID, RECORD_LENGTH
      PARAMETER( USER_ID  = 1 )
      PARAMETER( RECORD_LENGTH = 8191 )
      LOGICAL NEWFILE,LIST_PARTONS,GOTO_EVENT,ACTIVE
C----------------------------------------------------------------------
      REAL    XTUPLE(MAXVAR), CONE_SIZE, VALUE
      SAVE IEVT, JEVT, IREC
C----------------------------------------------------------------------
      GB_READ = .TRUE.
C
C ****  ZERO ALL OBJECT COUNTERS
C
      NVERTEX   = 0
      NPHOTON   = 0
      NELECTRON = 0
      NMUON     = 0
      NTAU      = 0
      NJET      = 0
      NALLJET   = 0
      NETMISS   = 0
      NPARTON   = 0
      NPETMISS  = 0
      NGLOBAL   = 0
      NOBJECT   = 0
      NRECO     = 0
      NPART     = 0
      IJET      = 0
      IALLJET   = 0
C
      IOBJECT = 0
      DO WHILE ( IOBJECT .NE. ID_EVENT )
C
        CALL VZERO(XTUPLE,MAXVAR)
C
        ACTIVE  = .TRUE.
        DO WHILE ( ACTIVE )
          IREC = IREC + 1
          CALL NTUPLE_GET('NTUPLE',NTUPLE_ID,IREC,XTUPLE,STATUS)
C
C ****  CHECK FOR END-OF-DATA and SET END_PROCESSING FLAG to FALSE.
C
          IF ( STATUS .NE. 0 ) THEN
            CALL FLGSET('END_PROCESSING',.TRUE.)
            GB_READ = .FALSE.
            GOTO 999
          ENDIF
C
C ****  Check for VERTEX OBJECT
C
          IOBJECT = XTUPLE(JOBJECT)
          IF ( IOBJECT .EQ. ID_VERTEX ) THEN
C
C ****  UPDATE EVENT COUNTER
C
            INUMBER = XTUPLE(JNUMBER)
            IF ( INUMBER .EQ. 1 ) THEN
              IRUN   = XTUPLE(JRUN)
              IEVENT = XTUPLE(JEVENT)
              IEVT   = IEVT + 1
C
C ****  Load RUN/EVENT numbers into HEAD bank
C
              IF ( LHEAD .LE. 0 ) THEN
                CALL BKHEAD
              ENDIF
              IF ( LHEAD .GT. 0 ) THEN
                IQ(LHEAD+1) = 1005
                IQ(LHEAD+6) = IRUN
                IQ(LHEAD+9) = IEVT
              ENDIF
C
              IF ( GOTO_EVENT ) THEN
                IF ( IEVT .EQ. JEVT ) THEN
                  GOTO_EVENT = .FALSE.
                  ACTIVE = .FALSE.
                ENDIF
              ELSE
                ACTIVE = .FALSE.
              ENDIF
C
            ENDIF
          ELSE
C
C ****  Exit immediately if GOTO_EVENT is false
C
            IF ( .NOT. GOTO_EVENT ) THEN
              ACTIVE = .FALSE.   !EXIT  - IMPORTANT!!
            ENDIF
          ENDIF
        ENDDO
C
C ****  ACCUMULATE OBJECTS IN BOTH GM_4VECT and GM_EVENT BUFFERs
C
        IF ( IOBJECT .EQ. ID_VERTEX ) THEN
C
          NVERTEX   = XTUPLE(JCOUNT)
          INUMBER   = XTUPLE(JNUMBER)
          CALL UCOPY(XTUPLE(JPX),VERTEX(INUMBER).RDUM(1),MAXSIZE)
C
          IF ( LIST_PARTONS .AND. (INUMBER .EQ. 1) ) THEN
            IF ( IPRINT .LT. MAX_IPRINT ) THEN
              IPRINT = IPRINT + 1
C
              WRITE
     &              (LUN,'('' ------------------ EVENT -> '',2I10)')
     &              IRUN, IEVENT
              WRITE(LUN,'(1X,2X,
     &                       ''      PX'',
     &                       ''      PY'',
     &                       ''      PZ'',
     &                       ''       E'',
     &                       ''     ETA'',
     &                       ''     PHI'',2X,
     &                       ''PARTON  '',2X,
     &                       ''PARENT  '',2X,
     &                       ''FAMILY'')')
              IF ( DEBUG_LEVEL .LT. 0 ) THEN
                WRITE
     &                (6,'('' ------------------ EVENT -> '',2I10)')
     &                IRUN, IEVENT
                WRITE(6,'(1X,2X,
     &                       ''      PX'',
     &                       ''      PY'',
     &                       ''      PZ'',
     &                       ''       E'',
     &                       ''     ETA'',
     &                       ''     PHI'',2X,
     &                       ''PARTON  '',2X,
     &                       ''PARENT  '',2X,
     &                       ''FAMILY'')')
              ENDIF
            ENDIF
          ENDIF
C
        ELSEIF ( IOBJECT .EQ. ID_PARTON   ) THEN
C
C ****  LOAD 4VECT buffer
C
          NPARTON = XTUPLE(JCOUNT)
          NPART   = NPARTON
          NOBJECT = NOBJECT + 1
          PARTID(NOBJECT) = XTUPLE(JQUALITY+1)
          PARENT(NOBJECT) = XTUPLE(JQUALITY+3)
          ORIGIN(NOBJECT) = XTUPLE(JQUALITY+5)
          P(1,NOBJECT)    = XTUPLE(JPX)
          P(2,NOBJECT)    = XTUPLE(JPY)
          P(3,NOBJECT)    = XTUPLE(JPZ)
          P(4,NOBJECT)    = XTUPLE(JE)
C
C ****  LOAD EVENT buffer
C
          INUMBER = XTUPLE(JNUMBER)
          CALL UCOPY(XTUPLE(JPX),PARTON(INUMBER).RDUM(1),MAXSIZE)
          PARTON(INUMBER).ID     = XTUPLE(JQUALITY+1)
          PARTON(INUMBER).PN     = XTUPLE(JQUALITY+2)
          PARTON(INUMBER).PARENT = XTUPLE(JQUALITY+3)
          PARTON(INUMBER).FN     = XTUPLE(JQUALITY+4)
          PARTON(INUMBER).FAMILY = XTUPLE(JQUALITY+5)
C
          IF ( LIST_PARTONS ) THEN
            IF ( IPRINT .LT. MAX_IPRINT ) THEN
C
              WRITE(LUN,'(I3,6F8.2,2X,A8,I2,A8,I2,A6)') INUMBER,
     &              PARTON(INUMBER).PX,
     &              PARTON(INUMBER).PY,
     &              PARTON(INUMBER).PZ,
     &              PARTON(INUMBER).E,
     &              PARTON(INUMBER).ETA,
     &              PARTON(INUMBER).PHI,
     &              LABEL(PARTON(INUMBER).ID),
     &              PARTON(INUMBER).PN,
     &              LABEL(PARTON(INUMBER).PARENT),
     &              PARTON(INUMBER).FN,
     &              LABEL(PARTON(INUMBER).FAMILY)
C
              IF ( DEBUG_LEVEL .LT. 0 ) THEN
                WRITE(6,'(I3,6F8.2,2X,A8,I2,A8,I2,A6)') INUMBER,
     &                PARTON(INUMBER).PX,
     &                PARTON(INUMBER).PY,
     &                PARTON(INUMBER).PZ,
     &                PARTON(INUMBER).E,
     &                PARTON(INUMBER).ETA,
     &                PARTON(INUMBER).PHI,
     &                LABEL(PARTON(INUMBER).ID),
     &                PARTON(INUMBER).PN,
     &                LABEL(PARTON(INUMBER).PARENT),
     &                PARTON(INUMBER).FN,
     &                LABEL(PARTON(INUMBER).FAMILY)
              ENDIF
            ENDIF
          ENDIF
C
        ELSEIF ( IOBJECT .EQ. ID_PETMISS  ) THEN
C
          NPETMISS  = XTUPLE(JCOUNT)
          INUMBER   = XTUPLE(JNUMBER)
          CALL UCOPY(XTUPLE(JPX),PETMISS(INUMBER).RDUM(1),MAXSIZE)
C
        ELSEIF ( IOBJECT .EQ. ID_PEVENT   ) THEN
C
          CALL UCOPY(XTUPLE(JCOUNT),PEVENT.RDUM(1),MAXSIZE)
C
        ELSEIF ( IOBJECT .EQ. ID_PHOTON   ) THEN
C
C ****  LOAD 4VECT buffer
C
          NOBJECT = NOBJECT + 1
          PARTID(NOBJECT) = ID_PHOTON
          PARENT(NOBJECT) = 0
          ORIGIN(NOBJECT) = 0
          P(1,NOBJECT)    = XTUPLE(JPX)
          P(2,NOBJECT)    = XTUPLE(JPY)
          P(3,NOBJECT)    = XTUPLE(JPZ)
          P(4,NOBJECT)    = XTUPLE(JE)
C
          NPHOTON   = XTUPLE(JCOUNT)
          INUMBER   = XTUPLE(JNUMBER)
          CALL UCOPY(XTUPLE(JPX),PHOTON(INUMBER).RDUM(1),MAXSIZE)
C
        ELSEIF ( IOBJECT .EQ. ID_ELECTRON ) THEN
C
C ****  LOAD 4VECT buffer
C
          NOBJECT = NOBJECT + 1
          PARTID(NOBJECT) = ID_ELECTRON
          PARENT(NOBJECT) = 0
          ORIGIN(NOBJECT) = 0
          P(1,NOBJECT)    = XTUPLE(JPX)
          P(2,NOBJECT)    = XTUPLE(JPY)
          P(3,NOBJECT)    = XTUPLE(JPZ)
          P(4,NOBJECT)    = XTUPLE(JE)
C
          NELECTRON = XTUPLE(JCOUNT)
          INUMBER   = XTUPLE(JNUMBER)
          CALL UCOPY(XTUPLE(JPX),ELECTRON(INUMBER).RDUM(1),MAXSIZE)
C
        ELSEIF ( IOBJECT .EQ. ID_MUON     ) THEN
C
C ****  LOAD 4VECT buffer
C
          NOBJECT = NOBJECT + 1
          PARTID(NOBJECT) = ID_MUON
          PARENT(NOBJECT) = 0
          ORIGIN(NOBJECT) = 0
          P(1,NOBJECT)    = XTUPLE(JPX)
          P(2,NOBJECT)    = XTUPLE(JPY)
          P(3,NOBJECT)    = XTUPLE(JPZ)
          P(4,NOBJECT)    = XTUPLE(JE)
C
          NMUON     = XTUPLE(JCOUNT)
          INUMBER   = XTUPLE(JNUMBER)
          CALL UCOPY(XTUPLE(JPX),MUON(INUMBER).RDUM(1),MAXSIZE)
C
C ****  Convert from floating point representation to integer
C
          MUON(INUMBER).NCD  = XTUPLE(JPX+12)
          MUON(INUMBER).IFW4 = XTUPLE(JPX+15)
C
        ELSEIF ( IOBJECT .EQ. ID_TAU      ) THEN
C
C ****  LOAD 4VECT buffer
C
          NOBJECT = NOBJECT + 1
          PARTID(NOBJECT) = ID_TAU
          PARENT(NOBJECT) = 0
          ORIGIN(NOBJECT) = 0
          P(1,NOBJECT)    = XTUPLE(JPX)
          P(2,NOBJECT)    = XTUPLE(JPY)
          P(3,NOBJECT)    = XTUPLE(JPZ)
          P(4,NOBJECT)    = XTUPLE(JE)
C
          NTAU      = XTUPLE(JCOUNT)
          INUMBER   = XTUPLE(JNUMBER)
          CALL UCOPY(XTUPLE(JPX),TAU(INUMBER).RDUM(1),MAXSIZE)
C
        ELSEIF ( IOBJECT .EQ. ID_JET      ) THEN
          INUMBER   = XTUPLE(JNUMBER)
C
C ****  LOAD 4VECT buffer
C
          NOBJECT = NOBJECT + 1
          PARTID(NOBJECT) = ID_JET
          PARENT(NOBJECT) = INUMBER
          ORIGIN(NOBJECT) = 10.0*XTUPLE(JX13) !ConeSize
          P(1,NOBJECT)    = XTUPLE(JPX)
          P(2,NOBJECT)    = XTUPLE(JPY)
          P(3,NOBJECT)    = XTUPLE(JPZ)
          P(4,NOBJECT)    = XTUPLE(JE)
C
          IF ( INUMBER .EQ. 1 ) THEN
            NJET = NJET + IFIX(XTUPLE(JCOUNT))
          ENDIF
          IJET = IJET + 1
          CALL UCOPY(XTUPLE(JPX),JET(IJET).RDUM(1),MAXSIZE)
C
          JET(IJET).NTRACKS = JET(IJET).IDUM(JX11)
          JET(IJET).NTOWERS = JET(IJET).IDUM(JX12)
          JET(IJET).FLAG    = JET(IJET).IDUM(JX6)
          JET(IJET).NCELLS  = JET(IJET).IDUM(JX7)
C
        ELSEIF ( IOBJECT .EQ. ID_ALLJET   ) THEN
C
          INUMBER   = XTUPLE(JNUMBER)
          IF ( INUMBER .EQ. 1 ) THEN
            NALLJET = NALLJET + IFIX(XTUPLE(JCOUNT))
          ENDIF
          IALLJET = IALLJET + 1
          CALL UCOPY(XTUPLE(JPX),ALLJET(IALLJET).RDUM(1),MAXSIZE)
C
          ALLJET(IALLJET).NTRACKS = ALLJET(IALLJET).IDUM(JX11)
          ALLJET(IALLJET).NTOWERS = ALLJET(IALLJET).IDUM(JX12)
          ALLJET(IALLJET).FLAG    = ALLJET(IALLJET).IDUM(JX6)
          ALLJET(IALLJET).NCELLS  = ALLJET(IALLJET).IDUM(JX7)
C
        ELSEIF ( IOBJECT .EQ. ID_ETMISS   ) THEN
          NETMISS   = XTUPLE(JCOUNT)
          INUMBER   = XTUPLE(JNUMBER)
C
C ****  LOAD 4VECT buffer
C
          NOBJECT = NOBJECT + 1
          PARTID(NOBJECT) = ID_ETMISS
          PARENT(NOBJECT) = INUMBER
          ORIGIN(NOBJECT) = INUMBER
          P(1,NOBJECT)    = XTUPLE(JPX)
          P(2,NOBJECT)    = XTUPLE(JPY)
          P(3,NOBJECT)    = XTUPLE(JPZ)
          P(4,NOBJECT)    = XTUPLE(JE)
C
          CALL UCOPY(XTUPLE(JPX),ETMISS(INUMBER).RDUM(1),MAXSIZE)
C
        ELSEIF ( IOBJECT .EQ. ID_EVENT    ) THEN
C
          EVENT.RUN   = IRUN
          EVENT.EVENT = IEVENT
          EVENT.OBJECT= IOBJECT
          EVENT.COUNT = XTUPLE(JCOUNT)
          CALL UCOPY(XTUPLE(JNUMBER),EVENT.RDUM(5),MAXSIZE+1)
C
        ELSEIF ( IOBJECT .EQ. ID_GLOBAL     ) THEN
C
          NGLOBAL   = XTUPLE(JCOUNT)
          CALL UCOPY(XTUPLE(JCOUNT+1),GLOBAL.RDUM(1),MAXSIZE)
C
        ELSE
C
          WRITE(STRING(1:10),'(I10)') IOBJECT
          STRING = 'Invalid Object ID '//STRING(1:10)
          CALL ERRMSG('WRONG_OBJECT_ID','GB_READ',STRING,'W')
C
        ENDIF
      ENDDO
      NRECO = NOBJECT - NPART
      RETURN
C
      ENTRY GB_READ_BEGIN()
C----------------------------------------------------------------------
C-
C----------------------------------------------------------------------
      GB_READ_BEGIN = .TRUE.
C
C ****    Open ntuple file
C
      NEWFILE       = .FALSE.
      CALL NTUPLE_FILE_OPEN (USER_ID,NEWFILE,FILENAME,
     &                         RECORD_LENGTH,TOP_DIRECTORY,STATUS)
      IF ( STATUS .NE. 0 ) THEN
        CALL ERRMSG('OPEN_ERROR','GB_READ_BEGIN',
     &        'Unable to open file NTUPLE','F')
      ENDIF
C
C ****  Check for debug mode
C
      CALL TRNLNM('LIST_PARTONS',STRING,L)
      LIST_PARTONS = STRING(1:L) .NE. 'LIST_PARTONS'
      IF ( .NOT. LIST_PARTONS ) THEN
        CALL TRNLNM('DEB_OBJECT_PARTON',STRING,L)
        LIST_PARTONS = STRING(1:L) .NE. 'DEB_OBJECT_PARTON'
      ENDIF
C
      IF ( LIST_PARTONS ) THEN
        DEBUG_LEVEL = VALUE(STRING(1:L),I,J,K)
        MAX_IPRINT  = IABS(DEBUG_LEVEL)
        LUN         = SSUNIT()
      ENDIF
C
C ****  INITIALIZE EVENT COUNTER
C
      IEVT    = 0
      IREC    = 0
      IPRINT  = 0
C
      RETURN
C
      ENTRY GB_READ_GOTO_EVENT(KEVT)
      JEVT = KEVT
      GOTO_EVENT = .TRUE.
C
C ****  If event requested is earlier than the current event then
C ****  rewind to the start
C
      IF ( JEVT .LE. IEVT ) THEN
        IEVT = 0
        IREC = 0
      ENDIF
      RETURN
C
      ENTRY GB_READ_END()
      GB_READ_END = .TRUE.
      CALL NTUPLE_CLOSE('NTUPLE',STATUS)
  999 RETURN
      END

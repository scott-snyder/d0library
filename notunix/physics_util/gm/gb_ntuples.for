      FUNCTION GB_NTUPLES()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build Global Event Monitoring ntuple from
C-   DST banks.
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  25-OCT-1992   Harrison B. Prosper
C-   Updated  17-NOV-1992   Harrison B. Prosper
C-    Alter muon data
C-   Updated   1-DEC-1992   Harrison B. Prosper
C-    Use OBJECT_xxxx entry points
C-   Updated   7-DEC-1992   Harrison B. Prosper
C-    Add call to L2_TRIGGER_WORD
C-   Updated  14-DEC-1992   Harrison B. Prosper
C-    Add HEAD info into END-OF-EVENT object
C-   Updated   3-FEB-1993   K. Wyatt Merritt
C-    Make separate JET and JNEP objects
C-   Updated  23-FEB-1993   Harrison B. Prosper
C-    Skip bounds calculation in HFN for word JPX+16 (IFW1 word in
C-    OBJECT_MUON)
C-   Updated  23-FEB-1993   Harrison B. Prosper
C-    INRCP CLEANEM and ZTRAKS
C-   Updated  28-FEB-1993   Harrison B. Prosper
C-    Use OBJECT_ALLJET (id=7); introduce WRITE_object flags
C-   Updated   1-MAR-1993   Harrison B. Prosper
C-    Add OBJECT_PARTON
C-   Updated  16-MAR-1993   Harrison B. Prosper
C-    Fix vertex count, pevent
C-   Updated  21-APR-1993   Harrison B. Prosper
C-    Use OBJECT_EVENT
C-   Updated  29-APR-1993   Harrison B. Prosper
C-    Add object GLOBAL
C-   Updated   3-MAY-1993   Harrison B. Prosper
C-    Add GB_NTUPLES_RCP optional overwrite
C-   Updated  18-MAY-1993   Stan M. Krzywdzinski, Harrison B. Prosper
C-    Increase size of ntuple
C-   Updated  28-JAN-1994   Harrison B. Prosper
C-    Use direct HBOOK calls
C-    Use (for now) modified HFN which skips bound calculation
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:GM_PARAMS.DEF'
      INCLUDE 'D0$INC:QUEST.INC'
C----------------------------------------------------------------------
      LOGICAL GB_NTUPLES_BEGIN
      LOGICAL GB_NTUPLES
      LOGICAL GB_NTUPLES_END
C----------------------------------------------------------------------
      INTEGER NTUPLE_ID
      PARAMETER( NTUPLE_ID =  1)
      INTEGER IUSER
      PARAMETER( IUSER  = 11)
      INTEGER MAXJET
      PARAMETER( MAXJET = 20)
C----------------------------------------------------------------------
      LOGICAL EZERROR, FOUND, KEEP_EVENT, DO_EDIT, DISPLAY_VERSION
      LOGICAL ZBANK_EDIT, SELECT_EVENTS_BY_FILTER
      LOGICAL WRITE_VERTEX
      LOGICAL WRITE_PHOTON
      LOGICAL WRITE_ELECTRON
      LOGICAL WRITE_MUON
      LOGICAL WRITE_TAU
      LOGICAL WRITE_JET
      LOGICAL WRITE_ETMISS
      LOGICAL WRITE_ALLJET
      LOGICAL WRITE_EVENT
      LOGICAL WRITE_PEVENT
      LOGICAL WRITE_PARTON
      LOGICAL WRITE_TRUE_ETMISS
      LOGICAL WRITE_GLOBAL
C
      INTEGER KTRIGGER
      REAL    TRIGGER
      EQUIVALENCE (KTRIGGER,TRIGGER)
C
      INTEGER JET, NJET_ALGORITHMS, NALLJET_ALGORITHMS
      INTEGER STATUS, NELEC, NMUON, NPHOT, NJET, NETMISS
      INTEGER LUN, I, J, K, L, II, JJ, ICYCLE
      INTEGER RUNNO, EVONUM, RUN, EVENT, NSIZE
      INTEGER NUMBER, NOBJECTS, LREC, NPRIME
C
      REAL    XTUPLE(MAXVAR)
      REAL    CONESIZE_JET(MAXJET), CONESIZE_ALLJET(MAXJET)
      CHARACTER*4  CHOPT
      CHARACTER*8  CHTAGS(MAXVAR)
      CHARACTER*16 TYPE_ALLJET(MAXJET), TYPE_JET(MAXJET)
      CHARACTER*72 VERSION, VPHYSICS_UTIL
      DATA CHTAGS /
     & 'RUN'      ,'EVENT'    ,'TRIGGER'  ,'OBJECT'   ,'COUNT'
     &,'NUMBER'   ,'PX'       ,'PY'       ,'PZ'       ,'E'
     &,'ET'       ,'ETA'      ,'PHI'      ,'DETA'     ,'QUALITY'
     &,'X1'       ,'X2'       ,'X3'       ,'X4'       ,'X5'
     &,'X6'       ,'X7'       ,'X8'       ,'X9'       ,'X10'
     &,'X11'      ,'X12'      ,'X13'      ,'X14'      ,'X15'
     &,'X16'      ,'X17'      ,'X18'      ,'X19'      ,'X20'
     & /
C----------------------------------------------------------------------
      GB_NTUPLES = .TRUE.
C
C ****  Apply cuts defined in RCP file
C
C      IF ( DO_EDIT ) THEN
C        KEEP_EVENT  = ZBANK_EDIT('GB_NTUPLES_RCP')
C        IF ( .NOT. KEEP_EVENT ) THEN
C          GB_NTUPLES = .FALSE.
C          GOTO 999
C        ENDIF
C      ENDIF
C
C ****  INITIALIZE
C
      NOBJECTS  = 0
      RUN       = RUNNO()
      EVENT     = EVONUM()
C
C ****  Get TRIGGER mask
C
      KTRIGGER = 0
      CALL L2_TRIGGER_WORD(KTRIGGER)
C
C ****  Select on LEVEL 2 Triggers
C
      IF ( SELECT_EVENTS_BY_FILTER ) THEN
        IF ( KTRIGGER .EQ. 0 ) THEN
          GB_NTUPLES = .FALSE.
          GOTO 999
        ENDIF
      ENDIF
C
C ****  Work in PAWC
C
      CALL HCDIR('//PAWC',' ')
C
C
C ****  OBJECT: MAIN VERTEX
C ****  SAVE at least ONE vertex object
C
      IF ( WRITE_VERTEX ) THEN
        CALL NOBJ_VERTEX(NUMBER,NSIZE)
        IF ( NUMBER .LE. 0 ) THEN
          NUMBER = 1
        ENDIF
        NSIZE = MIN(NSIZE,MAXSIZE)
C
        CALL VZERO(XTUPLE,MAXVAR)
        XTUPLE(JRUN)     = RUN
        XTUPLE(JEVENT)   = EVENT
        XTUPLE(JTRIGGER) = TRIGGER
        XTUPLE(JOBJECT)  = ID_VERTEX
        XTUPLE(JCOUNT)   = NUMBER
        XTUPLE(JNUMBER)  = 1
C
        IF ( NUMBER .LE. 0 ) THEN
          NOBJECTS = NOBJECTS + 1
          CALL HFN(NTUPLE_ID,XTUPLE)
        ELSE
C
          DO I =  1, NUMBER
            II = I
            XTUPLE(JNUMBER)  = II
            CALL OBJECT_VERTEX(II,NSIZE,XTUPLE(JPX))
C
            NOBJECTS = NOBJECTS + 1
            CALL HFN(NTUPLE_ID,XTUPLE)
          ENDDO
        ENDIF
      ENDIF
C
C ****  OBJECT: PARTON
C
      IF ( WRITE_PARTON ) THEN
        CALL NOBJ_PARTONS(NUMBER,NSIZE)
C
        IF ( NUMBER .GT. 0 ) THEN
          NSIZE = MIN(NSIZE,MAXSIZE)
          CALL VZERO(XTUPLE,MAXVAR)
          XTUPLE(JRUN)     = RUN
          XTUPLE(JEVENT)   = EVENT
          XTUPLE(JTRIGGER) = TRIGGER
          XTUPLE(JOBJECT)  = ID_PARTON
          XTUPLE(JCOUNT)   = NUMBER
C
          DO I =  1, NUMBER
            II = I
            XTUPLE(JNUMBER) = II
            CALL OBJECT_PARTON(II,NSIZE,XTUPLE(JPX))
C
            NOBJECTS = NOBJECTS + 1
            CALL HFN(NTUPLE_ID,XTUPLE)
          ENDDO
        ENDIF
      ENDIF
C
C ****  OBJECT: TRUE MISSING ET
C
      IF ( WRITE_TRUE_ETMISS ) THEN
        CALL NOBJ_TRUE_MISSING_ET(NUMBER,NSIZE)
C
        IF ( NUMBER .GT. 0 ) THEN
          NSIZE = MIN(NSIZE,MAXSIZE)
          CALL VZERO(XTUPLE,MAXVAR)
          XTUPLE(JRUN)     = RUN
          XTUPLE(JEVENT)   = EVENT
          XTUPLE(JTRIGGER) = TRIGGER
          XTUPLE(JOBJECT)  = ID_PETMISS
          XTUPLE(JCOUNT)   = NUMBER
C
          DO I =  1, NUMBER
            II = I
            XTUPLE(JNUMBER) = II
            CALL OBJECT_TRUE_MISSING_ET(II,NSIZE,XTUPLE(JPX))
C
            NOBJECTS = NOBJECTS + 1
            CALL HFN(NTUPLE_ID,XTUPLE)
          ENDDO
        ENDIF
      ENDIF
C
C ****  OBJECT: PEVENT
C
      IF ( WRITE_PEVENT ) THEN
        CALL NOBJ_PEVENTS(NUMBER,NSIZE)
C
        IF ( NUMBER .GT. 0 ) THEN
          NSIZE = MIN(NSIZE,MAXSIZE)
          CALL VZERO(XTUPLE,MAXVAR)
          NOBJECTS = NOBJECTS + 1
C
          CALL VZERO(XTUPLE,MAXVAR)
          XTUPLE(JRUN)     = RUN
          XTUPLE(JEVENT)   = EVENT
          XTUPLE(JTRIGGER) = TRIGGER
          XTUPLE(JOBJECT)  = ID_PEVENT
          XTUPLE(JCOUNT)   = NOBJECTS
          CALL OBJECT_PEVENT(1,NSIZE,XTUPLE(JCOUNT+1))
          CALL HFN(NTUPLE_ID,XTUPLE)
        ENDIF
      ENDIF
C
C ****  OBJECT: PHOTON
C
      IF ( WRITE_PHOTON ) THEN
        CALL NOBJ_PHOTONS(NUMBER,NSIZE)
C
        IF ( NUMBER .GT. 0 ) THEN
          NSIZE = MIN(NSIZE,MAXSIZE)
          CALL VZERO(XTUPLE,MAXVAR)
          XTUPLE(JRUN)     = RUN
          XTUPLE(JEVENT)   = EVENT
          XTUPLE(JTRIGGER) = TRIGGER
          XTUPLE(JOBJECT)  = ID_PHOTON
          XTUPLE(JCOUNT)   = NUMBER
C
          DO I =  1, NUMBER
            II = I
            XTUPLE(JNUMBER) = II
            CALL OBJECT_PHOTON(II,NSIZE,XTUPLE(JPX))
C
            NOBJECTS = NOBJECTS + 1
            CALL HFN(NTUPLE_ID,XTUPLE)
          ENDDO
        ENDIF
      ENDIF
C
C ****  OBJECT: ELECTRON
C
      IF ( WRITE_ELECTRON ) THEN
        CALL NOBJ_ELECTRONS(NUMBER,NSIZE)
C
        IF ( NUMBER .GT. 0 ) THEN
          NSIZE = MIN(NSIZE,MAXSIZE)
          CALL VZERO(XTUPLE,MAXVAR)
          XTUPLE(JRUN)     = RUN
          XTUPLE(JEVENT)   = EVENT
          XTUPLE(JTRIGGER) = TRIGGER
          XTUPLE(JOBJECT)  = ID_ELECTRON
          XTUPLE(JCOUNT)   = NUMBER
C
          DO I =  1, NUMBER
            II = I
            XTUPLE(JNUMBER) = II
            CALL OBJECT_ELECTRON(II,NSIZE,XTUPLE(JPX))
C
            NOBJECTS = NOBJECTS + 1
            CALL HFN(NTUPLE_ID,XTUPLE)
          ENDDO
        ENDIF
      ENDIF
C
C ****  OBJECT: MUON
C
      IF ( WRITE_MUON ) THEN
        CALL NOBJ_MUONS(NUMBER,NSIZE)
C
        IF ( NUMBER .GT. 0 ) THEN
          NSIZE = MIN(NSIZE,MAXSIZE)
          CALL VZERO(XTUPLE,MAXVAR)
          XTUPLE(JRUN)     = RUN
          XTUPLE(JEVENT)   = EVENT
          XTUPLE(JTRIGGER) = TRIGGER
          XTUPLE(JOBJECT)  = ID_MUON
          XTUPLE(JCOUNT)   = NUMBER
C
          DO I =  1, NUMBER
            II = I
            XTUPLE(JNUMBER) = II
            CALL OBJECT_MUON(II,NSIZE,XTUPLE(JPX))
C
            NOBJECTS = NOBJECTS + 1
            CALL HFN(NTUPLE_ID,XTUPLE)
          ENDDO
        ENDIF
      ENDIF
C
C ****  OBJECT: TAU
C
      IF ( WRITE_TAU ) THEN
        CALL NOBJ_TAUS(NUMBER,NSIZE)
C
        IF ( NUMBER .GT. 0 ) THEN
          NSIZE = MIN(NSIZE,MAXSIZE)
          CALL VZERO(XTUPLE,MAXVAR)
          XTUPLE(JRUN)     = RUN
          XTUPLE(JEVENT)   = EVENT
          XTUPLE(JTRIGGER) = TRIGGER
          XTUPLE(JOBJECT)  = ID_TAU
          XTUPLE(JCOUNT)   = NUMBER
C
          DO I =  1, NUMBER
            II = I
            XTUPLE(JNUMBER) = II
            CALL OBJECT_TAU(II,NSIZE,XTUPLE(JPX))
C
            NOBJECTS = NOBJECTS + 1
            CALL HFN(NTUPLE_ID,XTUPLE)
          ENDDO
        ENDIF
      ENDIF
C
C ****  OBJECT: JET (with JNEP applied)
C
      IF ( WRITE_JET ) THEN
        DO JET = 1, NJET_ALGORITHMS
          CALL SET_JET(TYPE_JET(JET),CONESIZE_JET(JET),STATUS)
C
          IF ( STATUS .GE. 0 ) THEN
C
            CALL NOBJ_JETS(NUMBER,NSIZE)
C
            IF ( NUMBER .GT. 0 ) THEN
              NSIZE = MIN(NSIZE,MAXSIZE)
              CALL VZERO(XTUPLE,MAXVAR)
              XTUPLE(JRUN)     = RUN
              XTUPLE(JEVENT)   = EVENT
              XTUPLE(JTRIGGER) = TRIGGER
              XTUPLE(JOBJECT)  = ID_JET
              XTUPLE(JCOUNT)   = NUMBER
C
              DO I =  1, NUMBER
                II = I
                XTUPLE(JNUMBER) = II
                CALL OBJECT_JET(II,NSIZE,XTUPLE(JPX))
C
                NOBJECTS = NOBJECTS + 1
                CALL HFN(NTUPLE_ID,XTUPLE)
              ENDDO
            ENDIF
            CALL RESET_JET
          ENDIF
        ENDDO
      ENDIF
C
C ****  OBJECT: ETMISS
C
      IF ( WRITE_ETMISS ) THEN
        CALL NOBJ_MISSING_ET(NUMBER,NSIZE)
C
        IF ( NUMBER .GT. 0 ) THEN
          NSIZE = MIN(NSIZE,MAXSIZE)
          CALL VZERO(XTUPLE,MAXVAR)
          XTUPLE(JRUN)     = RUN
          XTUPLE(JEVENT)   = EVENT
          XTUPLE(JTRIGGER) = TRIGGER
          XTUPLE(JOBJECT)  = ID_ETMISS
          XTUPLE(JCOUNT)   = NUMBER
C
          DO I =  1, NUMBER
            II = I
            XTUPLE(JNUMBER) = II
            CALL OBJECT_MISSING_ET(II,NSIZE,XTUPLE(JPX))
C
            NOBJECTS = NOBJECTS + 1
            CALL HFN(NTUPLE_ID,XTUPLE)
          ENDDO
        ENDIF
      ENDIF
C
C ****  OBJECT: ALLJET
C
      IF ( WRITE_ALLJET ) THEN
C
        DO JET = 1, NALLJET_ALGORITHMS
          CALL SET_JET(TYPE_ALLJET(JET),CONESIZE_ALLJET(JET),STATUS)
C
          IF ( STATUS .GE. 0 ) THEN
C
            CALL NOBJ_ALLJETS(NUMBER,NSIZE)
C
            IF ( NUMBER .GT. 0 ) THEN
              NSIZE = MIN(NSIZE,MAXSIZE)
              CALL VZERO(XTUPLE,MAXVAR)
              XTUPLE(JRUN)     = RUN
              XTUPLE(JEVENT)   = EVENT
              XTUPLE(JTRIGGER) = TRIGGER
              XTUPLE(JOBJECT)  = ID_ALLJET
              XTUPLE(JCOUNT)   = NUMBER
C
              DO I =  1, NUMBER
                II = I
                XTUPLE(JNUMBER) = II
                CALL OBJECT_ALLJET(II,NSIZE,XTUPLE(JPX))
C
                NOBJECTS = NOBJECTS + 1
                CALL HFN(NTUPLE_ID,XTUPLE)
              ENDDO
            ENDIF
            CALL RESET_JET
          ENDIF
        ENDDO
      ENDIF
C
C ****  OBJECT: GLOBAL
C
      IF ( WRITE_GLOBAL ) THEN
        CALL NOBJ_GLOBALS(NUMBER,NSIZE)
C
        IF ( NUMBER .GT. 0 ) THEN
          NSIZE = MIN(NSIZE,MAXSIZE)
          CALL VZERO(XTUPLE,MAXVAR)
C
          NOBJECTS = NOBJECTS + 1
          XTUPLE(JRUN)     = RUN
          XTUPLE(JEVENT)   = EVENT
          XTUPLE(JTRIGGER) = TRIGGER
          XTUPLE(JOBJECT)  = ID_GLOBAL
          XTUPLE(JCOUNT)   = 1
          CALL OBJECT_GLOBAL(1,NSIZE,XTUPLE(JCOUNT+1))
          CALL HFN(NTUPLE_ID,XTUPLE)
        ENDIF
      ENDIF
C
C ****  END-OF-EVENT MARKER
C
      IF ( WRITE_EVENT ) THEN
        CALL NOBJ_EVENTS(NUMBER,NSIZE)
C
        IF ( NUMBER .GT. 0 ) THEN
          NSIZE = MIN(NSIZE,MAXSIZE)
          CALL VZERO(XTUPLE,MAXVAR)
C
          NOBJECTS = NOBJECTS + 1
          XTUPLE(JRUN)     = RUN
          XTUPLE(JEVENT)   = EVENT
          XTUPLE(JTRIGGER) = TRIGGER
          XTUPLE(JOBJECT)  = ID_EVENT
          XTUPLE(JCOUNT)   = NOBJECTS
          CALL OBJECT_EVENT(1,NSIZE,XTUPLE(JCOUNT+1))
          CALL HFN(NTUPLE_ID,XTUPLE)
        ENDIF
      ENDIF
      RETURN
C
      ENTRY GB_NTUPLES_BEGIN()
C----------------------------------------------------------------------
C-
C----------------------------------------------------------------------
      GB_NTUPLES_BEGIN = .TRUE.
C
      WRITE(6,'('' '')')
      WRITE(6,'(''    WELCOME TO THE GM PROGRAM'')')
C
C ****  Get trigger selection switch
C
      CALL EZPICK('GB_TRIGGER_RCP')
      IF ( EZERROR(STATUS) ) THEN
        SELECT_EVENTS_BY_FILTER = .FALSE.
      ELSE
        CALL EZGET('SELECT_EVENTS_BY_FILTER',
     &              SELECT_EVENTS_BY_FILTER,STATUS)
        CALL EZRSET
      ENDIF
C
C ****  READ IN RCP FILE
C
      CALL INRCP('GB_NTUPLES_RCP',STATUS)
      IF ( STATUS .NE. 0 ) THEN
        CALL ERRMSG('BADREAD','GB_NTUPLES_BEGIN',
     &      'Cannot find file GB_NTUPLES_RCP','F')
      ENDIF
C
C ****  Display Version Number of Program
C
      CALL EZPICK('GB_NTUPLES_RCP')
      CALL EZGET('DISPLAY_LIBRARY_VERSION',DISPLAY_VERSION,STATUS)
      IF ( DISPLAY_VERSION ) THEN
        VERSION = VPHYSICS_UTIL()
        WRITE(6,'(''   '',A)') VERSION
      ENDIF
      CALL EZGETS('PROGRAM_VERSION',1,VERSION,L,STATUS)
      CALL EZRSET
      WRITE(6,'(''    Program Version '',A)') VERSION(1:L)
      WRITE(6,'('' '')')
C
C ****  READ IN RCPE OVERWRITE FILE
C
      CALL INRCPE('GB_NTUPLES_RCPE',STATUS)
      IF ( STATUS .EQ. 0 ) THEN
        CALL ERRMSG
     &    ('EDIT_RCP','GB_NTUPLES_BEGIN','Edit GB_NTUPLES_RCP','W')
      ENDIF
C
      CALL EZPICK('GB_NTUPLES_RCP')
C
C ****  Get EDIT switch
C
      CALL EZGET('DO_EDIT',DO_EDIT,STATUS)
C
C ****  Get object switches
C
      CALL EZGET('WRITE_VERTEX',  WRITE_VERTEX,STATUS)
      CALL EZGET('WRITE_PHOTON',  WRITE_PHOTON,STATUS)
      CALL EZGET('WRITE_ELECTRON',WRITE_ELECTRON,STATUS)
      CALL EZGET('WRITE_MUON',    WRITE_MUON,STATUS)
      CALL EZGET('WRITE_TAU',     WRITE_TAU,STATUS)
      CALL EZGET('WRITE_JET',     WRITE_JET,STATUS)
      CALL EZGET('WRITE_ETMISS',  WRITE_ETMISS,STATUS)
      CALL EZGET('WRITE_ALLJET',  WRITE_ALLJET,STATUS)
      CALL EZGET('WRITE_EVENT',   WRITE_EVENT,STATUS)
      CALL EZGET('WRITE_PEVENT',  WRITE_PEVENT,STATUS)
      CALL EZGET('WRITE_PARTON',  WRITE_PARTON,STATUS)
      CALL EZGET('WRITE_TRUE_ETMISS',  WRITE_TRUE_ETMISS,STATUS)
      CALL EZGET('WRITE_GLOBAL',  WRITE_GLOBAL,STATUS)
C
      WRITE(6,'('' '')')
      WRITE(6,'('' The following Objects will be written out'')')
      WRITE(6,'('' '')')
      IF ( WRITE_VERTEX )   WRITE(6,'(''          VERTEX'')')
      IF ( WRITE_PARTON )     WRITE(6,'(''            PARTON'')')
      IF ( WRITE_TRUE_ETMISS )WRITE(6,'(''            TRUE_ETMISS'')')
      IF ( WRITE_PEVENT )     WRITE(6,'(''            PEVENT'')')
      IF ( WRITE_PHOTON )   WRITE(6,'(''          PHOTON'')')
      IF ( WRITE_ELECTRON ) WRITE(6,'(''          ELECTRON'')')
      IF ( WRITE_MUON )     WRITE(6,'(''          MUON'')')
      IF ( WRITE_TAU )      WRITE(6,'(''          TAU'')')
      IF ( WRITE_JET )      WRITE(6,'(''          JET'')')
      IF ( WRITE_ETMISS )   WRITE(6,'(''          ETMISS'')')
      IF ( WRITE_ALLJET )   WRITE(6,'(''          ALLJET'')')
      IF ( WRITE_EVENT )    WRITE(6,'(''          EVENT'')')
      IF ( WRITE_GLOBAL )   WRITE(6,'(''          GLOBAL'')')
      WRITE(6,'('' '')')
C
C ****  GET JET TYPES
C
      NALLJET_ALGORITHMS = 0
      CALL EZGETA('CONESIZE_ALLJET',0,0,0,NALLJET_ALGORITHMS,STATUS)
      CALL EZGET ('CONESIZE_ALLJET',CONESIZE_ALLJET,STATUS)
C
      DO I =  1, NALLJET_ALGORITHMS
        IF ( CONESIZE_ALLJET(I) .GT. 0.0 ) THEN
          TYPE_JET(I) = 'CONE_JET'
        ELSE
          TYPE_JET(I) = 'NN_JET'
        ENDIF
      ENDDO
C
      NJET_ALGORITHMS = 0
      CALL EZGETA('CONESIZE_JET',0,0,0,NJET_ALGORITHMS,STATUS)
      CALL EZGET ('CONESIZE_JET',CONESIZE_JET,STATUS)
C
      DO I =  1, NJET_ALGORITHMS
        IF ( CONESIZE_JET(I) .GT. 0.0 ) THEN
          TYPE_ALLJET(I) = 'CONE_JET'
        ELSE
          TYPE_ALLJET(I) = 'NN_JET'
        ENDIF
      ENDDO
C
C ****  Say which Jet algorithm is being used
C
      IF ( WRITE_JET ) THEN
        WRITE(6,'(1X,''Jet Algorithms'')')
        DO I =  1, NJET_ALGORITHMS
          WRITE(6,'(1X,A,1X,F10.1)') TYPE_JET(I), CONESIZE_JET(I)
        ENDDO
        WRITE(6,'('' '')')
      ENDIF
C
      IF ( WRITE_ALLJET ) THEN
        WRITE(6,'(1X,''AllJet Algorithms'')')
        DO I =  1, NALLJET_ALGORITHMS
          WRITE(6,'(1X,A,1X,F10.1)') TYPE_ALLJET(I), CONESIZE_ALLJET(I)
        ENDDO
        WRITE(6,'('' '')')
      ENDIF
      CALL EZRSET
C
C
C **** Get a logical unit to open file:
C
      CALL GTUNIT(IUSER,LUN,STATUS)
      IF( STATUS.NE.0 )THEN
        CALL ERRMSG('BAD_UNIT','GB_NTUPLES',
     &              'Could not get a unit number','W')
        GOTO 999
      ENDIF
C
C ****  Open RZ file
C
      IQUEST(11) = 65000                ! the maximum allowed by RZMAKE in
      CHOPT      = 'NQ'
      LREC       = 8191
C
      CALL HROPEN(LUN,'NTUPLE','NTUPLE',CHOPT,LREC,STATUS)
      IF ( STATUS.NE.0 )THEN
        CALL ERRMSG('BAD_OPEN','GB_NTUPLES',
     &    'Cannot open NTUPLE file','F')
      ENDIF
C
C ****    Book ntuple
C
      NPRIME = MAXVAR * 1000
      CALL HCDIR('//PAWC',' ')
      CALL HBOOKN (NTUPLE_ID,
     &             'GM Ntuple',
     &             MAXVAR,
     &             'NTUPLE',
     &             NPRIME,
     &             CHTAGS)
      RETURN
C
      ENTRY GB_NTUPLES_END()
C----------------------------------------------------------------------
C-
C----------------------------------------------------------------------
      GB_NTUPLES_END = .TRUE.
C
      CALL HCDIR('//PAWC',' ')
      CALL HCDIR('//NTUPLE',' ')
      CALL HROUT(0,ICYCLE,' ')        ! Write out the last buffer
      CALL RZCLOS('NTUPLE',' ')       ! Close the direct access file
      CALL RLUNIT(IUSER,LUN,STATUS)       ! Return the logical unit
      CLOSE(UNIT=LUN)
C
  999 RETURN
      END

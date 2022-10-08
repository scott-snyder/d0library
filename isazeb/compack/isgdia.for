      LOGICAL FUNCTION ISGDIA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Handle dialog for ISAJET commands
C-
C-     ENTRY ISQUIT
C-       discard command file if given name NONE
C-
C-   Created   8-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ITAPES.INC'
      INCLUDE 'D0$INC:PRIMAR.INC'
      INCLUDE 'D0$INC:PRTOUT.INC'
      CHARACTER*56 NAMDKY              ! name of files for decay table
      CHARACTER*64 FILCOM              ! name of ISAJET command file
      CHARACTER*8 W_DECAY
      SAVE FILCOM,NAMDKY
      INTEGER IFL,I,IER,NIT,MUON,NIR,IT
      INTEGER JTDKY,JTLIS,JTCOM,JDUMY,USUNIT,IRUN,NUM
      REAL    ETCUT,DRCUT
      LOGICAL YES,OK,QPART,QCAL,QLEP,ISQUIT,FIRST,IR,IM,USE_CONE_LIM
      INTEGER ISTAT,LIBERA,LIBREP
      CHARACTER*4 ALG
      DATA NAMDKY/'D0$ISAJET:DECAY.DAT'/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C            setup units for ISAJET
C
      IF(FIRST) THEN
        CALL GTUNIT(101,JTCOM,IER)      ! reserve command unit
        CALL INTMSG(' ISAJET decay table '//NAMDKY)
        CALL GTUNIT(111,ITDKY,IER)             ! reserve decay unit
   10   CALL D0OPEN(ITDKY,NAMDKY,' ',OK)    ! open decay file
        IF ( .NOT.OK ) THEN
          CALL GETPAR1l(' Cannot open decay file. Give another? Y/N>',
     &    'L',OK)
          IF(OK) CALL GETPAR1c(' Name of file','C',NAMDKY)
          IF(OK) GOTO 10
          CALL QUITS
        ENDIF
C
C           needed units (command file and printout)
        JTLIS=USUNIT()                  ! ISAJET printouts on USER.OUT
        DATA JDUMY/-1/
        JTDKY=-ITDKY
        CALL ISAINI(JTDKY,JDUMY,JTCOM,JTLIS)
        IFL=0
        FIRST=.FALSE.
      ENDIF
C
C   dialog to chose banks generated
C
      QPART=.TRUE.    
      QCAL=.FALSE.
      QLEP=.TRUE.
      CALL OUTMSG(' Default is not to fill ISAC banks')
      YES=.FALSE.
      CALL GETPAR1l(' Change default? [N]>','L',YES)
      IF ( YES ) THEN
        YES=.TRUE.
        CALL GETPAR1l(' Fill particle banks? [Y]>','L',YES)
        QPART=YES
        YES=.FALSE.
        CALL GETPAR1l(' Fill pseudo-calorimeter banks (ISAC)? [N]>',
     &    'L',YES)
        QCAL=YES
        YES=.TRUE.
        CALL GETPAR1l(' Fill lepton banks (ISAL)? [Y]>','L',YES)
        QLEP=YES
      ENDIF
      CALL ISBKST(QPART,QCAL,QLEP)
      IRUN=1
      CALL GETPAR1(' Give a run number [1] >','I',IRUN)
      CALL ISA_SETRUN(IRUN)
      YES=.FALSE.
      CALL GETPAR1l(' Do you want to generate single track events? [N]>'
     &  ,'L',YES)
      CALL FLGSET('ONE_TRACK',YES)
      IF(.NOT.YES) THEN             ! standard event generation
C
        YES=.FALSE.
        CALL GETPAR1l(' Change defaults for PJET? [N]>','L',YES)
        IF(YES) THEN
          NUM=1
          CALL GETPAR1(' number of PJET algorithms [1]>','I',NUM)
          IT=1
          CALL GETPAR1(' algorithm type 1=CONE, 2=ANGL or 3=PART, [1]>'
     &      ,'I',IT)  
          IF(IT.EQ.1) ALG='CONE'
          IF(IT.EQ.2) ALG='ANGL'
          IF(IT.EQ.3) ALG='PART'
          ETCUT=5.
          CALL GETPAR(1,' ET cut from PJET jets [5.]>','R',ETCUT)
          DRCUT=.45
          CALL GETPAR(1,' CONE CUT (IN R OR ANGLE) [.45]>','R',DRCUT)
          NIT=1
          CALL GETPAR1(' maximum number of iterations [1]>','I',NIT)
          NIR=0
          IR=.TRUE.
          CALL GETPAR1l(' include init radiation [Y]>','L',IR)
          IF(.NOT.IR) NIR=1
          MUON=0
          IM=.FALSE.
          CALL GETPAR1l(' include muons [N]>','L',IM)
          IF(IM) MUON=1
          CALL PJPSET(NUM,ALG,ETCUT,DRCUT,NIT,NIR,MUON,0.)
        ENDIF
C   
C          specify command file 
C
        YES=.TRUE.
    1   CALL GETPAR1l(' Read an OLD command file? [Y]>','L',YES)
        IF(YES) THEN
    2     CALL GETPAR1c(' Enter old command file NAME>','C',FILCOM)
          CALL D0OPEN(ITCOM,FILCOM,' ',OK)
          IF(.NOT.OK) THEN
            YES=.TRUE.
            CALL GETPAR1l(' Cannot open file. Try another? [Y]>',
     &        'L',YES)
            IF(YES) GOTO 2
            GOTO 1
          ENDIF
        ELSE  
          YES=.FALSE.
          CALL OUTMSG(' If the name is NONE, file will not be saved.')
          CALL OUTMSG(' If the name is PARTONS a parton input file'//
     &      ' is expected')
          CALL GETPAR1c(' Enter command file name>','C',FILCOM)
C
C              initialize to read partons from a file
          IF(FILCOM(1:7).EQ.'PARTONS') THEN
   11       CALL GETPAR1c(' Enter PARTONS file name>','C',FILCOM)
            CALL D0OPEN(ITCOM,FILCOM,'IU',OK)
            IF(.NOT.OK) THEN
              YES=.TRUE.
              CALL GETPAR1l(' Cannot open file. Try another? [Y]>',
     &        'L',YES)
              IF(YES) GOTO 11
              CALL QUITS                 ! exit, cannot handle files
            ENDIF
            USE_CONE_LIM=.TRUE.
            CALL GETPAR1l('Use cone limits? [Y]>','L',USE_CONE_LIM)
            CALL OUTMSG(' Pick W/Z decay modes:')
            CALL OUTMSG(' ALL, QUARKS, LEPTONS, ELECTRON, MUON,'
     &        //' E_AND_MU, TAUS, NUS')
            CALL GETPAR1c(' [E_AND_MU]>','C',W_DECAY)
            IF(W_DECAY.EQ.' ') W_DECAY='E_AND_MU'
            IFL=-1
C
          ELSE
            CALL D0OPEN(ITCOM,FILCOM,'O',OK)
            IF(.NOT.OK) THEN
              YES=.TRUE.
              CALL GETPAR1l(' Cannot open file. Try another? [Y]>',
     &        'L',YES)
              IF(YES) GOTO 1
              CALL QUITS                 ! exit, cannot handle files
            ENDIF
C
C       construct a command file
            CALL ENDSPL       
            ISTAT=LIBERA(1,1)
            CALL DIALOG
            ISTAT=LIBERA(1,1)
            ISTAT=LIBREP()
            CALL SPLTIT       
            REWIND ITCOM  
C
          ENDIF 
        ENDIF
C
        CALL INTMSG(' Initializing an ISAJET run, be patient...')
        IF(IFL.EQ.-1) THEN
          CALL ISA_PARTONS_INI(ITCOM,W_DECAY,USE_CONE_LIM)
          CALL FLGSET('PARTONS',.TRUE.)
        ELSE
          CALL ISABEG(IFL)
          IF(IFL.NE.0) THEN
            YES=.TRUE.
            CALL GETPAR1l(' Bad command file. Try another?[Y]>',
     &        'L',YES)
            IF(YES) GOTO 1
            CALL QUITS
          ENDIF
        ENDIF
        CALL ISGZNC             ! zero event count
        CALL INTMSG(' Done.')
C
      ELSE    ! generate events with one track only
        CALL INTMSG(' You must provide subroutine IS1TRK, default is:')
        CALL INTMSG(' D0$ISAZEB$SOURCE:IS1TRK.FOR, use it as example.')
        CALL GETPAR1(' How many 1 track events do you want?>','I',
     &    NEVENT)
        ITCOM=0
      ENDIF
C
      CALL STNVRN(NEVENT)
      CALL FLGSET('ISA_DIAL',.TRUE.)
      ISGDIA=.TRUE.
  999 RETURN
C
C
      ENTRY ISQUIT()
C   
C          close command file
      IF(ITCOM.NE.0) THEN
        IF(FILCOM(1:4).EQ.'NONE') THEN                     
          CLOSE(ITCOM,STATUS='DELETE')    
        ELSE
          CLOSE(ITCOM)
        ENDIF 
      ENDIF
      ISQUIT=.TRUE.
C
      RETURN
      END

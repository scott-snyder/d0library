      SUBROUTINE TRD_USER_DIALOG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Reselects options from TRD RCP file 
C-     
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls:  
C-
C-   Created  13-SEP-1990   J-Fr Glicenstein
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*2 KCH
      CHARACTER*1 REPLY
      CHARACTER*4 REPONS(16)
      CHARACTER*4 ANSWER(16)
      CHARACTER*20 ITEM_SPEC
      CHARACTER*53 QUESTION
      CHARACTER*20 TRIGGERS(10)
      CHARACTER*10   LAYERS(3)
      CHARACTER*1 TYPARR(16)
      CHARACTER*1 TYPHIS(6)
      CHARACTER*2 CHASEC
      CHARACTER*7 ANODS(2)
      CHARACTER*6 UNITS(4)
      CHARACTER*19 HISTOS(5)
      CHARACTER*9 SECTORS(16)
      CHARACTER*31 HISLIM(6)
      INTEGER LIMITS(2,10)
      INTEGER NUM_PHYS,LITEM,KBC,NUM_SEC,CHANGE_COUNT
      INTEGER LUNOUT,IER,JJJ
      REAL BORNES(6),FRANCHIES(6)
      INTEGER IBORNES(1)
      EQUIVALENCE (BORNES,IBORNES)
      INTEGER IANSWER(1),IREPONS(1)
      EQUIVALENCE (ANSWER,IANSWER),(REPONS,IREPONS)
      LOGICAL OK
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF (FIRST) THEN
       DO 5 JJJ = 1,10
        LIMITS(1,JJJ) = 0
        LIMITS(2,JJJ) = 0
        TYPARR(JJJ)   = 'C'
 5     CONTINUE
       DO 7 JJJ = 1,6
 7      TYPHIS(JJJ) = 'R'
       TRIGGERS(1) = 'No special selection'
       TRIGGERS(2) = 'Tracks              '
       TRIGGERS(3) = 'Electrons           '
       TRIGGERS(4) = 'Pions               '
       TRIGGERS(5) = 'Spare               '
       TRIGGERS(6) = 'Spare               '
       TRIGGERS(7) = 'Spare               '
       TRIGGERS(8) = 'Spare               '
       TRIGGERS(9) = 'Spare               '
       TRIGGERS(10)= 'Spare               '
       LAYERS(1)   = 'Chamber 1 '
       LAYERS(2)   = 'Chamber 2 '
       LAYERS(3)   = 'Chamber 3 '
       ANODS(1)    = 'anod   '
       ANODS(2)    = 'cathode'
       UNITS(1)    = 'layer '
       UNITS(2)    = 'sector'
       UNITS(3)    = 'wire  '
       UNITS(4)    = 'TRD   '
       HISTOS(1)   = 'channel map        '
       HISTOS(2)   = 'mean energy/channel'
       HISTOS(3)   = 'number of channels '
       HISTOS(4)   = 'dQ/dt              '
       HISTOS(5)   = 'total energy       '
       HISLIM(1)   = 'Number of channels dQ/dt       '
       HISLIM(2)   = 'bin min dQ/dt                  '
       HISLIM(3)   = 'bin max dQ/dt                  '
       HISLIM(4)   = 'Number of channels total energy'
       HISLIM(5)   = 'bin min total energy           '
       HISLIM(6)   = 'bin max total energy           '
        DO 6 JJJ = 1,16
         WRITE(CHASEC,1000) JJJ
         SECTORS(JJJ) = 'sector '//CHASEC
 6      CONTINUE
       FIRST =.FALSE.
      ENDIF
      NUM_PHYS = 0
      NUM_SEC  = 0
      CHANGE_COUNT = 0
      CALL INTMSG(' TRD user dialog.
     & Please use uppercase letters to answer.')
      CALL EZPICK('TRDHIT_RCP')
      CALL ENDSPL
C
      CALL GETPAR(1,' Change the trigger selection (Y/N) [N]? >',
     &  'C',REPLY)
       IF (REPLY.EQ.'Y') THEN
         CHANGE_COUNT = CHANGE_COUNT+1
         CALL EZGET('PHYSICS_SWITCHES',IANSWER(1),IER)
      CALL UCOPY(IANSWER(1),IREPONS(1),10)
      CALL GETDIS(10,TRIGGERS,TYPARR,LIMITS,REPONS(1)(1:1),
     &REPONS(2)(1:1),REPONS(3)(1:1),REPONS(4)(1:1),
     &REPONS(5)(1:1),REPONS(6)(1:1),REPONS(7)(1:1),REPONS(8)(1:1),
     &REPONS(9)(1:1),REPONS(10)(1:1))
      CALL UCOPY(IREPONS(1),IANSWER(1),10)
        DO 10 KBC = 1,10
        CALL EZSETS('PHYSICS_SWITCHES',KBC,ANSWER(KBC),1,IER)
         IF (ANSWER(KBC)(1:1).EQ.'Y') THEN
        NUM_PHYS = NUM_PHYS + 1
         ENDIF
 10     CONTINUE
       CALL EZSET('NUM_PHYS_SWIT',NUM_PHYS,IER)
       ENDIF
C
      CALL GETPAR(1,' Reselect the chambers (Y/N) [N]? >',
     &  'C',REPLY)
       IF (REPLY.EQ.'Y') THEN
         CHANGE_COUNT = CHANGE_COUNT+1
         CALL EZGET('LAYER_SWITCHES',IANSWER(1),IER)
         CALL UCOPY(IANSWER(1),IREPONS(1),3)
      CALL GETDIS(3,LAYERS,TYPARR,LIMITS,REPONS(1)(1:1),
     &REPONS(2)(1:1),REPONS(3)(1:1))
         CALL UCOPY(IREPONS(1),IANSWER(1),3)
        DO 20 KBC = 1,3
        CALL EZSETS('LAYER_SWITCHES',KBC,ANSWER(KBC),1,IER)
 20     CONTINUE
       ENDIF
C
      CALL GETPAR(1,' Reselect anode and cathod (Y/N) [N]? >',
     &  'C',REPLY)
       IF (REPLY.EQ.'Y') THEN
         CHANGE_COUNT = CHANGE_COUNT+1
         CALL EZGET('ANOD_SWITCHES',IANSWER(1),IER)
         CALL UCOPY(IANSWER(1),IREPONS(1),2)
      CALL GETDIS(2,ANODS,TYPARR,LIMITS,REPONS(1)(1:1),
     &REPONS(2)(1:1))
         CALL UCOPY(IREPONS(1),IANSWER(1),2)
        DO 30 KBC = 1,2
        CALL EZSETS('ANOD_SWITCHES',KBC,ANSWER(KBC),1,IER)
 30     CONTINUE
       ENDIF
C
      CALL GETPAR(1,' reselect the units (Y/N) [N]? >',
     &  'C',REPLY)
       IF (REPLY.EQ.'Y') THEN
         CHANGE_COUNT = CHANGE_COUNT+1
         CALL EZGET('UNIT_SWITCHES',IANSWER(1),IER)
         CALL UCOPY(IANSWER(1),IREPONS(1),4)
      CALL GETDIS(4,UNITS,TYPARR,LIMITS,REPONS(1)(1:1),
     &REPONS(2)(1:1),REPONS(3)(1:1),REPONS(4)(1:1))
         CALL UCOPY(IREPONS(1),IANSWER(1),4)
        DO 40 KBC = 1,4
        CALL EZSETS('UNIT_SWITCHES',KBC,ANSWER(KBC),1,IER)
 40     CONTINUE
       ENDIF
C
      CALL GETPAR(1,' Reselect the histograms (Y/N) [N]? >',
     &  'C',REPLY) 
       IF (REPLY.EQ.'Y') THEN
         CHANGE_COUNT = CHANGE_COUNT+1
         CALL EZGET('HISTOON_SWITCHES',IANSWER(1),IER)
         CALL UCOPY(IANSWER(1),IREPONS(1),5)
      CALL GETDIS(5,HISTOS,TYPARR,LIMITS,REPONS(1)(1:1),
     &REPONS(2)(1:1),REPONS(3)(1:1),REPONS(4)(1:1),REPONS(5)(1:1))
         CALL UCOPY(IREPONS(1),IANSWER(1),5)
        DO 50 KBC = 1,5
        CALL EZSETS('HISTOON_SWITCHES',KBC,ANSWER(KBC),1,IER)
 50     CONTINUE
       ENDIF
C
      CALL GETPAR(1,' Change the histogram limits (Y/N) [N]? >',
     &  'C',REPLY) 
       IF (REPLY.EQ.'Y') THEN
         CHANGE_COUNT = CHANGE_COUNT+1
         CALL EZGET('HISTO_LIMITS',IBORNES,IER)
         DO 61 JJJ = 1,6,3
          FRANCHIES(JJJ)   = IBORNES(JJJ)
          FRANCHIES(JJJ+1) = BORNES(JJJ+1)
 61       FRANCHIES(JJJ+2) = BORNES(JJJ+2)
      CALL GETDIS(6,HISLIM,TYPHIS,LIMITS,FRANCHIES(1),
     &FRANCHIES(2),FRANCHIES(3),FRANCHIES(4),FRANCHIES(5),
     &FRANCHIES(6))
         DO 62 JJJ = 1,6,3
          IBORNES(JJJ)    = FRANCHIES(JJJ)  
          BORNES(JJJ+1)   = FRANCHIES(JJJ+1) 
 62       BORNES(JJJ+2)   = FRANCHIES(JJJ+2) 
        DO 60 KBC = 1,6,3
        CALL EZSETA('HISTO_LIMITS',KBC,KBC,1,IBORNES(KBC),IER)
        CALL EZSETA('HISTO_LIMITS',KBC+1,KBC+1,1,BORNES(KBC+1),IER)
        CALL EZSETA('HISTO_LIMITS',KBC+2,KBC+2,1,BORNES(KBC+2),IER)
 60     CONTINUE
       ENDIF
C
      CALL GETPAR(1,' Change the sectors (Y/N) [N]? >',
     &  'C',REPLY)
       IF (REPLY.EQ.'Y') THEN
         CHANGE_COUNT = CHANGE_COUNT+1
         CALL EZGET('SEC_NUM_SWIT',IANSWER(1),IER)
         CALL UCOPY(IANSWER(1),IREPONS(1),16)
      CALL GETDIS(16,SECTORS,TYPARR,LIMITS,REPONS(1)(1:1),
     &REPONS(2)(1:1),REPONS(3)(1:1),REPONS(4)(1:1),REPONS(5)(1:1),
     &REPONS(6)(1:1),REPONS(7)(1:1),REPONS(8)(1:1),REPONS(9)(1:1),
     &REPONS(10)(1:1),REPONS(11)(1:1),REPONS(12)(1:1),REPONS(13)(1:1),
     &REPONS(14)(1:1),REPONS(15)(1:1),REPONS(16)(1:1))
         CALL UCOPY(IREPONS(1),IANSWER(1),16)
        DO 70 KBC = 1,16
        CALL EZSETS('SEC_NUM_SWIT',KBC,ANSWER(KBC),1,IER)
         IF (ANSWER(KBC)(1:1).EQ.'Y') THEN
        NUM_SEC = NUM_SEC + 1
         ENDIF
 70     CONTINUE
       CALL EZSET('NUM_SEC_SWIT',NUM_SEC,IER)
       ENDIF
C
      CALL GETPAR(1,'Store the changes in a new RCP file (Y/N)
     &[Y]? >','C',REPLY)
       IF (REPLY.NE.'N') THEN
        CALL GTUNIT(666,LUNOUT,IER)
        CALL D0OPEN(LUNOUT,'TRDHIT.RCP','OF',OK)
        CALL EZDUMP(LUNOUT,0,0)
        CLOSE(LUNOUT)
        CALL RLUNIT(666,LUNOUT,IER)
       ENDIF
C
C If changes in the RCP file, takes the correct action
      IF (CHANGE_COUNT.GT.0) CALL TRD_CHGHBK('ON')
      CALL EZRSET
      CALL SPLTIT
C
  999 RETURN
 1000 FORMAT(A2)
C
      END

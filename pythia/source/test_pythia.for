      PROGRAM test_pythia
C--------------------------------------------------------------------
C       PROGRAM TO ILLUSTRATE COMMON PYTHIA USAGE
C-   Updated  23-MAR-2004   compile with g77.
C--------------------------------------------------------------------
      INCLUDE 'D0$INC:ITAPES.INC'
C       PYTHIA COMMONS
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
C
      INTEGER NEVENTS,NPRINT,PRINT_LEVEL,EDIT_LEVEL,STAT_LEVEL
      LOGICAL PYTHRA_INI,OK,EDIT,STAT,WRITEOUT,USER_EDIT
C
C ****  INITIALIZE PROGRAM VARIABLES (ZEBRA,HBOOK, etc.)
C
      OK =  PYTHRA_INI()
      IF(.NOT.OK)CALL ERRMSG('PYTHRA','PYTHRA', 
     &  'CANNOT INITIALIZE PYTHRA ','W')        
C
C ****  INITIALIZE PYTHIA VARIABLES
C
      CALL RCPYTH
C
C ****  GET REMAINING RCP PARAMETERS
C
      CALL EZPICK('PYTHIA_RCP')
      CALL EZGET_i('NUMBER_OF_EVENTS',NEVENTS,IER)      
      CALL EZGET_l('DO_WRITE',WRITEOUT,IER)      
      CALL EZGET_l('USER_EDIT',USER_EDIT,IER)      
      IF(USER_EDIT) THEN
       CALL ERRMSG('PYTHIA','PYTHIA', 
     &  'USER EDIT ENBABLED. USE CROSS SECTIONS ETC. WITH CAUTION','W')
      ENDIF
      CALL EZGET_i('NUMBER_OF_EVENTS_TO_PRINT',NPRINT,IER)      
      CALL EZGET_i('LEVEL_OF_PRINT',PRINT_LEVEL,IER)      
      CALL EZGET_l('DO_EDIT',EDIT,IER)      
      CALL EZGET_i('LEVEL_OF_EDIT',EDIT_LEVEL,IER)      
      CALL EZGET_l('DO_STAT',STAT,IER)      
      CALL EZGET_i('LEVEL_OF_STAT',STAT_LEVEL,IER)      
C
C ****  BEGIN RUN
C
      CALL PYTBFL(NEVENTS)
      IF(WRITEOUT) CALL WREVNT(ITEVT)            ! Write BOR event
C
C **** LOOP OVER EVENTS
C
      OK = .TRUE.
      DO ievent=1,nevents

        CALL pyevnt             !GET EVENT

       IF(EDIT) call luedit(EDIT_LEVEL)
       
       IF (ievent.le.NPRINT) CALL lulist(PRINT_LEVEL)

       CALL PYTEFL
C
       IF(USER_EDIT) THEN
         CALL PYTHIA_USER(OK)
       ENDIF
C
       IF(WRITEOUT .AND. OK) CALL WREVNT(ITEVT)  ! Write output event
      END DO
C
C ****  END  RUN
C
      CALL PYTHIA_END
      IF(WRITEOUT) CALL WREVNT(ITEVT)            ! Write EOR event

C       END PYTHIA STATISTICS

      IF(STAT) CALL pystat(STAT_LEVEL)

      STOP
      END

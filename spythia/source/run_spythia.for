      PROGRAM RUN_SPYTHIA
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Main program routine for SPYTHIA (spythia by
C-                        S. Mrenna)  
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-NOV-1995   Adam L. Lyon
C-
C-----------------------------------------------------------------------
C*****Include variables with output file info
      INCLUDE 'D0$INC:ITAPES.INC'
      INCLUDE 'D0$INC:QUEST.INC'

C*****Pythia commons
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)

      INTEGER NEVENTS,NPRINT,PRINT_LEVEL,EDIT_LEVEL,STAT_LEVEL
      LOGICAL PYTHRA_INI,OK,EDIT,STAT,WRITEOUT,USER_EDIT

C-----------------------------------------------------------------------

C*****Initialize zebra, get info from RCP, etc...
      CALL d0spythia_ini
      

C*****Get initial RCP info, load susy info and call pyinit
      CALL d0spythia_rcp

C*****Get remaining RCP parameters
      CALL EZPICK('SPYTHIA_RCP')
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
      CALL EZGET('LEVEL_OF_STAT',STAT_LEVEL,IER)     

C*****Setup isajet banks
      CALL pytbfl(nevents)
      IF ( writeout) call wrevnt(itevt)  ! Write out begin of run record

C*****Event loop

      ok = .true.

      DO ievent=1,nevents

        CALL pyevnt()

        IF ( edit ) call luedit(edit_level)
        
        IF ( ievent .le. nprint ) call lulist(print_level)

        CALL pytefl    !  Fill zebra banks

        IF ( user_edit ) call pythia_user(ok)

        IF ( writeout .AND. ok ) call wrevnt(itevt)   ! write out event
       
      ENDDO

C*****Finished run
      CALL pytffl

      IF ( writeout ) call wrevnt(itevt)

      IF (stat) call pystat(stat_level)

      CALL FZENDO(itevt,'T')
      CALL D0CLOSE(itevt, ' ', ok)

      STOP
      
 999  CONTINUE
      END




      SUBROUTINE INIJOB(INUNIT,NOIN)
C---------------------------------------------------------------------
C-                                                                   -
C-    Call MENU to chose I/O files and histograms                    -
C-                                                                   -
C-                                                                   -
C-    OUTPUT:                                                        -
C-    INUNIT= Zebra input unit                                       -
C-    NOIN  = .TRUE. if no input file defined                        -
C-                                                                   -
C-                       SDP Apr.,1987                               -
C-                                                                   -
C---------------------------------------------------------------------
C
      IMPLICIT NONE
      INTEGER INUNIT
      LOGICAL NOIN,FLGVAL
      CHARACTER*8 COMMAND
C
      CALL FRSTRN     !  set parameters for first of series of runs
      CALL STNVRN(0)  !  start with 0 events to process
C
    1 CALL MENUDO('D0USER SETUP','SETUP',COMMAND) 
C
      IF(COMMAND(1:5).EQ.'INPUT') THEN
        CALL ZBINPF(INUNIT,NOIN)       ! pick Zebra input file
C
      ELSE IF(COMMAND(1:6).EQ.'OUTPUT') THEN                         
        CALL ZBOUTF                    ! pick output files
C
      ELSE IF(COMMAND(1:4).EQ.'ZERO') THEN
        CALL HRESET(0,'  ')
        CALL USZERO
C
      ELSE IF(COMMAND(1:4).EQ.'AUTO') THEN
        IF(NOIN.AND.(.NOT.FLGVAL('NO_INPUT_FILE'))) THEN
          CALL INTMSG(' Auto process cannot go without an input file')
          CALL ZBINPF(INUNIT,NOIN)       ! pick Zebra input file
        ENDIF
        CALL SETGON(.TRUE.)
        RETURN                 ! return starts processing
C
      ELSE IF(COMMAND(1:6).EQ.'DMPDEF') THEN
        CALL DMPDEF
C
      ELSE IF(COMMAND(1:7).EQ.'DISPLAY') THEN
        CALL SELEDS
C
      ELSE IF(COMMAND(1:6).EQ.'HISPAK') THEN
        CALL HISPAK(.FALSE.)
C
      ELSE IF(COMMAND(1:8).EQ.'STANDARD') THEN  ! select standard output
        CALL SETHPR('STANDARD')
        CALL USETSS
C
      ELSE IF(COMMAND(1:6).EQ.'USDIAL') THEN
        CALL USDIAL     ! user dialog
C
      ELSE IF(COMMAND(1:8).EQ.'SWITCHES') THEN
        CALL STSWCH     ! run-time switches
C                  
      ELSE IF(COMMAND.EQ.'INTERRUP') THEN
        CALL INTRPT                           ! turn INTERRUPT menu on/off
C
      ELSE IF(COMMAND(1:7).EQ.'PROCESS') THEN
C
C             process runs and events
        RETURN                                ! return starts processing
C
      ELSE IF(COMMAND(1:4).EQ.'QUIT') THEN
        CALL QUIT
C
      ELSE IF(COMMAND(1:4).EQ.'EXIT') THEN
        CALL QUITS
C
      ENDIF
      GOTO 1
      END

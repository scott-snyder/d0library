      PROGRAM ISARUN
C   
C          MAIN PROGRAM FOR THE INTERACTIVE VERSION OF ISAJET.  
C   
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ISABNK.INC'
      INCLUDE 'D0$INC:ISAUNT.INC'
C
      INCLUDE 'D0$INC:ITAPES.INC'
      INCLUDE 'D0$INC:IDRUN.INC'
      INTEGER IEVTAP,IDKY,RUNNUM
      LOGICAL OK
      CHARACTER*40 V,VISAZEB,VISAJET
C   
      CHARACTER*80 FILDKY,FILEVT,FILCOM,FILLIS,FILEX    
      CHARACTER*1 YN    
      LOGICAL NEWCOM,QPART,QCAL,QLEP
C   
C   
      V=VISAJET()
      PRINT 1000,V
1000  FORMAT('0',/,10X,A40,/,10X,' (interactive version)',/,
     $10X,'All input should be UPPER CASE.',/,
     $10X,'If in trouble, try HELP.',/)  
C&IF VAXVMS
   1  FILDKY='D0$ISAJET:DECAY.DAT'
      PRINT *,' The program will use D0$ISAJET:DECAY.DAT as default.'
C&ENDIF
C&IF SIUNIX,ALFOSF
C&   1  FILDKY='/nlib/isajet/decay.dat'
C&      PRINT *,' The program will use DECAY.DAT as default.'
C&ENDIF
      PRINT*,' Do you want a different decay table? (Y/N)' 
      READ 1020,YN  
C   
C          SPECIFY DECAY TABLE -- OPENED ONLY FOR EXECUTION 
      IF(YN.EQ.'Y'.OR.YN.EQ.'y') THEN
        PRINT*,' Enter file name for the decay table.'    
        READ 1010,FILDKY  
      ENDIF
1010  FORMAT(A80)   
      PRINT*,' Do you want to print the decay table? (Y/N)' 
      READ 1020,YN  
1020  FORMAT(A1)    
      IF(YN.EQ.'Y'.OR.YN.EQ.'y') THEN    
        IDKY=+21
      ELSE  
        IDKY=-21
      ENDIF 
C            check file can be opened
      FILEX=FILDKY    
      ITDKY=IABS(IDKY)
      CALL D0OPEN(ITDKY,FILDKY,'I',OK)
      IF(.NOT.OK) GOTO 100
C   
C          SPECIFY OUTPUT FILE -- OPENED ONLY FOR EXECUTION 
      PRINT*,' Enter file name for the output data.'    
      READ 1010,FILEVT  
      IEVTAP=-2     ! no provision for handling unstable particles
C
C          Setup for ZEBRA only
C
      ISUNIT=IABS(IEVTAP)
      FILISA=FILEVT
      V=VISAZEB()
      PRINT *,' You are running with ',V
      PRINT *,' Select ZEBRA banks by typing:ISAP or ISAC or ISAL'
      PRINT *,' ISAP: all particles banks will be written'
      PRINT *,' ISAC: pseudo calorimeter banks will be written'
      PRINT *,' ISAL: leptons banks will be written'
      PRINT *,' Any combination is allowed, i.e. ISAPISACISAL will write
     $ all 3 banks'
      READ 1011,BANK
 1011 FORMAT(A12)
C
C  Set up flags to select type of output
C
      QPART=BANK(1:4).EQ.'ISAP'.OR.BANK(5:8).EQ.'ISAP'.OR.
     $        BANK(9:12).EQ.'ISAP'
      QCAL=BANK(1:4).EQ.'ISAC'.OR.BANK(5:8).EQ.'ISAC'.OR.
     $       BANK(9:12).EQ.'ISAC'
      QLEP=BANK(1:4).EQ.'ISAL'.OR.BANK(5:8).EQ.'ISAL'.OR.
     $     BANK(9:12).EQ.'ISAL'
      CALL ISBKST(QPART,QCAL,QLEP)
C
      CALL MZEBRA(0)
      CALL ISAZEB('O')           ! initialize Zebra for ISAJET
C   
C          SPECIFY COMMAND FILE 
   2  PRINT* ,' Do you wish to use an OLD command file? (Y/N)'  
      READ 1020,YN  
      IF(YN.EQ.'Y'.OR.YN.EQ.'y') THEN
        PRINT* ,' Enter file name for the old command file.'    
        READ 1010,FILCOM    
        FILEX=FILCOM    
        CALL D0OPEN(11,FILCOM,'I',OK)
        IF(.NOT.OK) GOTO 100
        NEWCOM=.FALSE.  
      ELSE  
        PRINT*, ' Enter file name for the new command file.'    
        PRINT*, ' If the name is NONE, the file will not be saved.'  
        READ 1010,FILCOM    
        FILEX=FILCOM    
        CALL D0OPEN(11,FILCOM,'O',OK)
        IF(.NOT.OK) GOTO 200
        NEWCOM=.TRUE.   
      ENDIF 
C   
C          SPECIFY LISTING FILE 
      PRINT*,' Enter file name for the listing.'    
      READ 1010, FILLIS 
      FILEX=FILLIS  
      CALL D0OPEN(12,FILLIS,'O',OK)
      IF(.NOT.OK) GOTO 200
C   
C          PREPARE COMMAND FILE 
      IF(NEWCOM) CALL ISASET(IDKY,IEVTAP,11,12) 
C   
C          EXECUTE COMMAND FILE 
      PRINT*,' Do you wish to run this job? (Y/N)'  
      READ 1020,YN  
      IF(YN.EQ.'Y'.OR.YN.EQ.'y') THEN
        ITDKY=IABS(IDKY)
        FILEX=FILEVT    
        ITEVT=IABS(IEVTAP)
        CALL D0OPEN(ITEVT,FILEVT,'OU',OK)
        IF(.NOT.OK) GOTO 100
        PRINT *,' Give a run number [<100000]'
        READ *,RUNNUM
        CALL ISA_SETRUN(RUNNUM)
        CALL ISAJET(IDKY,IEVTAP,11,12)  
        PRINT*,' Events have been generated on file'    
        PRINT*,FILEVT   
      ENDIF 
C   
C          TERMINATE    
      IF(FILCOM(1:4).EQ.'NONE') THEN                     
        FILEX=FILCOM    
        CLOSE(11,ERR=200,STATUS='DELETE')    
      ENDIF 
      PRINT*,' Job terminated normally.'    
      STOP  
C   
100   PRINT 1030,FILEX  
1030  FORMAT('0',/,' Unable to open or close file ',/,1X,A80)
C&IF SIUNIX,ALFOSF
C&      CALL PERROR("OPEN ") 
C&ENDIF
      PRINT *,'  Try another file?'
      READ 1020,YN  
      IF(YN.EQ.'Y'.OR.YN.EQ.'y') THEN
        IF(FILEX.EQ.FILDKY) GOTO 1
        IF(FILEX.EQ.FILCOM) GOTO 2
      ENDIF
      CALL EXIT 
200   PRINT 1031,FILEX
1031  FORMAT('0',/,' Unable to open or close file ',/,1X,A80,/
     &,' JOB TERMINATED')
      CALL EXIT
      END   
      INTEGER FUNCTION USNVRN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Number of events to run
C-  
C-   ENTRY STNVRN(NEVRUN)  
C-   Input: NEVRUN = value of USNVRN in subsequent calls
C-
C-   Created   8-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NEVRUN,NEVSAV,STNVRN
      SAVE NEVSAV
C----------------------------------------------------------------------
C
      DATA NEVSAV/0/
      USNVRN=NEVSAV
      RETURN
C
      ENTRY STNVRN(NEVRUN)
      NEVSAV=NEVRUN
  999 RETURN
      END

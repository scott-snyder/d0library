      PROGRAM ISAIBM                                                            
C                                                                               
C          MAIN PROGRAM FOR THE INTERACTIVE VERSION OF ISAJET.                  
C                                                                               
C          Revisions for IBM version:                                           
C          Main program must come at beginning of file for GENMOD to            
C     create a proper module.                                                   
C          OPEN statement for data file deleted in main program -- it           
C     is duplicated somewhere inside.                                           
C          Output file changed to UNIT ?                                        
C          FORMAT 1010 changed.                                                 
C                                                                               
C  ZEBCOM is the main zebra common block for event data storage                 
C                                                                               
      INTEGER NNQ,NREF                                                          
      PARAMETER (NNQ=200000)                                                    
      PARAMETER (NREF=9)                                                        
      COMMON/ZEBCOM/IXMAIN,FENCE,LHEAD,LHEADR,LREF,ZSTOR,ENDZS                  
      INTEGER IXMAIN                                                            
      INTEGER FENCE(10),LREF(NREF),ZSTOR(NNQ),ENDZS                             
C                       ! pointer to event HEAD bank                            
      INTEGER LHEAD                                                             
C                       ! pointer to begin run HEAD bank                        
      INTEGER LHEADR                                                            
      REAL Q(NNQ)                                                               
      INTEGER IQ(NNQ),LQ(NNQ)                                                   
      EQUIVALENCE (LHEAD,LQ(1)),(LQ(9),IQ(1),Q(1))                              
C                                                                               
      COMMON/ISABNK/BANK,FILISA,FILIS2                                          
      CHARACTER*12 BANK                                                         
      CHARACTER*80 FILISA,FILIS2                                                
C                                                                               
C  If BANK='ISAP' Zebra bank ISAP (particles) will be written out               
C  if BANK='ISAC'     "      ISAC (pseudo calorimeter) will be written out      
C  If BANK='ISAL'     "      ISAL (leptons) will be written out                 
C  if BANK='ISAPISAC' both groups will be written out                           
C  if BANK='ISAPISACISAL' all groups will be written out                        
C                                                                               
C  FILISA= name of ISAJET events file (ZEBRA)                                   
C  FILIS2= name of second file if needed for output                             
C                                                                               
      COMMON/ISAUNT/ISUNIT,ISWRIT                                               
      INTEGER ISUNIT,ISWRIT                                                     
C  ISUNIT=file number to write(read) ISAJET ZEBRA output                        
C  ISWRIT=        "   to write if ISUNIT used for reading                       
C                                                                               
      COMMON/IDRUN/IDVER,IDG(2),IEVT                                            
      INTEGER   IDVER,IDG,IEVT                                                  
      CHARACTER*40 V,VISAZE                                                     
      REAL VERSN                                                                
C                                                                               
      CHARACTER*80 FILDKY,FILEVT,FILCOM,FILLIS,FILEX                            
      CHARACTER*1 YN                                                            
      LOGICAL NEWCOM                                                            
C                                                                               
C                                                                               
      VERSN=IDVER/100.                                                          
      PRINT 1000, VERSN                                                         
1000  FORMAT('0',/,10X,'ISAJET ',F5.2,' (interactive version)',/,               
     $10X,'All input should be UPPER CASE.',/,                                  
     $10X,'If in trouble, try HELP.',/)                                         
C          SPECIFY DECAY TABLE -- OPENED ONLY FOR EXECUTION                     
1010  FORMAT(A)                                                                 
      PRINT*,' Do you want to print the decay table? (Y/N)'                     
      READ 1020,YN                                                              
1020  FORMAT(A1)                                                                
      IF(YN.EQ.'Y'.OR.YN.EQ.'y') THEN                                           
        IDKY=+21                                                                
      ELSE                                                                      
        IDKY=-21                                                                
      ENDIF                                                                     
C            check file can be opened                                           
        ITDKY=IABS(IDKY)                                                        
        OPEN(UNIT=ITDKY,ERR=100,STATUS='OLD',                                   
     1       FORM='FORMATTED'         )                                         
C                                                                               
C          SPECIFY OUTPUT FILE -- OPENED ONLY FOR EXECUTION                     
      IEVTAP=-2                                                                 
C                                                                               
C          Setup for ZEBRA only                                                 
C                                                                               
      ISUNIT=IABS(IEVTAP)                                                       
      V=VISAZE()                                                                
      PRINT *,' You are running with ',V                                        
      PRINT *,' Select ZEBRA banks by typing:ISAP or ISAC or ISAL'              
      PRINT *,' ISAP: all particles banks will be written'                      
      PRINT *,' ISAC: pseudo calorimeter banks will be written'                 
      PRINT *,' ISAL: leptons banks will be written'                            
      PRINT *,' Any combination is allowed, i.e. ISAPISACISAL will write        
     $ ALL 3 BANKS'                                                             
      READ 1010,BANK                                                            
      CALL ISAZEB('O')                                                          
C                                                                               
C          SPECIFY COMMAND FILE                                                 
   2  PRINT* ,' Do you wish to use an OLD command file? (Y/N)'                  
      READ 1020,YN                                                              
      IF(YN.EQ.'Y'.OR.YN.EQ.'y') THEN                                           
        OPEN(UNIT=11,ERR=100,STATUS='OLD',FORM='FORMATTED')                     
        NEWCOM=.FALSE.                                                          
      ELSE                                                                      
        OPEN(UNIT=11,ERR=200,STATUS='NEW',FORM='FORMATTED')                     
        NEWCOM=.TRUE.                                                           
      ENDIF                                                                     
C                                                                               
C          SPECIFY LISTING FILE                                                 
      OPEN(UNIT=12,ERR=200,STATUS='NEW',FORM='FORMATTED')                       
C                                                                               
C          PREPARE COMMAND FILE                                                 
      IF(NEWCOM) CALL ISASET(IDKY,IEVTAP,11,12)                                 
C                                                                               
C          EXECUTE COMMAND FILE                                                 
      PRINT*,' Do you wish to run this job? (Y/N)'                              
      READ 1020,YN                                                              
      IF(YN.EQ.'Y'.OR.YN.EQ.'y') THEN                                           
        ITDKY=IABS(IDKY)                                                        
        ITEVT=IABS(IEVTAP)                                                      
        CALL ISAJET(IDKY,IEVTAP,11,12)                                          
      ENDIF                                                                     
C                                                                               
C          TERMINATE                                                            
      PRINT*,' Job terminated normally.'                                        
      STOP                                                                      
C                                                                               
100   PRINT 1030                                                                
1030  FORMAT('0',/,' Unable to open or close file ',/)                          
      CALL EXIT                                                                 
200   PRINT 1031                                                                
1031  FORMAT('0',/,' Unable to open or close file ',/)                          
      CALL EXIT                                                                 
      END                                                                       

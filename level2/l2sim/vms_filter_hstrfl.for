C-                                                                              
C-                                                                              
C-    Created by the PROGRAM BUILDER                                            
C-    29-JUL-93 15:30:09                                                        
C-                                                                              
C-                                                                              
C-                                                                              
      SUBROUTINE HSTRFL                                                         
C----------------------------------------------------------------------         
C-                                                                              
C-   Purpose and Methods :                                                      
C-      This is a DUMMY in ZEBRA_UTIL                                           
C-      For production programs HSTRFL is build by PROGRAM_BUILDER              
C-                                                                              
C-   Created   3-NOV-1988   Serban D. Protopopescu                              
C-                                                                              
C----------------------------------------------------------------------         
      IMPLICIT NONE                                                             
      INCLUDE 'D0$INC:ZEBCOM.INC'                                               
      INTEGER LHSTR                                                             
      INTEGER CREATION_TIME,OFTIM,PRODID,VERSION,PASS                           
      CHARACTER*40 PROGRAM_NAME                                                 
      CHARACTER*8 CREATION_SITE                                                 
      CHARACTER*8 PROCESSING_SITE                                               
      CHARACTER*8 GTSITE                                                        
      PARAMETER( CREATION_TIME = 112807809 )                                    
      PARAMETER( PRODID = 2 )                                                   
      PARAMETER( VERSION = 5 )                                                  
      PARAMETER( PASS = 0 )                                                     
      PARAMETER( PROGRAM_NAME = 'VMS_FILTER_HSTRFL' )                           
      PARAMETER( CREATION_SITE = 'D0SB15' )                                     
      INTEGER OFFTIM                                                            
      EXTERNAL OFFTIM,GTSITE                                                    
C----------------------------------------------------------------------         
C                                                                               
      CALL BKHSTR(LHSTR)                                                        
      IQ(LHSTR+1)=1                                                             
      IQ(LHSTR+2)=PRODID                                                        
      IQ(LHSTR+3)=VERSION                                                       
      IQ(LHSTR+4)=PASS                                                          
      IQ(LHSTR+5)=CREATION_TIME                                                 
      IQ(LHSTR+6)=OFFTIM()                                                      
      CALL UCTOH(PROGRAM_NAME,Q(LHSTR+7),40,40)                                 
      CALL UCTOH(CREATION_SITE,Q(LHSTR+17),8,8)                                 
      PROCESSING_SITE = GTSITE()                                                
      CALL UCTOH(PROCESSING_SITE,Q(LHSTR+19),8,8)                               
  999 RETURN                                                                    
      END                                                                       

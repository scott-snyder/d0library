************************************************************************
*
*       CLUSCOM.INC
*
************************************************************************
        INCLUDE '($FABDEF)'
        INCLUDE '($RABDEF)'
        PARAMETER CC__MAXCOM=20   
        PARAMETER CC__MAXLEN=20   
        PARAMETER CC__MAXFIL=60   
*
* following parameter governs i/o to avoid excess ws allocation.
* (all units in blocks=pages=512 byte chunks).
        PARAMETER CC__MAXIOP=512  
*
**         lock status block with value block and name
        STRUCTURE /CCLSTB/
          INTEGER*2     STATUS, NULL
          INTEGER*4     ID, LVB(4)
          CHARACTER*(CC__MAXLEN+6)  NAME  
        END STRUCTURE
        STRUCTURE /CLUSCOM/
          CHARACTER*(CC__MAXLEN+3) COMID    
          CHARACTER*1  IO       
          CHARACTER*(CC__MAXFIL) FNAME    
          INTEGER IFIRST        
          INTEGER ILAST         
          INTEGER ILPAGE        
          INTEGER REV           
          INTEGER AUTH          
          INTEGER LOCK          
          INTEGER CHAN          
          INTEGER IADR(2)       
          INTEGER LUN           
          RECORD /FABDEF/ FAB   
          RECORD /RABDEF/ RAB   
          RECORD /CCLSTB/ LKST  
        END STRUCTURE
        RECORD /CLUSCOM/ CCOM(CC__MAXCOM)
        COMMON /CCOMSAV/ CCOM,NCCOM,NCURRENT
*
        REAL DEFTIMOUT_CC /60./  

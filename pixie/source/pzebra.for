      SUBROUTINE PZEBRA(NZEB)   
C.  
C.    ******************************************************************    
C.    *                                                                *    
C.    *       Routine to initialise ZEBRA store (//)                   *    
C.    *                                                                *    
C.    *    ==>Called by : <USER>                                       *    
C.    *       Author    R.Brun  *********                              *    
C.    *                                                                *    
C.    ******************************************************************    
C.  
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)  
     +             ,LMAIN,LR1,WS(9000)  
      DIMENSION IQ(1),Q(1),LQ(8000) 
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN)  
C   
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART 
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX    
     +      ,JVOLUM,JXYZ    
C   
C.  
C.    ------------------------------------------------------------------    
C.  
      NZEBRA=NZEB   
      CALL MZSTOR(IXSTOR,'/GCBANK/',' ',FENDQ,LQ,LR1,WS,LQ(5300)    
     +            ,LQ(NZEBRA-30))   
      CALL MZLOGL(IXSTOR,0) 
C   
      RETURN    
      END   

      SUBROUTINE GZEBRA(NZEB)   
C.  
C.    ******************************************************************    
C.    *                                                                *    
C.    *       Routine to initialise ZEBRA store (//)                   *    
C.    *                                                                *    
C.    *    ==>Called by : <USER>                                       *    
C.    *       Author    R.Brun  *********                              *    
C.    *                                                                *    
C.    *    UPDATED to be included in other packages which use          *
C.    *            geant as a salve.    (SHAHRIAR ABACHI)              *    
C.    *                                                                *    
C.    ******************************************************************    
C   
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCBANK.INC'
      INCLUDE 'D0$INC:GCLINK.INC'
C.  
      INTEGER NZEB
C.    ------------------------------------------------------------------    
C.  
      NZEBRA=NZEB   
CC      CALL INZBRA   
      CALL MZSTOR(IXSTOR,'/GCBANK/',' ',FENDQ,LQ,LR1,WS,LQ(KWWORK+100)  
     +            ,LQ(NZEBRA-30))   
      CALL MZLOGL(IXSTOR,0) 
C   
      RETURN    
      END   

      SUBROUTINE ROTINO(V,SIGTHE)
C.  
C.    ******************************************************************    
C.    *                                                                *    
C.    *       Rotate vector(v) by (the,phi) using the convention       *
C.    *       of GBREME                                                *  
C.    *                                                                *    
C.    *    ==>Called by : <USER>, GOKING                               *    
C.    *       Author    S.Linn  *********                              *    
C.    *                                                                *    
C.    ******************************************************************    

      REAL THE,PHI,V(4),CT,CP,ST,SP,RAND1,RNDM
      REAL DIRCOS(3), SIGTHE
c     ----------------------------------------------------------------

C            Rotation angles in old GTNINO system


      RAND1=RNDM(0)
      THE = -SIGTHE*ALOG(RAND1)
      PHI = RNDM(0)*6.28 

      SP=SIN(PHI)
      CP=COS(PHI)
      ST=SIN(THE)
      CT=COS(THE)

      DIRCOS(1)=V(1)/V(4)
      DIRCOS(2)=V(2)/V(4)
      DIRCOS(3)=V(3)/V(4)

C           MOMENTUM IN GTNINO SYSTEM

      V(1) = V(4)*ST*CP
      V(2) = V(4)*ST*SP
      V(3) = V(4)*CT

C           ROTATE INTO VOLUME SYSTEM

      CT = DIRCOS(3)
      ST = SQRT(ABS(1.-CT*CT) )
      IF( ST .NE. 0.)THEN
         CP = DIRCOS(1)/ST
         SP = DIRCOS(2)/ST
      ELSE
         CP = 1.
         SP = 0.
      ENDIF
      CALL GDROT(V,CT,ST,CP,SP) 
      END

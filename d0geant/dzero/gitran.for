*CMZ :  3.12/18 06/09/88  14.34.18  by  Rene Brun   
*-- Author :    
      SUBROUTINE GITRAN(X,DX,IROT,XNEW) 
C.  
C.    ******************************************************************    
C     *                                                                *    
C     *        ROUTINE TO APPLY THE TRANSFORMATION GIVEN BY            *    
C     *        THE POINT DX AND THE ROTATION MATRIX IN THE GEANT       *    
C     *        ROTATION MATRIX BANKS WITH INDEX NUMBER IROT TO THE     *    
C     *        POINT X, STORING THE RESULT AS THE POINT XNEW.          *    
C     *                                                                *    
C.    *    ==>Called by : GFTRAC,GINVOL,GMEPOS,GNEXT,GTMEDI,GTNEXT     *    
C.    *         Author  A.McPherson  *********                         *    
C.    *                                                                *    
C.    ******************************************************************    
C.  
      INCLUDE 'D0$INC:GCBANK.INC'
      INCLUDE 'D0$INC:GCLINK.INC'
C   
      DIMENSION X(3),DX(3),XNEW(3),XL(3)    
C.  
C.    ------------------------------------------------------------------    
C.  
      IF (IROT.EQ.0) THEN
        DO 10 I=1,3   
          XNEW(I) = X(I) - DX(I)    
   10   CONTINUE  
      ELSE
        DO 20 I=1,3   
          XL(I) = X(I) - DX(I)
   20   CONTINUE
        JR=LQ(JROTM-IROT) 
        DO 30 I=1,3   
          PTR = JR + 3*I
          XNEW(I) = XL(1)*Q(PTR-2)
     &          + XL(2)*Q(PTR-1)
     &          + XL(3)*Q(PTR)
   30   CONTINUE
      ENDIF
C   
  99  RETURN    
      END   

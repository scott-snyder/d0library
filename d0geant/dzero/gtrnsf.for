*CMZ :  3.12/18 06/09/88  14.34.36  by  Rene Brun   
*-- Author :    
      SUBROUTINE GTRNSF(X,DX,RMAT,XNEW) 
C.  
C.    ******************************************************************    
C.    *                                                                *    
C     *       ROUTINE TO APPLY THE TRANSFORMATION GIVEN BY             *    
C     *       THE POINT DX AND THE ROTATION MATRIX RMAT TO THE         *    
C     *       POINT X, PLACING THE RESULT IN THE POINT XNEW.           *    
C.    *                                                                *    
C.    *    ==>Called by : GDFR3D, GFTRAC, GINVOL,GMEDIA, GNEXT, GSTRAC,*    
C.    *                   GTMEDI, GTNEXT                               *    
C.    *         Author  A.McPherson  *********                         *    
C     *                                                                *    
C.    ******************************************************************    
C.  
      DIMENSION X(3),DX(3),RMAT(10),XNEW(3),XL(3)   
C   
      IF (RMAT(10).EQ.0.) THEN
        DO 10 I=1,3   
          XNEW(I)=X(I)-DX(I)    
   10   CONTINUE
      ELSE
        DO 20 I=1,3   
          XL(I)=X(I)-DX(I)  
   20   CONTINUE
        DO 30 I=1,3   
          PTR = 3*I
          XNEW(I)=XL(1)*RMAT(PTR-2)+XL(2)*RMAT(PTR-1)+XL(3)*RMAT(PTR)   
   30   CONTINUE
      ENDIF
C   
  99  RETURN    
      END   

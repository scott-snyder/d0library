        SUBROUTINE PXDV06(GDIPAR)  
c   
c       ***************************************************************** 
c       *                                                               *    
c       *       Geant Draw Volume shape type 6                          * 
c       *                                                               *    
c       *  GOAL: Draw a TUBS ( 5 parameters)                            * 
c       *                                                               *    
c       *  DATE : April 1986                                            *   
c       *                                                               *    
c       *                                                               *    
c       ***************************************************************** 
c   
        REAL GDIPAR(*)
        DIMENSION XTUBS(4),YTUBS(4),ZTUBS(4)   
      INTEGER IOPDOK(5)
      DATA IOPDOK/1,4*0/
C   
c===========================================================================    
c   
c       Section 1 : draw the volume itself    
c   
c         
        CALL JARC(0.,0.,-GDIPAR(3),GDIPAR(1),0,GDIPAR(4),GDIPAR(5))    
        CALL JARC(0.,0.,-GDIPAR(3),GDIPAR(2),0,GDIPAR(4),GDIPAR(5))    
        CALL JARC(0.,0.,+GDIPAR(3),GDIPAR(1),0,GDIPAR(4),GDIPAR(5))    
        CALL JARC(0.,0.,+GDIPAR(3),GDIPAR(2),0,GDIPAR(4),GDIPAR(5))    
c   
c       draw 2 sectors, edges invisible, with appropriate fill    
c   
        CALL JPEDGE(0) 
        CALL J1IGET(11,IVD)    
        IF(IVD.EQ.0) THEN  
          DO 655 IDEV=1,5  
655       IF(IOPDOK(IDEV).EQ.1) CALL JPFSIM(IDEV,0) 
          CALL PXCOLFILL('WHI')
        CALL JSECTR(0.,0.,-GDIPAR(3),GDIPAR(2),0,GDIPAR(4),GDIPAR(5))  
        CALL JSECTR(0.,0.,+GDIPAR(3),GDIPAR(2),0,GDIPAR(4),GDIPAR(5))  
      PHI=GDIPAR(4)*DEGRAD  
      N=1   
      RINC=((GDIPAR(5)-GDIPAR(4))*DEGRAD)/FLOAT(N)  
      M=N-1 
      DO 10 I=1,M   
      XTUBS(1)=+GDIPAR(2)*COS(PHI)  
      YTUBS(1)=+GDIPAR(2)*SIN(PHI)  
      ZTUBS(1)=-GDIPAR(3)   
      XTUBS(2)=GDIPAR(2)*COS(PHI)   
      YTUBS(2)=GDIPAR(2)*SIN(PHI)   
      ZTUBS(2)=+GDIPAR(3)   
      PHI=PHI+RINC  
      XTUBS(3)=+GDIPAR(2)*COS(PHI)  
      YTUBS(3)=+GDIPAR(2)*SIN(PHI)  
      ZTUBS(3)=+GDIPAR(3)   
      XTUBS(4)=GDIPAR(2)*COS(PHI)   
      YTUBS(4)=GDIPAR(2)*SIN(PHI)   
      ZTUBS(4)=-GDIPAR(3)   
      CALL J3PLGN(XTUBS,YTUBS,ZTUBS,4)  
   10 CONTINUE  
      PHI=GDIPAR(4)*DEGRAD  
      DO 20 I=1,M   
      XTUBS(1)=+GDIPAR(1)*COS(PHI)  
      YTUBS(1)=+GDIPAR(1)*SIN(PHI)  
      ZTUBS(1)=-GDIPAR(3)   
      XTUBS(2)=GDIPAR(1)*COS(PHI)   
      YTUBS(2)=GDIPAR(1)*SIN(PHI)   
      ZTUBS(2)=+GDIPAR(3)   
      PHI=PHI+RINC  
      XTUBS(3)=+GDIPAR(1)*COS(PHI)  
      YTUBS(3)=+GDIPAR(1)*SIN(PHI)  
      ZTUBS(3)=+GDIPAR(3)   
      XTUBS(4)=GDIPAR(1)*COS(PHI)   
      YTUBS(4)=GDIPAR(1)*SIN(PHI)   
      ZTUBS(4)=-GDIPAR(3)   
      CALL J3PLGN(XTUBS,YTUBS,ZTUBS,4)  
   20 CONTINUE  
        END IF 
        CALL JPEDGE(1) 
c   
      XTUBS(1)=+GDIPAR(1)*COS(GDIPAR(4)*DEGRAD) 
      YTUBS(1)=+GDIPAR(1)*SIN(GDIPAR(4)*DEGRAD) 
      ZTUBS(1)=-GDIPAR(3)   
      XTUBS(2)=+GDIPAR(2)*COS(GDIPAR(4)*DEGRAD) 
      YTUBS(2)=+GDIPAR(2)*SIN(GDIPAR(4)*DEGRAD) 
      ZTUBS(2)=-GDIPAR(3)   
      XTUBS(3)=+GDIPAR(2)*COS(GDIPAR(4)*DEGRAD) 
      YTUBS(3)=+GDIPAR(2)*SIN(GDIPAR(4)*DEGRAD) 
      ZTUBS(3)=+GDIPAR(3)   
      XTUBS(4)=+GDIPAR(1)*COS(GDIPAR(4)*DEGRAD) 
      YTUBS(4)=+GDIPAR(1)*SIN(GDIPAR(4)*DEGRAD) 
      ZTUBS(4)=GDIPAR(3)    
      CALL J3PLGN(XTUBS,YTUBS,ZTUBS,4)  
      XTUBS(1)=+GDIPAR(1)*COS(GDIPAR(5)*DEGRAD) 
      YTUBS(1)=+GDIPAR(1)*SIN(GDIPAR(5)*DEGRAD) 
      ZTUBS(1)=-GDIPAR(3)   
      XTUBS(2)=+GDIPAR(2)*COS(GDIPAR(5)*DEGRAD) 
      YTUBS(2)=+GDIPAR(2)*SIN(GDIPAR(5)*DEGRAD) 
      ZTUBS(2)=-GDIPAR(3)   
      XTUBS(3)=+GDIPAR(2)*COS(GDIPAR(5)*DEGRAD) 
      YTUBS(3)=+GDIPAR(2)*SIN(GDIPAR(5)*DEGRAD) 
      ZTUBS(3)=+GDIPAR(3)   
      XTUBS(4)=+GDIPAR(1)*COS(GDIPAR(5)*DEGRAD) 
      YTUBS(4)=+GDIPAR(1)*SIN(GDIPAR(5)*DEGRAD) 
      ZTUBS(4)=GDIPAR(3)    
      CALL J3PLGN(XTUBS,YTUBS,ZTUBS,4)  
c   
c       -----------------------------------------------------------   
c   
          DO 656 IDEV=1,5  
656       IF(IOPDOK(IDEV).EQ.1) CALL JPFSIM(IDEV,1) 
99      CONTINUE 
        RETURN 
        END    

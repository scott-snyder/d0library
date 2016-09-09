      SUBROUTINE DBXINI 
*     ================= 
*   
************************************************************************    
*                                                                      *    
*        SUBR. DBXINI                                                  *    
*                                                                      *    
*   Routine to Initialize the DB Interactive Run                       *    
*                                                                      *    
*   Called by DBMAIN                                                   *    
*                                                                      *    
************************************************************************    
*   
      PARAMETER       (MAXJDX=20, MAXVDX= 20, LUKYDX=88, LUDADX=89) 
      COMMON /DXLINK/ ISTODX, L3PRDX, LURZDX, LFRSDX, LJOIDX,   
     +                LKJNDX(MAXJDX), LDJNDX(MAXJDX), LVIWDX,   
     +                LKVWDX(MAXVDX), LDVWDX(MAXVDX),   
     +                LASTDX    
*   
      COMMON /DDISPL/ IOPHDD, IOTYDD(100), MXDPDD, NUMCDD(100)  
      COMMON /DDCFMT/ CFMTDD    
      CHARACTER       CFMTDD*20 
*   
      PARAMETER       (L3CORQ=200000, L3CORH=200000, L3CORK=70000)  
*   
      PARAMETER       (NDIV1=5000)  
      COMMON /GCBANK/ FENCDB(22), LQ(L3CORQ)    
      DIMENSION       IAR(3)    
*   
*     ------------------------------------------------------------------    
*   
      LURZDX = 1    
      L3PRDX = 6    
*   
      CALL MZSTOR (ISTODX, '/GCBANK/', ' ', FENCDB(1), LQ(1), LQ(1),    
     +             LQ(1), LQ(NDIV1), LQ(L3CORQ))    
      NDIV2  = 3*L3CORQ/4   
*   
      CALL MZDIV  (ISTODX, IDIV, 'DB-USERS', NDIV2/2, NDIV2, 'LC')  
      CALL MZLINK (ISTODX, '/DXLINK/', LFRSDX, LFRSDX, LASTDX)  
*   
      IAR(1) = LURZDX   
      IAR(2) = ISTODX   
      IAR(3) = IDIV 
      CALL DBPRNT (L3PRDX, '(/,''  ----- Interactive Session on DBL3 '//    
     +     'Test-Run Starts -----'',//,10X,''RZ-Unit Number : '',I5'//  
     +     ',''  DBL3_Store :'',I10,''  User-Division :'',I10,/)',IAR,3)    
*   
*  ** Set Default Display Range 
*   
      MXDPDD = 80   
      WRITE (CFMTDD, '(''(A'',I3,'')'')') MXDPDD    
      GO TO 999 
*                                                             END DBXINI    
  999 END   

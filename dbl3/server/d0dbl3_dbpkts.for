      SUBROUTINE D0DBL3_DBPKTS (IDATE, ITIME, IDATM)   
*     =======================================   
*   
************************************************************************    
*                                                                      *    
*        SUBR. DBPKTS (IDATE, ITIME, IDATM*)                           *    
*                                                                      *    
*   Packs date and time into one word                                  *    
*                                                                      *    
*   Arguments :                                                        *    
*                                                                      *    
*     IDATE    Date : 6 Decimal integer : YYMMDD                       *    
*              (Only Year > 80 allowed)                                *    
*     ITIME    Time : 6 Decimal integer : HHMMSS                       *    
*     IDATM*   Packed date-time                                        *    
*                                                                      *    
*   Called by user,   DBINIT, DBCDIC, DBSNAM, DBUDIC, DBPLNT, DBPLOB,  *    
*             DBPLOV, DBPLTI, DBACPL, DBAUXI, DBDCKH, DBDCKV, DBVIEW   *    
*                                                                      *    
************************************************************************    
*   
      PARAMETER       (MXSEC=61, MXMIN=61, MXHOU=25, MXDAY=32, MXMON=13)
      INTEGER         IDAYY(20), IDAYM(12,2)    
      SAVE            IDAYY, IDAYM  
      DATA            IDAYY / 366,  731, 1096, 1461, 1827, 2192, 2557,
     1                       2922, 3288, 3653, 4018, 4383, 4749, 5114,
     2                       5479, 5844, 6210, 6575, 6940, 7305/    
      DATA            IDAYM / 0,  31,  59,  90, 120, 151, 181, 212, 243,
     1                      273, 304, 334,   0,  31,  60,  91, 121, 152,
     2                      182, 213, 244, 274, 305, 335/   
*   
*     ------------------------------------------------------------------
*   
      ISEC = MOD(ITIME,100) 
      IHOU = ITIME/100  
      IMIN = MOD(IHOU,100)  
      IHOU = IHOU/100   
*   
      IDAY = MOD(IDATE,100) 
      IYEA = IDATE/100  
      IMON = MOD(IYEA,100)  
      IYEA = IYEA/100 - 80  
*   
      IDATM = ISEC  
      MAXX = MXSEC  
      IDATM = IDATM + MAXX*IMIN 
      MAXX = MAXX*MXMIN 
      IDATM = IDATM + MAXX*IHOU 
      MAXX = MAXX*MXHOU 
      IDATM = IDATM + MAXX*IDAY 
      MAXX = MAXX*MXDAY 
      IDATM = IDATM + MAXX*IMON 
      MAXX = MAXX*MXMON 
      IDATM = IDATM + MAXX*IYEA 
*                                                             END DBPKTS    
      END   

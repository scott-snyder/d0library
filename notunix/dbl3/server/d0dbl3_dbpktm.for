      SUBROUTINE D0DBL3_DBPKTM (IDATE, ITIME, IDATM)   
*     =======================================   
*   
************************************************************************    
*                                                                      *    
*        SUBR. DBPKTM (IDATE, ITIME, IDATM*)                           *    
*                                                                      *    
*   Packs date and time into one word                                  *    
*                                                                      *    
*   Arguments :                                                        *    
*                                                                      *    
*     IDATE    Date : 6 Decimal integer : YYMMDD                       *    
*     ITIME    Time : 4 Decimal integer : HHMM                         *    
*     IDATM*   Packed date-time                                        *    
*                                                                      *    
*   Called by user,   DBDELT, DBEFOR, DBENTB, DBINIT, DBMDIP, DBPRGD,  *    
*             DBCDIC, DBENFZ, DBKOUT, DBSDIR, DBSNAM, DBSPUR, DBUDIC,  *    
*             DBPLOB, DBACPL, DBAUXI, DBDCKH, DBDCKV                   *    
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
      IMIN = MOD(ITIME,100) 
      IHOU = ITIME/100  
*   
      IDAY = MOD(IDATE,100) 
      IYEA = IDATE/100  
      IMON = MOD(IYEA,100)  
      IYEA = IYEA/100   
*   
      IDATM = IMIN  
      MAXX = MXMIN  
      IDATM = IDATM + MAXX*IHOU 
      MAXX = MAXX*MXHOU 
      IDATM = IDATM + MAXX*IDAY 
      MAXX = MAXX*MXDAY 
      IDATM = IDATM + MAXX*IMON 
      MAXX = MAXX*MXMON 
      IDATM = IDATM + MAXX*IYEA 
*                                                             END DBPKTM    
      END   

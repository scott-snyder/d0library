      SUBROUTINE D0DBL3_DBUPTM (IDATE, ITIME, IDATM)   
*     =======================================   
*   
************************************************************************    
*                                                                      *    
*        SUBR. DBUPTM (IDATE*, ITIME*, IDATM)                          *    
*                                                                      *    
*   Unpacks date and time from one word                                *    
*                                                                      *    
*   Arguments :                                                        *    
*                                                                      *    
*     IDATE*   Date : 6 Decimal integer : YYMMDD                       *    
*     ITIME*   Time : 4 Decimal integer : HHMM                         *    
*     IDATM    Packed date-time                                        *    
*                                                                      *    
*   Called by user,   DBFZUP, DBLKEY, DBNTOP, DBPRGD, DBSRTM, DBENFZ,  *    
*             DBPLOB, DBDKYH, DBDKYV                                   *    
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
      III  = IDATM  
      IMIN = MOD(III,MXMIN) 
      III  = (III-IMIN)/MXMIN   
      IHOU = MOD(III,MXHOU) 
      III  = (III-IHOU)/MXHOU   
      IDAY = MOD(III,MXDAY) 
      III  = (III-IDAY)/MXDAY   
      IMON = MOD(III,MXMON) 
      IYEA = (III-IMON)/MXMON   
*   
      IDATE = IDAY + 100*IMON + 10000*IYEA  
      ITIME = IMIN + 100*IHOU   
*                                                             END DBUPTM    
      END   

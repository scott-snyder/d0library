      SUBROUTINE D0DBL3_DBUTIS (IDATM, ISECS)  
*     ================================  
*   
************************************************************************    
*                                                                      *    
*        SUBR. DBUTIS (IDATM, ISECS*)                                  *    
*                                                                      *    
*   Computes number of seconds passed since midnight of January 1,1980 *    
*   from the packed date and time (ala DBPKTS)                         *    
*                                                                      *    
*   Arguments :                                                        *    
*                                                                      *    
*     IDATM    Packed date-time                                        *    
*     IMINS*   Number of seconds passed from 00.00 on Jan 1, 1980      *    
*                                                                      *    
*   Called by DBPLNT                                                   *    
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
      ISEC = MOD (III, MXSEC)   
      III  = (III-ISEC)/MXSEC   
      IMIN = MOD (III, MXMIN)   
      III  = (III-IMIN)/MXMIN   
      IHOU = MOD (III, MXHOU)   
      III  = (III-IHOU)/MXHOU   
      IDAY = MOD (III, MXDAY)   
      III  = (III-IDAY)/MXDAY   
      IMON = MOD (III, MXMON)   
      IYEA = (III-IMON)/MXMON   
      IF (IYEA.LT.1) THEN   
        IADD = 0    
        ITYP = 2    
      ELSE IF (IYEA.GT.20) THEN 
        IADD = IDAYY(20)    
        ITYP = 2    
      ELSE  
        IADD = IDAYY(IYEA)  
        ITYP = MOD (IYEA, 4)    
        IF (ITYP.EQ.0) THEN 
          ITYP = 2  
        ELSE    
          ITYP = 1  
        ENDIF   
      ENDIF 
*   
      IDAYS = IDAY + IADD + IDAYM(IMON,ITYP)    
      ISECS = ISEC + 60*(IMIN + 60*(IHOU + 24*IDAYS))   
*                                                             END DBUTIS    
      END   

      SUBROUTINE D0DBL3_DBINCT (IDTMI, ISADD, IDTMO)   
*     =======================================   
*   
************************************************************************    
*                                                                      *    
*        SUBR. DBINCT (IDTMI, ISADD, IDTMO*)                           *    
*                                                                      *    
*   Converts packed date and time (ala DBPKTS) to a similar packed     *    
*   number after adding a fixed time in seconds                        *    
*                                                                      *    
*   Arguments :                                                        *    
*                                                                      *    
*     IDTMI    Packed date-time (ala DBPKTS) on input                  *    
*     ISADD    Incremental time to IDTMI in seconds                    *    
*     IDTMO    Packed date-time (ala DBPKTS) on output                 *    
*                                                                      *    
*   Called by user                                                     *    
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
* *** Get the real Year/Month/Date Hour/Minute/Second   
*   
      III    = IDTMI    
      ISEC   = MOD (III, MXSEC) 
      III    = (III-ISEC)/MXSEC 
      IMIN   = MOD (III, MXMIN) 
      III    = (III-IMIN)/MXMIN 
      IHOU   = MOD (III, MXHOU) 
      III    = (III-IHOU)/MXHOU 
      IDAY   = MOD (III, MXDAY) 
      III    = (III-IDAY)/MXDAY 
      IMON   = MOD (III, MXMON) 
      IYEA   = (III-IMON)/MXMON 
*   
* *** Add the second to real seconds in the day 
*   
      ISECS  = ISEC  + 60*(IMIN + 60*IHOU)  
      ISECS  = ISECS + ISADD    
      IF (ISECS.GE.0.AND.ISECS.LT.86400) THEN   
        ISEC   = MOD (ISECS, 60)    
        ISECS  = (ISECS-ISEC)/60    
        IMIN   = MOD (ISECS, 60)    
        IHOU   = (ISECS-IMIN)/60    
      ELSE  
        IF (ISECS.LT.0) THEN    
          III    = -ISECS   
          IDADD  = -(III/86400 + 1) 
        ELSE    
          IDADD  = ISECS/86400  
        ENDIF   
        ISECS  = ISECS - IDADD*86400    
        ISEC   = MOD (ISECS, 60)    
        ISECS  = (ISECS-ISEC)/60    
        IMIN   = MOD (ISECS, 60)    
        IHOU   = (ISECS-IMIN)/60    
        IF (IYEA.LT.1) THEN 
          IADD   = 0    
          ITYP   = 2    
        ELSE IF (IYEA.GT.20) THEN   
          IADD   = IDAYY(20)    
          ITYP   = 2    
        ELSE    
          IADD   = IDAYY(IYEA)  
          ITYP   = MOD (IYEA, 4)    
          IF (ITYP.EQ.0) THEN   
            ITYP = 2    
          ELSE  
            ITYP = 1    
          ENDIF 
        ENDIF   
        IDAYS  = IDAY + IADD + IDAYM(IMON,ITYP) + IDADD 
        DO 10 I = 1, 20 
          IF (IDAYS.LE.IDAYY(I)) THEN   
            IYEA   = I - 1  
            IF (IYEA.GT.0) IDAYS  = IDAYS - IDAYY(IYEA) 
            GO TO 15    
          ENDIF 
   10   CONTINUE    
        IYEA   = 20 
        IDAYS  = 366    
   15   IF (MOD(IYEA,4).EQ.0) THEN  
          ITYP   = 2    
        ELSE    
          ITYP   = 1    
        ENDIF   
        DO 20 I = 2, 12 
          IF (IDAYS.LE.IDAYM(I,ITYP)) THEN  
            IMON   = I - 1  
            GO TO 25    
          ENDIF 
   20   CONTINUE    
        IMON   = 12 
   25   IDAY   = IDAYS - IDAYM(IMON,ITYP)   
      ENDIF 
*   
* *** Now reconvert into a packed time  
*   
      IDTMO  = ISEC 
      MAXX   = MXSEC    
      IDTMO  = IDTMO + MAXX*IMIN    
      MAXX   = MAXX*MXMIN   
      IDTMO  = IDTMO + MAXX*IHOU    
      MAXX   = MAXX*MXHOU   
      IDTMO  = IDTMO + MAXX*IDAY    
      MAXX   = MAXX*MXDAY   
      IDTMO  = IDTMO + MAXX*IMON    
      MAXX   = MAXX*MXMON   
      IDTMO  = IDTMO + MAXX*IYEA    
*                                                             END DBINCT    
      END   

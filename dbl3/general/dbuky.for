      SUBROUTINE DBUKY (PATHN, KY, NKY, KEYS, CHOPT)    
*     ======================================================    
*   
************************************************************************    
*                                                                      *    
*        SUBR. DBUKY (PATHN, KY*, NKY*, KEYS, CHOPT)                   *    
*                                                                      *    
*   Prepares the database key structure in memory for any required     *    
*   Pathname and set of Keys, unless already done.                     *    
*   Returns the key values for the corresponding Key banks after       *
*   checking their validity for the given time and keys.               *    
*   ps. Keys are returned as integers (do you own equivalences)        *
*                                                                      *    
*   Arguments :                                                        *    
*                                                                      *    
*     PATHN    Character string describing the pathname                *    
*     KY(*)    Values of Keys bank(s) KYDB   (INPUT or OUTPUT)         *    
*              For option 'S' it is the support address of the linear  *    
*              structure                                               *    
*              For option 'M' with selection on user keys 8 and 9,     *    
*              LBK(k) is the address corresponding to the ith Key-8    *    
*              and the jth Key-9 value, where k = KEYS(8) * (j-1) + i  *    
*     KEYS     Vector of keys. Only the elements declared in CHOPT are *    
*              assumed to contain useful information.                  *    
*              When option 'M' is declared KEYS(n) (when user Key n    *    
*              is selected should contain the number of data objects   *    
*              to be retrieved according to the KEYS(n) values and     *    
*              the values of the key elements for Key-n to be matched  *    
*              should be stored in successive KEYS(i) elements, with   *    
*              i starting from NWKEY+1 (NWKEY is the number of key     *    
*              elements for this directory)                            *    
*     CHOPT    Character string with any of the following characters   *    
*          A   trust LBK address(es) if non-zero                       *    
*          K   read only the keys (no data is required)                *    
*          M   expect multiple Key banks to be returned (only up to    *    
*              a maximum of 5 user keys)                               *    
*          S   expect multiple Key banks satisfying selection on a     *    
*              number of keys (Options S and M are mutually exclusive) *    
*          V   declare the Data as being different in size to what is  *    
*              already resident in memory                              *    
*          3   selects objects with start validity time < KEYS(3)      *    
*              (with option S)                                         *    
*          4   selects objects with end validity time > KEYS(4)        *    
*              (with option S)                                         *    
*          5   specific Program version number required                *    
*          7   select objects with insertion time < KEYS(7)            *    
*          n   consider user key n (where 7 < n < 29 )                 *    
*                                                                      *    
*   Called by user
*                                                                      *    
*   Error Condition :                                                  *    
*                                                                      *    
*     IQUEST(1) =  0 : No error                                        *    
*               =  1 : Illegal character option                        *    
*               =  2 : Illegal path name                               *    
*               =  3 : Data base structure in memory clobbered         *    
*               =  4 : Illegal key option                              *    
*                                                                      *    
*     If IQUEST(1) =0, IQUEST(2) carries information whether data      *    
*     part had been actually read from the disk or not                 *    
*     IQUEST(2) =  0 : No disk i/o has been performed                  *    
*               =  1 : Data have been refreshed from the disk          *    
*                                                                      *    
************************************************************************    
*   
      PARAMETER       (MXKYDA=100)  
      COMMON /DAOPTS/ IOPADA, IOPBDA, IOPCDA, IOPDDA, IOPEDA, IOPFDA    
     +              , IOPGDA, IOPHDA, IOPIDA, IOPJDA, IOPKDA, IOPLDA    
     +              , IOPMDA, IOPNDA, IOPODA, IOPPDA, IOPQDA, IOPRDA    
     +              , IOPSDA, IOPTDA, IOPUDA, IOPVDA, IOPWDA, IOPXDA    
     +              , IOPYDA, IOPZDA, IOKYDA(MXKYDA)    
*   
      COMMON /GCBANK/ FENCDB(22), LQ(9) 
      DIMENSION       IQ(2), Q(2)   
      EQUIVALENCE     (IQ(1),Q(1),LQ(9))    
*   
      COMMON /QUEST/  IQUEST(100)   
*   
      PARAMETER       (IKTYDB=33, KNSDDB=23, KLSDDB=26, NWNODB=16)  
      PARAMETER       (NWITDB=25, NPUSDB=50, MAXLDB=80, LUFMDB=999) 
      PARAMETER       (JRZUDB=1, JIGNDB=2, JPRTDB=3, JASFDB=4)  
      PARAMETER       (KLKYDB=0, KLDADB=1, KLNODB=2, 
     &  KLUPDB=3, KLDICT=2)    
      PARAMETER       (KLFZDB=3, MFZTOP=1, MFZDIR=5, MXLWDB=20) 
      PARAMETER       (MUPLUN=1, MUPFLG=2, MUPJFL=3, MUPBAK=4, 
     &  MUPDIC=5)    
      PARAMETER       (MUPNCH=6, MUPSRV=7, MUPKY7=8, MUPNAM=9)  
      PARAMETER       (MNDNWK=1, MNDNWD=2, MNDNCH=3, MNDDIC=4)  
      PARAMETER       (MNDIOF=5, MNDNAM=NWNODB+5, NLKYDB=3, NSKYDB=1)   
      PARAMETER       (MKYRID=-4, MKYCEV=-3, MKYCRU=-2, MKYPRE=-1)  
      PARAMETER       (MKYFRI=0, NLUPDB=3, NSUPDB=2, NDUPDB=MUPNAM+3)   
      PARAMETER       (MDCNTM=1, MDCITM=1, MDCNCH=2, MDCLUP=3)  
      PARAMETER       (MDCALI=4, MDCNAM=6)  
      PARAMETER       (NARGDB=20)   
      COMMON /DBSTOR/ LCDRDB, LJNKDB, IBIGDB, INSRDB, IODIDB, IOFZDB    
     +              , IOVRDB, KY4MDB, KY7MDB, IOKYDB(NWNODB)    
     +              , IARGDB(NARGDB)    
      COMMON /DBUSER/ IDEBDB, IDISDB, IDIVDB, IHKYDB, IKDRDB, IONODB    
     +              , KOFSDB, KOFUDB, LBADDB, LBDADB, LBFXDB, LBFYDB    
     +              , LBKYDB, LBNODB, LFIXDB, LREFDB(7),      LSAVDB    
     +              , LTOPDB, LPRTDB, NTOPDB    
      PARAMETER       (NDMXDB=25000)    
*   
      PARAMETER       (MXDMDK=90, MXKYDK=10000, NINEDK=9, NSYSDK=7) 
      PARAMETER       (MXKPDK=1000) 
*   
      COMMON /DKKEYS/ ICONDK(MXDMDK), INDKDK(MXKYDK), IOTYDK(MXDMDK)    
     +              , IPURDK(MXKYDK), KEY1DK(MXKYDK), KEYNDK(MXDMDK)    
     +              , KEYVDK(MXDMDK), KEY7DK, NKEYDK, NSKPDK, NWKYDK    
      DIMENSION       RKY1DK(MXKYDK)    
      EQUIVALENCE     (KEY1DK(1), RKY1DK(1))    
*   
      COMMON /DKTAGS/ CHTGDK(NINEDK), CTAGDK(MXDMDK), CHFTDK    
      CHARACTER       CHFTDK*9, CHTGDK*8, CTAGDK*8  
*   
*     (Arbitary dimension 9 to force transmission by address for scalar)    
      DIMENSION       KEYS(9), LBD(9), LBK(9)
      DIMENSION       KY(NKY)
      CHARACTER       CHOPT*(*), PATHN*(*), PATH*80, FPATH*80
*   
*     ------------------------------------------------------------------    
*   
* *** Initialize options    
*
      ITIME = 0   
C ACP_data_retrieval_start  
      LREFDB(1) = LBK(1)    
      CALL DBOPTS (CHOPT)   
      IF (IQUEST(1).NE.0)       GO TO 999   
      IF (IOPMDA.NE.0 .AND. IOPSDA.NE.0) THEN   
        IQUEST(1) = 1   
        IF (IDEBDB.GT.0) CALL DBPRNT (LPRTDB,
     &     '(/,'' DBUSE : Illegal '//    
     +  'Character option - S/M options are mutually exclusive'')', 
     +  IARGDB, 0)  
        GO TO 999   
      ENDIF 
*   
* *** Suppress blanks from the path name    
*   
      CALL DBSBLC (PATHN, PATH, NCHAR)  
*   
* *** Create (or complete) database skeleton in memory  
*                       (banks NODB and KYDB)   
*   
      CALL DBNODE (PATH, LBNODB)  
      IF (IQUEST(1).NE.0)     GO TO 999   
*
      CALL DBKEYS (LBNODB, KEYS, LBK, ITIME)  
      IF (IQUEST(1).NE.0)     GO TO 999   
      LREFDB(1) = LBK(1)
      DO I = 1,NKY
        IF (IOTYDK(I) .EQ. 5 .OR. IOTYDK(I) .EQ. 6) THEN 
           CALL ZITOH (IQ(KOFUDB+LREFDB(1)+I),KY(I),1)
        ELSE 
           KY (I) = IQ(KOFUDB+LREFDB(1)+I)
        END IF
      END DO
      IQUEST(2) = 0   
*   
  999 CONTINUE  
C ACP_data_retrieval_end    
      END   

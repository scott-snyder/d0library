*
      SUBROUTINE DBUME (PATHN, KEYS, LNK, LBK, CHOPT) 
*     ================================================
*   
************************************************************************    
*                                                                      *    
*   Creates or completes the Key banks supported as next of same type  *    
*   to the Node bank.                                                  *    
*   It will check if node/key bank exist in memory, if then lbk(1)#0   *
*   else lbk(1)=0.                                                     *    
*   It only works if at least CHOPT = 'S34'.                           *    
*                                                                      *    
*   Arguments :                                                        *    
*                                                                      *    
*     PATHN    Path name                                               *
*     KEYS     Vector of keys                                          *    
*     LNK      Number of keys                                          *    
*     LBK(*)   Address(es) of Keys bank(s) KYDB                        *    
*     CHOPT    Option a'la DBUSE                                       *
*                                                                      *    
*   Called by DBABRD, DBABWR, DBENTR, DBPURK, DBREPL, DBUSE            *    
*                                                                      *    
*   Error Condition :                                                  *    
*                                                                      *    
*     IQUEST(1) =  0 : No error                                        *    
*               = 22 : Illegal key option                              *    
*               = 25 : Illegal Path Name                               *    
*                                                                      *    
* Adopted from DBUSE and DBKEYS, Lor 16-oct-92                         *    
* There is no reason that this code will work under all circumstances  *    
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
      PARAMETER       (NWITDB=25, NPUSDB=50, MAXLDB=80) 
      PARAMETER       (JRZUDB=1, JIGNDB=2, JPRTDB=3, JASFDB=4)  
      PARAMETER       (KLKYDB=0, KLDADB=1, KLNODB=2, KLUPDB=3, KLDICT=2)
      PARAMETER       (KLFZDB=3, MFZTOP=1, MFZDIR=5, MXLWDB=20) 
      PARAMETER       (MUPLUN=1, MUPFLG=2, MUPJFL=3, MUPBAK=4, MUPDIC=5)
      PARAMETER       (MUPNCH=6, MUPSRV=7, MUPKY7=8, MUPNAM=9)  
      PARAMETER       (MNDNWK=1, MNDNWD=2, MNDNCH=3, MNDDIC=4)  
      PARAMETER       (MNDIOF=5, MNDNAM=NWNODB+5, NLKYDB=3, NSKYDB=1)
      PARAMETER       (MKYRID=-4, MKYCEV=-3, MKYCRU=-2, MKYPRE=-1)  
      PARAMETER       (MKYFRI=0, NLUPDB=3, NSUPDB=2, NDUPDB=MUPNAM+3)   
      PARAMETER       (MDCNTM=1, MDCITM=1, MDCNCH=2, MDCLUP=3)  
      PARAMETER       (MDCALI=4, MDCNAM=6)  
      PARAMETER       (NARGDB=20)   
      PARAMETER       (LUFMDB=999)  
      PARAMETER       (MSERDB=1, MUPNDB=2, MBVRDB=3, MEVRDB=4, MPVSDB=5,
     +                 MFLGDB=6, MITMDB=7)  
      PARAMETER       (MPSRDB=1, MXKPDB=2, MOBJDB=5)    
      PARAMETER       (MHFMDB=3, MLEVDB=3, MNAMDB=7, MNCHDB=5, MNDWDB=4)
      PARAMETER       (MNFNDB=3, MNLVDB=2, MNNUDB=2, MNODDB=4, MPNNDB=2)
      PARAMETER       (MPNLDB=1, MPPLDB=8, MXOFDB=5, MXWDDB=6, MYFLDB=7)
      PARAMETER       (MYFNDB=6)    
      COMMON /DBSTOR/ LCDRDB, LJNKDB, IBIGDB, INSRDB, IODIDB, IOFDDB    
     +              , IOFZDB, IOVRDB, KY4MDB, KY7MDB, IOKYDB(NWNODB)    
     +              , IARGDB(NARGDB)    
      COMMON /DBUSER/ IDEBDB, IDISDB, IDIVDB, IHKYDB, IKDRDB, IONODB    
     +              , KOFSDB, KOFUDB, LBADDB, LBAFDB, LBDADB, LBFXDB    
     +              , LBFYDB, LBKYDB, LBNODB, LFIXDB, LREFDB(7) 
     +              , LSAVDB, LTOPDB, LPRTDB, NTOPDB    
      PARAMETER       (NDMXDB=25000)    
*   
      COMMON /DINITL/ TOPLDI, TOPNDI    
      CHARACTER       TOPNDI*16, TOPLDI*16  
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
      PARAMETER       (NMLMDM=10)   
      COMMON /DMULOP/ INDXDM(NMLMDM), KEYVDM(NMLMDM), KTYPDM(NMLMDM)    
     +              , LFKYDM(NMLMDM), NOCCDM(NMLMDM)    
*   
      COMMON /DTKXIN/ ICURDT, IDNRDT, IKYLDT, IMINDT, INRSDT, IPRVDT    
     +              , IUSEDT, KEY6DT, KY6NDT, MNKYDT, NTIMDT    
*   
      PARAMETER       (NZ=0)    
      DIMENSION       KEYS(LNK), LBK(9)
      CHARACTER       PATH*80, PATHN*(*), PATHX*16, CHOPT*(*)
      LOGICAL         KHERE
*
*     ------------------------------------------------------------------    
*
      LBK(1) = 0
      CALL DBOPTS (CHOPT)   
      IF (IQUEST(1).NE.0)       GO TO 999
      IF (IOPMDA.NE.0 .AND. IOPSDA.NE.0) THEN
        IQUEST(1) = 1
        IF (IDEBDB.GT.0) CALL DBPRNT (LPRTDB, '(/,'' DBUSE : Illegal '//
     +  'Character option - S/M options are mutually exclusive'')',
     +  IARGDB, 0)
        GO TO 999
      ENDIF
*
      CALL DBSBLC (PATHN, PATH, NCHAR)
*
      CALL DBNODE (PATH, LBNODB)
      IF (IQUEST(1).NE.0)     GO TO 999
*
      IF (IOPSDA.EQ.0 .OR. IOKYDA(3) .EQ. 0 .OR.
     &    IOKYDA(4) .EQ. 0 .OR. LNK .LT. 7)  THEN
         IQUEST(1) = 22
         GOTO 999
      END IF
*   
*  ** Check if this Key bank already exists in memory
*
      LBKYDB = LQ(KOFUDB+LBNODB-KLKYDB)
      DO WHILE (LBKYDB .GT. 0)
         IF (KEYS(3) .GE. IQ(KOFUDB+LBKYDB+3) .AND. 
     &       KEYS(4) .LE. IQ(KOFUDB+LBKYDB+4)) THEN
             KHERE = .TRUE.
             DO I = 7,LNK
                IF (IOKYDA(I) .EQ. 1 .AND. 
     &             (IQ(KOFUDB+LBKYDB+I) .NE. KEYS(I))) KHERE = .FALSE.
             END DO
             IF (KHERE) THEN
                LBK(1) = LBKYDB
                GOTO 999
             END IF
         END IF
         LBKYDB = LQ(KOFUDB+LBKYDB)
      END DO
*
999   RETURN
      END

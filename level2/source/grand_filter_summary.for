      FUNCTION GRAND_FILTER_SUMMARY(TTIME,NUMB,LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Send summary report status of filtering for
C-              VMS_FILTER
C-
C-   Returned value  : TRUE always
C-   Inputs  : LUN  the logical unit to write to
C-           : NUMB  number of files in the sum
C-           : TTIME the total time in "D0 units (ask Drew)"
C-   Outputs : None
C-   Controls: None
C-
C-   Created 2-FEB-1992   James T. Linnemann   grand summary 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL GRAND_FILTER_SUMMARY
      INCLUDE 'D0$INC:FILTER_COM.INC'
      INTEGER LUN,I,ITOOL,K,LENINT,M,NUMB
      LOGICAL NOT_FOUND
      CHARACTER*128 MSGLIN(512)
      REAL PERCNT
      INCLUDE 'D0$PARAMS:L2_TYPE.DEF'
      INCLUDE 'D0$INC:GRAND_FILTER_COUNT.INC'
      INCLUDE 'D0$INC:GRAND_FILTER_TIMING.INC'
      CHARACTER*16 TYPE_NAME,PNAME(MAXPARAM)
      CHARACTER*10 TRY_TYPE
      CHARACTER*32 NAME
      CHARACTER*1 PTYPE(MAXPARAM)
      INTEGER N1,N2,NTOT,NTOOLS,NPARS,TOOL_NUM(MAXTOOL)
      REAL TFILT,TPCT,TSCRIPT,TTOOL,AVG,SIGMA,TTIME
      REAL NCALL,NEVT,NSCALL,NTCALL,ERR,ERREVT,ERRTRY,ERRPASS
      INTEGER LENT,TRULEN
      REAL NTRY,NPASS,NTTRY(MAXTOOL),NTPASS(MAXTOOL)
      CHARACTER*16 TNAME(MAXTOOL)
      INTEGER L1_BIT
      CHARACTER*64 L1_NAME,L2_NAME
      INTEGER NRUN,RUNNO,N,IER
C----------------------------------------------------------------------
      NOT_FOUND=.TRUE.
      NAME = 'VMS_FILTER'
      CALL GET_L2TYPE_INFO(NAME,NTOOLS,TOOL_NUM)
      LENT = 0
      DO ITOOL = 1,NTOOLS
        NTTRY(ITOOL) = 0
        NTPASS(ITOOL) = 0
       CALL GET_L2TOOL_INFO(ITOOL,TNAME(ITOOL),NPARS,PNAME,PTYPE)
        LENT = MAX(LENT,TRULEN(TNAME(ITOOL)))   ! find max length of tool name
      ENDDO
                                       ! directory
      CALL SETUP_L1_L2_INFO(IER)   !read file associating triggers and filters

      M = 2
      MSGLIN(1)='                FILTER REPORT AT *GRAND* SUMMARY TIME'
      MSGLIN(2)='                -------------------------------------'
      NEVT = GRAND_TOT_EVENTS(1)
      ERREVT = GRAND_TOT_ERR(1)
      IF (NEVT.LE.0.) NEVT = 1.E-20
      TFILT = GRAND_NCALL*GRAND_AVG
      IF ( TFILT.LE.0)  TFILT = 1.0
      M=M+1
      MSGLIN(M) = ' '
      M = M + 1
      WRITE(MSGLIN(M),70)
   70 FORMAT(14X,
     &  '  Try if  #Try/    Pass   Time   RMS  Time/',
     &  ' L1 mubarn',' (error)')
      M = M + 1
      WRITE(MSGLIN(M),80)
   80 FORMAT(14X,
     &  '  L1 bit?   Evt    Rate   (ms)  (ms)  Total',
     &  ' L2 mubarn',' (error)')
      M = M + 1
      WRITE(MSGLIN(M),90)
   90 FORMAT(15X,
     &  '--------  ----    ----   ----  ----  -----',
     &  ' --------- -------')
      TSCRIPT = 0
      NSCALL = 0
      DO I=0,127
        IF(FILTER_SCRIPT(1,1,I).NE.0) THEN
          NOT_FOUND=.FALSE.
          NTRY=GRAND_FILTER_COUNT(1,0,I)
          ERRTRY = GRAND_FILTER_ERR(1,0,I)
          NPASS=GRAND_FILTER_COUNT(2,0,I)
          ERRPASS = GRAND_FILTER_ERR(2,0,I)
          NSCALL = NSCALL + NTRY
          IF( NTRY.GT.0) THEN
            PERCNT = 100.*(NPASS)/ NTRY 
          ELSE 
            PERCNT=0.0
          ENDIF
          IF(FILTER_FORCE(I)) THEN 
            TRY_TYPE = 'Always'
          ELSE
            TRY_TYPE = 'As Needed'
          ENDIF
          NCALL = GRAND_SNCALL(I)
          AVG = GRAND_SAVG(I)
          SIGMA = GRAND_SSIGMA(I)
            TPCT = 100*NTRY*AVG/TFILT
            TSCRIPT = TSCRIPT + NTRY*AVG
          M=M+1
          CALL GET_L1_L2_INFO(I,L1_BIT,L1_NAME,L2_NAME)
          WRITE(MSGLIN(M),104) L1_BIT,L1_NAME,I,L2_NAME,NTRY,ERRTRY
  104     FORMAT('L1 bit',I3,1X,A17,' L2 bit',I3,1X,A17,1PG12.3,
     &      1PG10.3)
          M=M+1
          WRITE(MSGLIN(M),105) I,TRY_TYPE,NTRY/NEVT,
     &        PERCNT,(1000.*AVG),(1000.*SIGMA),TPCT,NPASS,ERRPASS
  105     FORMAT('FILTER bit',I3,1X,A9,F5.2,
     &      F8.1,'%',F7.1,2F6.1,'%',1PG10.3,1PG10.3)
          DO K=1,MAX_TOOL_FILTER
            IF(FILTER_SCRIPT(1,K,I).NE.0) THEN
              ITOOL = FILTER_SCRIPT(1,K,I)
              NAME = TNAME(ITOOL)
              NTRY = GRAND_FILTER_COUNT(1,K,I)
              ERRTRY = GRAND_FILTER_ERR(1,K,I)
              NTTRY(ITOOL) = NTTRY(ITOOL) + NTRY
              NPASS = GRAND_FILTER_COUNT(2,K,I)
              
              NTPASS(ITOOL) = NTPASS(ITOOL) + NPASS
              IF( NTRY.GT.0) THEN
                PERCNT = 100.* NPASS/ NTRY 
              ELSE 
                PERCNT=0.0
              ENDIF
              M=M+1
              WRITE(MSGLIN(M),115) FILTER_SCRIPT(1,K,I),NAME(1:LENT),
     &                  FILTER_SCRIPT(2,K,I),PERCNT
  115         FORMAT(' #',I2,' ',A12, ' Set',I3,7X, F5.1,'%')
            ENDIF
          ENDDO
          M=M+1
          MSGLIN(M)=' '    !Blank line for spacing
        ENDIF
      ENDDO
      PERCNT=100.*(GRAND_TOT_EVENTS(2))/NEVT
      NCALL = GRAND_NCALL
      AVG = GRAND_AVG
      SIGMA = GRAND_SIGMA
      M = M + 1
      WRITE(MSGLIN(M),116) GRAND_TOT_EVENTS(2),NEVT,PERCNT,
     &  (1000.*AVG),(1000.*SIGMA)
  116 FORMAT(' Overall Filtering',
     &          1PG10.3,' L2 /',1PG10.3,' L1 =',0PF7.1,'%',F7.1,' ms',
     &          F6.1,' RMS' )
      M=M+1
      MSGLIN(M)=' '    !Blank line for spacing
      M = M + 1
      WRITE(MSGLIN(M),117) GRAND_AVGBITS
  117 FORMAT( ' L1 Bits Set/Event =',F7.2,' L2 Bits Passed/Event ='
     &  ,F7.2)
      M = M + 1
      WRITE (MSGLIN(M),120)NSCALL/NEVT,TSCRIPT/TFILT
  120 FORMAT(' #  Scripts/Event =  ',F6.2,
     &  ';      T(Sum) Scripts/Filt =',F7.3)
      M=M+1
      MSGLIN(M)=' '    !Blank line for spacing
      M=M+1
      MSGLIN(M)= 'TOOL Results:'
      M=M+1
      WRITE(MSGLIN(M),121)
  121 FORMAT(20X,
     &  '#Try/  Pass    Time   RMS  Time/',
     &  '    Time')
      M=M+1
      WRITE(MSGLIN(M),122)
  122 FORMAT(20X,
     &  '  Evt  Rate    (ms)  (ms)  Total',
     &  '  (100K SQRTs)')
      M=M+1
      WRITE(MSGLIN(M),123)
  123 FORMAT(20X,
     &  '  ---  ----    ----  ----  -----  ----------')
      TTOOL = 0
      NTCALL = 0
      DO I = 1,NTOOLS
        NAME = TNAME(I) 
        NCALL = NTTRY(I)
        NTCALL = NTCALL + NCALL
        NCALL = GRAND_TNCALL(I)
        AVG = GRAND_TAVG(I)
        SIGMA = GRAND_TSIGMA(I)
          TPCT = 100*NCALL*AVG/TFILT
          TTOOL = TTOOL + NCALL*AVG
        IF (NCALL.GT.0) THEN
          PERCNT = 100.*NTPASS(I)/NCALL
        ELSE
          PERCNT = 0
        ENDIF
        M = M + 1
        WRITE(MSGLIN(M),125) I,NAME(1:LENT),NCALL/NEVT,
     &          PERCNT,(1000.*AVG),(1000.*SIGMA),TPCT,
     &          FLOAT(NUMB)*AVG/TTIME
  125   FORMAT(I2,1X,A16,F6.2,
     &      F6.1,'%',F7.1,2F6.1,'%',2X,E10.4)
      ENDDO
      M=M+1
      MSGLIN(M)=' '    !Blank line for spacing
      M = M + 1
      WRITE (MSGLIN(M),126)NTCALL/NEVT,TTOOL/TFILT
  126 FORMAT('   # Tools Called/Event =',F6.2,
     &       '    T(Sum) Tools/Filt =',F7.3)
      IF(NOT_FOUND) THEN
        WRITE(LUN,130)
  130   FORMAT('0No bit definitions available'//
     &        ' at SUMMARY time!'/)
      ELSE
        WRITE(LUN,135)
  135   FORMAT('1')        !Issue form-feed
        WRITE(LUN,140) (MSGLIN(K)(1:TRULEN(MSGLIN(K))),K=1,M) !trim
  140   FORMAT(' ',A)
      ENDIF
      GRAND_FILTER_SUMMARY=.TRUE.
      RETURN      
      END

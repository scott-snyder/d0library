      SUBROUTINE DIFF_FSUM(IFILE,TIME_IN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  loads two FILTER_SUMMARY files and outputs
C-              stuff telling you about the differences
C-
C-   Inputs  :IFILE   ith file (should be either 1 or 2, already checked)
C-            TIME    time (in D0 units [ask Drew] from this file)
C-   Outputs :
C-   Controls:
C-
C-   Created   10-MAR-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:FILTER_COM.INC'
      INCLUDE 'D0$INC:GRAND_FILTER_COUNT.INC'
      INCLUDE 'D0$PARAMS:L2_TYPE.DEF'
      INCLUDE 'D0$INC:FSUM.INC'
      INCLUDE 'D0$INC:GRAND_FILTER_TIMING.INC'
C
C
      INTEGER I,J,K,M,IFILE,LUN
      REAL    WTOLD,OLD,NEW,TIME_IN
C
      CHARACTER*128 MSGLIN(2000)
      REAL DRUN(2),DCROSS(2),DWEIGHT(2),DTIME(2)
      INTEGER DNCALL(2),DSNCALL(0:127,2),DTNCALL(MAXTOOL,2)
      REAL DTOT_EVENTS(3,2)
      INTEGER DFILTER_COUNT(2,0:MAX_TOOL_FILTER,0:127,2),NCALL
      REAL    DAVGBITS(2,2),DAVG(2),DSAVG(0:127,2),DTAVG(MAXTOOL,2),AVG
      REAL    DSIGMA(2),DSSIGMA(0:127,2),DTSIGMA(MAXTOOL,2),SIGMA

C
      REAL PERCNT(2)
      CHARACTER*16 TYPE_NAME,PNAME(MAXPARAM)
      CHARACTER*10 TRY_TYPE
      CHARACTER*32 NAME
      CHARACTER*64 L1_NAME,L2_NAME
      CHARACTER*1 PTYPE(MAXPARAM)
      INTEGER N1,N2,NTOT,NTOOLS,NPARS,TOOL_NUM(MAXTOOL),DX,ITOOL
      REAL TFILT(2),TPCT(2),TSCRIPT(2),TTOOL(2),AVGN(2),SIGMAN(2)
      REAL NEVT(2),NSCALL(2),NTCALL(2),ERR,PCNT
      REAL DIFFTRY,DIFFP,DIFFAV,DIFFSIG
      INTEGER LENT,TRULEN
      REAL NTRY(2),NPASS(2),NTTRY(MAXTOOL,2),NTPASS(MAXTOOL,2)
      CHARACTER*16 TNAME(MAXTOOL)
      INTEGER L1_BIT
      INTEGER NRUN,RUNNO,N,IER
      LOGICAL NOT_FOUND
C
C       check IFILE (to be sure)
C
      IF (IFILE.LT.1.OR.IFILE.GT.2) THEN
          CALL ERRMSG('TOO_MANY','DIFF_FSUM',
     &    'Too many files in GRAND_FSUM.RCP with DIFF selected','F')
      ENDIF
C
      DWEIGHT(IFILE) = EVENT_WEIGHT
      IF (EVENT_WEIGHT.LE.0) THEN
        CALL ERRMSG('ZERO_WEIGHT','SUM_FSUM',' zero weight; set to 1.0',
     &    'W')
        DWEIGHT(IFILE) = 1.0
      ENDIF
C
C     load up
C
      DRUN(IFILE) = NRUN_READ
      DCROSS(IFILE) = CROSS_SECT
      DTIME(IFILE) = TIME_IN
      DAVG(IFILE) = FAVG
      DSIGMA(IFILE) = FSIGMA
      DNCALL(IFILE) = FNCALL
      DO I = 1,2
        DO J = 0,MAX_TOOL_FILTER
          DO K = 0,127
            DFILTER_COUNT(I,J,K,IFILE) = FILTER_COUNT(I,J,K)
          ENDDO
        ENDDO
      ENDDO
C
      DO I = 1,3
        DTOT_EVENTS(I,IFILE) = TOT_EVENTS_READ(I)
      ENDDO
      DO I = 1,2
        DAVGBITS(I,IFILE) = AVGBITS(I)
      ENDDO
C
C     timing summary per script
C
      DO I = 0,127
        DSNCALL(I,IFILE) = SNCALL(I)
        DSAVG(I,IFILE) = SAVG(I)
        DSSIGMA(I,IFILE) = SSIGMA(I)
      ENDDO
C
C     timing summary per tool
C
      DO I=1,MAXTOOL
        DTNCALL(I,IFILE) = TNCALL(I)
        DTAVG(I,IFILE) = TAVG(I)
        DTSIGMA(I,IFILE) = TSIGMA(I)
      ENDDO
C
C     that's all folks
C
      GOTO 999
C
C----------------------------------------------------------------------
      ENTRY GRAND_FILTER_DIFF(LUN)
C
C     take what is in the above arrays, output results for differences
C
      NOT_FOUND=.TRUE.
      NAME = 'VMS_FILTER'
      CALL GET_L2TYPE_INFO(NAME,NTOOLS,TOOL_NUM)
      LENT = 0
      DO ITOOL = 1,NTOOLS
        CALL GET_L2TOOL_INFO(ITOOL,TNAME(ITOOL),NPARS,PNAME,PTYPE)
        LENT = MAX(LENT,TRULEN(TNAME(ITOOL)))   ! find max length of tool name
        DO I=1,2
          NTTRY(ITOOL,I) = 0
          NTPASS(ITOOL,I) = 0
        ENDDO
      ENDDO
      CALL SETUP_L1_L2_INFO(IER)   !read file associating triggers and filters
      M = 2
      MSGLIN(1)='               FILTER REPORT AT DIFFERENCE TIME '
      MSGLIN(2)='               --------------------------------'
      M=M+1
      MSGLIN(M) = ' '
C
C     normalize each one
C
      DO I=1,2
        NEVT(I) = DTOT_EVENTS(1,I)
        IF (NEVT(I).LE.0.) NEVT(I) = 1.
        TFILT(I) = DNCALL(I)*DAVG(I)
        IF ( TFILT(I).LE.0)  TFILT(I) = 1.0
        TSCRIPT(I) = 0
        NSCALL(I) = 0
        NTCALL(I) = 0
        TTOOL(I) = 0
      ENDDO
C
      M = M + 1
      WRITE(MSGLIN(M),70)
   70 FORMAT(15X,'Try if          #Try/   Pass     Time  ',
     &  'Time/  L1 mubarn')
      M = M + 1
      WRITE(MSGLIN(M),80)
   80 FORMAT(15X,'L1 bit?  Parset   Evt     Rate   (ms)  ',
     &  'Total  L2 mubarn')
      M = M + 1
      WRITE(MSGLIN(M),90)
   90 FORMAT(15X,'-------  ------  ----   ------   ----  ',
     &  '-----  ---------')
      DO I=0,127
        IF(FILTER_SCRIPT(1,1,I).NE.0) THEN
C
C         compare
C
          CALL GET_L1_L2_INFO(I,L1_BIT,L1_NAME,L2_NAME)
          NOT_FOUND=.FALSE.
          IF(FILTER_FORCE(I)) THEN
            TRY_TYPE = 'Always'
          ELSE
            TRY_TYPE = 'As Needed'
          ENDIF
C
          DO J=1,2
            NTRY(J) = DFILTER_COUNT(1,0,I,J)
            NPASS(J) = DFILTER_COUNT(2,0,I,J)
            NSCALL(J) = NSCALL(J) + NTRY(J)
            IF( NTRY(J).GT.0) THEN
              PERCNT(J) = 100.*(NPASS(J))/ NTRY(J)
            ELSE
              PERCNT(J)=0.0
            ENDIF
            AVGN(J) = DSAVG(I,J)
            SIGMAN(J) = DSSIGMA(I,J)
            TPCT(J) = 100*NTRY(J)*AVGN(J)/TFILT(J)
            TSCRIPT(J) = TSCRIPT(J) + NTRY(J)*AVGN(J)
          ENDDO
C
C         dump into MSGLIN
C
          M=M+1
          WRITE(MSGLIN(M),
     &        '(''L1 bit'',I3,1X,A17,33X,1PG10.3)') 
     &        L1_BIT,L1_NAME,
     &        NTRY(1)*DWEIGHT(1)-NTRY(2)*DWEIGHT(2)
          M=M+1
          WRITE(MSGLIN(M),
     &        '(''L2 bit'',I3,1X,A17,33X,1PG10.3)')
     &        I,L2_NAME,
     &        DWEIGHT(1)*NPASS(1)-DWEIGHT(2)*NPASS(2)
C
          DIFFTRY = (NTRY(1)/NEVT(1)) - (NTRY(2)/NEVT(2))
          DIFFP = PERCNT(1) - PERCNT(2)
          DIFFAV = 1000.*( AVGN(1) - AVGN(2) )
          DIFFSIG = 1000.*( SIGMAN(1) - SIGMAN(2) )
C
          M=M+1
          WRITE(MSGLIN(M),105) I,TRY_TYPE,DIFFTRY,
     &        DIFFP,DIFFAV,TPCT(1)-TPCT(2)
  105     FORMAT('FILTER bit',I3,2X,A9,6X,F6.2,
     &      F8.1,'%',F7.1,F6.1'%')
C
C         loop over tools for this script
C
          DO K=1,MAX_TOOL_FILTER
            IF(FILTER_SCRIPT(1,K,I).NE.0) THEN
C
C             both scripts have this tool
C
              ITOOL = FILTER_SCRIPT(1,K,I)
              NAME = TNAME(ITOOL)
              DO J=1,2
                NTRY(J) = DFILTER_COUNT(1,K,I,J)
                NTTRY(ITOOL,J) = NTTRY(ITOOL,J) + NTRY(J)
                NPASS(J) = DFILTER_COUNT(2,K,I,J)
                NTPASS(ITOOL,J) = NTPASS(ITOOL,J) + NPASS(J)
                IF( NTRY(J).GT.0) THEN
                  PERCNT(J) = 100.* NPASS(J)/NTRY(J)
                ELSE
                  PERCNT (J) = 0.0
                ENDIF
              ENDDO
C
              M=M+1
              WRITE(MSGLIN(M),115) FILTER_SCRIPT(1,K,I),NAME(1:LENT),
     &                  FILTER_SCRIPT(2,K,I),PERCNT(1)-PERCNT(2)
  115         FORMAT(' #',I2,' ',A12,8X,I5,8X,F6.1,'%')
            ENDIF
          ENDDO
          M=M+1
          MSGLIN(M)=' '    !Blank line for spacing
        ENDIF
      ENDDO
C
      PCNT=100.*(DTOT_EVENTS(1,1)/NEVT(1) - DTOT_EVENTS(1,2)/NEVT(2))
      FNCALL = DNCALL(1) - DNCALL(2)
      FAVG = DAVG(1) - DAVG(2)
      M = M + 1
      WRITE(MSGLIN(M),116) DTOT_EVENTS(1,1)-DTOT_EVENTS(1,2),
     &  NEVT(1)-NEVT(2),PCNT,1000.*(DAVG(1)-DAVG(2)),
     &  1000.*(DSIGMA(1)-DSIGMA(2))
  116 FORMAT(' Overall Filtering',
     &          1PG10.3,' L2 /',1PG10.3,' L1 =',0PF7.1,'%',F7.1,' ms',
     &          F6.1,' RMS' )
      M=M+1
      MSGLIN(M)=' '    !Blank line for spacing
      M = M + 1
      WRITE(MSGLIN(M),117) DAVGBITS(1,1)-DAVGBITS(1,2),
     &  DAVGBITS(2,1)-DAVGBITS(2,2)
  117 FORMAT( ' L1 Bits Set/Event =',F7.2,' L2 Bits Passed/Event ='
     &  ,F7.2)
      M = M + 1
      WRITE (MSGLIN(M),120) NSCALL(1)/NEVT(1)-NSCALL(2)/NEVT(2),
     &  TSCRIPT(1)/TFILT(1)-TSCRIPT(2)/TFILT(2)
  120 FORMAT(' #  Scripts/Event =  ',F6.2,
     &  ';      T(Sum) Scripts/Filt =',F7.3)
      M=M+1
      MSGLIN(M)=' '    !Blank line for spacing
      M=M+1
      MSGLIN(M)= 'TOOL Results:'
      M=M+1
      WRITE(MSGLIN(M),121)
  121 FORMAT(31X,'#Try/  Pass    Time   RMS  Time/    Time')
      M=M+1
      WRITE(MSGLIN(M),122)
  122 FORMAT(' Tool#/File',13X,' #Try  ',
     &  '  Evt  Rate    (ms)  (ms)  Total',
     &  '  (D0 units)')
      M=M+1
      WRITE(MSGLIN(M),123)
  123 FORMAT(23X,'------ ',
     &  ' -----  ----    ----  ----  -----  ----------')
C
C     loop over tools
C
      DO I = 1,NTOOLS
        DO J=1,2
          NAME = TNAME(I)
          NCALL = NTTRY(I,J)
          NTCALL(J) = NTCALL(J) + NCALL
          NCALL = DTNCALL(I,J)
          AVG = DTAVG(I,J)
          SIGMA = DTSIGMA(I,J)
          TPCT(J) = 100*NCALL*AVG/TFILT(J)
          TTOOL(J) = TTOOL(J) + NCALL*AVG
          IF (NCALL.GT.0) THEN
            PERCNT(J) = 100.*NTPASS(I,J)/NCALL
          ELSE
            PERCNT(J) = 0
          ENDIF
          M = M + 1
          WRITE(MSGLIN(M),125) I,J,NAME(1:LENT),NCALL,
     &      FLOAT(NCALL)/NEVT(J),
     &          PERCNT(J),(1000.*AVG),(1000.*SIGMA),TPCT(J),
     &          AVG/DTIME(J)
  125     FORMAT(I2,' /',I2,A16,I7,F6.2,
     &      F6.1,'%',F7.1,2F6.1,'%',2X,E10.4)
        ENDDO
C
C       now output the difference
C
        M = M + 1
        WRITE(MSGLIN(M),225)  NAME(1:LENT),DTNCALL(I,1)-DTNCALL(I,2),
     &    FLOAT(DTNCALL(I,1))/NEVT(1)-
     &    FLOAT(DTNCALL(I,2))/NEVT(2),
     &    PERCNT(1)-PERCNT(2),1000.*(DTAVG(I,1)-DTAVG(I,2)),
     &    1000.*(DTSIGMA(I,1)-DTSIGMA(I,2)),TPCT(1)-TPCT(2),
     &    DTAVG(I,1)/DTIME(1)-DTAVG(I,2)/DTIME(2)
  225     FORMAT('DIFF=>',A16,I7,F6.2,
     &      F6.1,'%',F7.1,2F6.1,'%',2X,E10.4)
        M = M + 1
        MSGLIN(M)=' '    !Blank line for spacing
      ENDDO
      M=M+1
      MSGLIN(M)=' '    !Blank line for spacing
      M = M + 1
      WRITE (MSGLIN(M),126) NTCALL(1)/NEVT(1)-NTCALL(2)/NEVT(2),
     &  TTOOL(1)/TFILT(1)-TTOOL(2)/TFILT(2)
  126 FORMAT('   # Tools Called/Event =',F6.2,
     &       '    T(Sum) Tools/Filt =',F7.3)
C
C     flush to file
C
      IF(NOT_FOUND) THEN
        WRITE(LUN,130)
  130   FORMAT('0No bit definitions available'//
     &        ' at SUMMARY time!'/)
      ELSE
        WRITE(LUN,135)
  135   FORMAT('1')        !Issue form-feed
        WRITE(LUN,140) (MSGLIN(K)(1:MAX(1,TRULEN(MSGLIN(K)))),K=1,M) !trim
  140   FORMAT(' ',A)
      ENDIF
C
C     that's really all folks
C
  999 RETURN
      END

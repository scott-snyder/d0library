      SUBROUTINE EVTWOS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       manage offline output streams and write events to the
C-       selected ones. Maximum number of output streams is 8.
C-       6 available for users + STA and DST outputs
C-       One can have multiple DST and STA output streams
C-       but not multiple user streams
C-
C-   ENTRY EVOPWO(OSTRM,ONAME,XOPT,OK):
C-       request output stream OSTRM to be open
C-   Input:
C-      OSTRM= CHARACTER*3 output stream identifier
C-             STA for standard output
C-             DST for DST output
C-             any other XXX for users
C-      ONAME= output stream file name
C-      XOPT= character option for file mode 'X'= for standard exchange
C-                                           'G'= for GEANT exchange
C-                                           'Z'= for ZZIP
C-            any other character is interpreted as native mode
C-   Output:
C-      OK   = true if successfull
C-
C-   ENTRY EVCLWO(OSTRM): close output stream OSTRM
C-
C-   Input:
C-   OSTRM = CHARACTER*3 output stream name
C-
C-   ENTRY EVWRITES(OSTRM,NWR): return number of events written
C-   Input:
C-   OSTRM = CHARACTER*3 output stream name
C-   Output:
C-   NWR   = number of events written to output stream OSTRM
C-
C-   ENTRY EVTWOS_MULT(ON): to be called if more than 1
C-                      DST and STA streams are required
C-   Input:
C-   ON = true(false) turn on(off) multiple stream features
C-
C-   Created  25-MAY-1989   Serban D. Protopopescu
C-   Updated  21-FEB-1992   Serban D. Protopopescu  add multiple streams 
C-   Updated  17-Nov-1992   Kirill Denisenko
C-      Added remote memory to memory transfer option
C-   Updated   8-MAR-1993   Hailin Li and Kirill Denisenko
C-                          Added parallel RECO option
C-   Updated  28-NOV-1994   sss - added zzip support
C-   Updated  17-Jan-1996   sss - tpmfarm doesn't work on linux yet.
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*3 OSTRM,OSTRM_IN
      CHARACTER*(*) ONAME,XOPT
      LOGICAL OK,ON
      INTEGER NMAXO,NMAX2
      PARAMETER (NMAXO=8)
      PARAMETER (NMAX2=NMAXO-2)
      INTEGER OUNITS(NMAXO),NUSER,IER,GIVEO,CHOSEO,I,K,LOOP,ISTRM
      INTEGER NWRITES(NMAXO),NWR,N,ILEN
      LOGICAL FLGVAL,FLGCHK,MULTIPLE,EVT_DSTDROP, CLOSE_OK
      CHARACTER*16 STREAM
      CHARACTER*4 CHOPT

C-- VARIABLES FOR REMOTE TRANSFER
C&IF VAXVMS,VAXELN,LINUX
C&ELSE
C&      INTEGER*4 DUMMY, IDUMMY(10), IERR
C&      EXTERNAL FARM_OUT
C&ENDIF

      SAVE OUNITS,NUSER,NWRITES,MULTIPLE
      DATA NUSER/2/
      DATA NWRITES/NMAXO*0/
      DATA OUNITS/NMAXO*0/
      DATA MULTIPLE/.FALSE./
C----------------------------------------------------------------------
C
      IF(FLGVAL('WRITE_STREAM_STA')) THEN
        CALL EVMARK('STA')     ! mark banks to drop from Standard stream
        NWRITES(1)=NWRITES(1)+1
        IF (MULTIPLE) THEN
          CALL EVWR_MULT(1)
        ELSE
          CALL OUTUTAG(1,NWRITES(1))
C ********* use WREVNT instead of WREVTM until MZMARK is fixed ****
          GIVEO=OUNITS(1)
          CALL WREVNT(GIVEO)
C ****************************************************************
        ENDIF
      ENDIF
C
      IF(FLGVAL('WRITE_STREAM_DST')) THEN
        CALL UNCOMPRESS_ZEBRA
        CALL EVMARK('STA')     ! mark banks to drop from Standard stream
        CALL EVMARK('DST')     ! mark banks to drop from DST stream
        IF(EVT_DSTDROP()) THEN       ! user hook for special drops
          NWRITES(2)=NWRITES(2)+1
          IF (MULTIPLE) THEN
            CALL EVWR_MULT(2)
          ELSE
C ********* use WREVNT instead of WREVTM until MZMARK is fixed ****
            CALL OUTUTAG(2,NWRITES(2))
            GIVEO=OUNITS(2)
            CALL WREVNT(GIVEO)
C ****************************************************************
          ENDIF
        ENDIF
      ENDIF
      CALL EV_STREAMS_OFF         ! turn off streams
C
      DO 1 I= 3,NUSER
        CALL EVSTRM_ITOC(I,OSTRM_IN) ! get stream name from index
        WRITE(STREAM,101) OSTRM_IN
        IF(FLGVAL(STREAM)) THEN
          CALL EVMARK(OSTRM_IN) ! mark banks to drop from USER streams
          GIVEO=OUNITS(I)
          NWRITES(I)=NWRITES(I)+1
          CALL OUTUTAG(I,NWRITES(I))
C ********* use WREVNT instead of WREVTM until MZMARK is fixed ****
C        CALL WREVTM(GIVEO)
          CALL WREVNT(GIVEO)
C ****************************************************************
        ENDIF
    1 CONTINUE
C
      GOTO 999   ! exit
C
C
      ENTRY EVOPWO(OSTRM,ONAME,XOPT,OK)
C
      WRITE(STREAM,101) OSTRM
      IF(.NOT.FLGCHK(STREAM)) CALL FLGBK(STREAM,1)
      CALL FLGSET(STREAM,.TRUE.)
      IF(OSTRM.EQ.'STA') THEN
        CHOSEO=1
      ELSEIF (OSTRM.EQ.'DST') THEN
        CHOSEO=2
      ELSE
        NUSER=NUSER+1
        IF(NUSER.GT.NMAXO-2) THEN
          CALL ERRMSG(' Exceeded number of allowed files','EVTWOS',
     &        ' New file not opened','W')
          GOTO 999   ! exit
        ENDIF
        CHOSEO=NUSER
      ENDIF
C
C              multiple DST and STA streams
      IF(MULTIPLE.AND.CHOSEO.LT.3) THEN
        CALL EVOP_MULT(CHOSEO,ONAME,XOPT,OK)
        GOTO 999      ! done
      ENDIF
C             single streams
      GIVEO=OUNITS(CHOSEO)
      IF(GIVEO.EQ.0) THEN          ! first call for OSTRM get unit number
        CALL GTUNIT(88,GIVEO,IER)
C&IF VAXVMS,VAXELN,LINUX
C&ELSE
C&        IF((CHOSEO.EQ.1) .AND. FLGVAL('REMOTE_STA')) THEN
C&          CALL SRV_INI
C&          CALL FZFILE(GIVEO,8190,'CSO')
C&          CALL FZHOOK(GIVEO,FARM_OUT,DUMMY)
C&        ELSE IF(FLGVAL('PARALLEL')) THEN
C&          CALL CLI_INI(GIVEO,CHOSEO,1,OK)
C&        ELSE
C&ENDIF
        CHOPT='OU'
        IF(XOPT(1:1).EQ.'X') CHOPT='XO'
        IF(XOPT(1:1).EQ.'G') CHOPT='GO'
        IF(XOPT(1:1).EQ.'Z') CHOPT='ZO'
        CALL D0OPEN(GIVEO,ONAME,CHOPT,OK)
        CALL XZRECL(ILEN,CHOPT)
        IF (ILEN .GE. 0) CALL FZFILE(GIVEO,ILEN,CHOPT)
C&IF VAXVMS,VAXELN,LINUX
C&ELSE
C&        ENDIF
C&ENDIF
      ENDIF
C
      IF(OUNITS(CHOSEO).NE.0) THEN    ! if called again open new file
        CALL D0CLOSE(GIVEO, ' ', CLOSE_OK)
        CHOPT='OU'
        IF(XOPT(1:1).EQ.'X') CHOPT='XO'
        IF(XOPT(1:1).EQ.'G') CHOPT='GO'
        IF(XOPT(1:1).EQ.'Z') CHOPT='ZO'
        CALL D0OPEN(GIVEO,ONAME,CHOPT,OK)
      ENDIF
C
      OUNITS(CHOSEO)=GIVEO
      CALL EVSTRM_STOR(OSTRM,CHOSEO)
C
      GOTO 999    ! exit
C
C
      ENTRY EVCLWO(OSTRM)
C
      LOOP=1
      N=1
      IF(OSTRM.EQ.'ALL') LOOP=NMAXO
C          STA and DST are closed separately for multiple streams
      IF(MULTIPLE) THEN  
        IF(OSTRM.EQ.'STA'.OR.OSTRM.EQ.'ALL') 
     &    CALL EVCL_MULT(1)
        IF(OSTRM.EQ.'DST'.OR.OSTRM.EQ.'ALL') 
     &    CALL EVCL_MULT(2)
        N=3
      ENDIF
C
      CALL EVSTRM(OSTRM,CHOSEO)
      IF(CHOSEO.EQ.0.AND.LOOP.EQ.1) GOTO 999   ! no file was opened
      OSTRM_IN=OSTRM
C
      DO 22 K=N,LOOP
        IF(LOOP.EQ.NMAXO) THEN
          CHOSEO=K
          CALL EVSTRM_ITOC(K,OSTRM_IN)
        ENDIF
        GIVEO=OUNITS(CHOSEO)
        IF(GIVEO.NE.0) THEN
          WRITE(STREAM,101) OSTRM_IN
          CALL FLGUBK(STREAM,1)
          CALL FZENDO(GIVEO,'T')

C&IF VAXVMS,VAXELN,LINUX
C&ELSE
C&          IF(CHOSEO.EQ.1 .AND. FLGVAL('REMOTE_STA')) THEN
C&            CALL SRV_WRI(IDUMMY,0,IERR)
C&            CALL SRV_FIN
C&          ELSE IF(FLGVAL('PARALLEL')) THEN
C&            CALL CLI_FIN
C&          ELSE
C&ENDIF
          CALL D0CLOSE(GIVEO, ' ', CLOSE_OK)
C&IF VAXVMS,VAXELN,LINUX
C&ELSE
C&          ENDIF
C&ENDIF
          CALL RLUNIT(88,GIVEO,IER)
          OUNITS(CHOSEO)=0
C
          IF(CHOSEO.GT.2) THEN
            CALL EVSTRM_STOR('   ',CHOSEO)
            NUSER= NUSER-1         ! stream
          ENDIF
C
        ENDIF
C
   22 CONTINUE
C
      GOTO 999    ! exit
C
C
      ENTRY EVWRITES(OSTRM,NWR)
C
      NWR=0
      CALL EVSTRM(OSTRM,ISTRM)
      IF(ISTRM.NE.0) NWR=NWRITES(ISTRM)
      GOTO 999    ! exit
C
      ENTRY EVTWOS_MULT(ON)
      MULTIPLE=ON
  999 RETURN
C
  101 FORMAT('WRITE_STREAM_',A3)
      END


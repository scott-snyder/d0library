      SUBROUTINE EVWR_MULT(ISTRM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      write to multiple STA or DST output streams
C-   Inputs  : 
C-      ISTRM   = output stream type (STA=1 or DST=2)
C-
C-   ENTRY EV_STREAMS_OFF
C-       turns off all streams
C-   ENTRY EVSET_STREAM(IPICK)
C-       must be called if event is to be written
C-       to stream IPICK
C-
C-   ENTRY EVGET_STREAM(STREAM,IPICK)
C-      must be called to be given a stream number IPICK
C-      for stream STREAM
C-   Inputs:
C-      STREAM= character*5 name for a stream
C-              name has 4th and 5th character ZZ it is treated
C-              as special stream requiring SPECIAL_STREAM hook
C-              ONLY WORKS AT DST STAGE
C-   Output
C-      IPICK = number attached to STREAM
C-
C-   ENTRY EVOP_MULT(ISTRM,ONAME,XOPT,OK)
C-       request all output streams for STA or DST to be open
C-   Input:
C-      ISTRM= 1 for STA, 2 for DST
C-      ONAME= generic output stream file name
C-      XOPT= character option for file mode 'X'= for standard exchange
C-                                           'G'= for GEANT exchange
C-                                           'Z'= for ZZIP
C-            any other character is interpreted as native mode
C-   Output:
C-      OK   = true if successfull
C-
C-   ENTRY EVCL_MULT(ISTRM): close all output streams ISTRM
C-
C-   ENTRY EVGIVE_MULT(NUM_STR,STR_NAME,NEVT_STR,STR_TYPE)
C-       give streams and number of events/stream
C-   Output:
C-     NUM_STR = number of streams
C-     STR_NAME= stream names
C-     NEVT_STR= number of events/stream
C-     STR_TYPE= stream type (STA or DST)
C-
C-   ENTRY EVSUM_MULT(LOUT,ISTRM)
C-      write summaries for stream ISTRM to unit LOUT
C-  ENTRY EVWR_MULT_ZEBPOS(I,ISTRM,IRECORD,IBYTE)
C-      Input:     ISTR is the stream number, ITYP is the type, 1=STA, 2=DST
C-      Output:   IRECORD record off set for this stream event
C-                IBYTE Byte offset for this stream event
C-
C-   Created  21-FEB-1992   Serban D. Protopopescu
C-   Updated  15-JUL-1993   Serban Protopopescu  create SPECIAL stream 
C-                                               option 
C-    Updated 10-FEB-1994   Lee Lueking added entry EVWR_MULT_ZEBPOS
C-                          to provide event off sets for each stream
C-    Updated 28-NOV-1994   sss - add zzip support
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER NSTREAMS,ISTRM,IPICK,LOUT
      CHARACTER*(*) ONAME,XOPT
      CHARACTER*(*) STR_NAME(*),STR_TYPE(*)
      INTEGER NUM_STR,NEVT_STR(*)
      CHARACTER*5 STREAM
      INTEGER MAX_STREAMS
      PARAMETER (MAX_STREAMS=40)
      INTEGER LUN(MAX_STREAMS,2),NWRITES(MAX_STREAMS,2)
      INTEGER I,L,IER,TRULEN,ILEN,ISPECIAL
      LOGICAL DOIT(MAX_STREAMS),OK,SPECIAL_STREAM, CLOSE_OK
      CHARACTER*5 STREAMS(MAX_STREAMS),CHOPT
      CHARACTER*128 NEW_NAME
      SAVE DOIT,NWRITES,STREAMS,LUN
      INTEGER IRECORD,IBYTE,ISTR,ITYP
      INTEGER IZBRECORD(MAX_STREAMS,2),IZBBYTE(MAX_STREAMS,2)
      SAVE IZBRECORD,IZBBYTE  ! ,IRECORD,IBYTE
      DATA NSTREAMS/0/
C----------------------------------------------------------------------
C
      DO I=1,NSTREAMS
        IF(I.EQ.ISPECIAL) THEN
C          IF(SPECIAL_STREAM().AND.ISTRM.EQ.2) THEN
          IF(SPECIAL_STREAM().AND.DOIT(I)) THEN
            NWRITES(I,ISTRM)=NWRITES(I,ISTRM)+1
            CALL OUTUTAG(I,NWRITES(I,ISTRM))
            CALL WREVNT(LUN(I,ISTRM))
            IZBRECORD(I,ISTRM)=IQUEST(5)
            IZBBYTE  (I,ISTRM)=IQUEST(6)
          ENDIF
        ELSE
          IF(DOIT(I)) THEN
            NWRITES(I,ISTRM)=NWRITES(I,ISTRM)+1
            CALL OUTUTAG(I,NWRITES(I,ISTRM))
            CALL WREVNT(LUN(I,ISTRM))
            IZBRECORD(I,ISTRM)=IQUEST(5)
            IZBBYTE  (I,ISTRM)=IQUEST(6)
          ENDIF
        ENDIF
      ENDDO
      GOTO 999   ! exit
C
C
      ENTRY EV_STREAMS_OFF
      DO I=1,NSTREAMS
        DOIT(I)=.FALSE.
      ENDDO
      GOTO 999   ! exit
C
C
      ENTRY EVSET_STREAM(IPICK) 
      DOIT(IPICK)=.TRUE.
      GOTO 999   ! exit
C
C
      ENTRY EVGET_STREAM(STREAM,IPICK)
      NSTREAMS=NSTREAMS+1
      IF(NSTREAMS.GT.MAX_STREAMS) THEN
        CALL ERRMSG('Exceeded number of allowed streams','EVGET_STREAM',
     &    ' must abort','F')
      ENDIF
      IPICK=NSTREAMS
      STREAMS(NSTREAMS)=STREAM
      IF(STREAM(4:5).EQ.'ZZ') ISPECIAL=IPICK
      DOIT(NSTREAMS)=.FALSE.
      NWRITES(NSTREAMS,1)=0
      NWRITES(NSTREAMS,2)=0
      CALL EVTWOS_MULT(.TRUE.)
      GOTO 999   ! exit
C
C
      ENTRY EVOP_MULT(ISTRM,ONAME,XOPT,OK)
      DO I=1,NSTREAMS
        IF(LUN(I,ISTRM).EQ.0) THEN          ! first call to get unit number
          CALL GTUNIT(88,LUN(I,ISTRM),IER)
          L=TRULEN(ONAME)
          ILEN=TRULEN(STREAMS(I))
          NEW_NAME=ONAME(1:L)//'_'//STREAMS(I)(1:ILEN)
          L=TRULEN(NEW_NAME)
          CHOPT='OU'
          IF(XOPT(1:1).EQ.'X') CHOPT='XO'
          IF(XOPT(1:1).EQ.'G') CHOPT='GO'
          IF(XOPT(1:1).EQ.'Z') CHOPT='ZO'
c
c Make STA's with the "S" option
c
c	  IF(ISTRM.EQ.1)CHOPT=CHOPT//'S'
	  IF((ISTRM.EQ.1).AND.(ISPECIAL.NE.I))CHOPT=CHOPT//'S'
          CALL D0OPEN(LUN(I,ISTRM),NEW_NAME(1:L),CHOPT,OK)
          CALL XZRECL(ILEN,CHOPT)
          IF (ILEN .GE. 0) CALL FZFILE(LUN(I,ISTRM),ILEN,CHOPT)
        ENDIF
      ENDDO
      GOTO 999   ! exit
C
C
      ENTRY EVCL_MULT(ISTRM)
      DO I=1,NSTREAMS
        CALL FZENDO(LUN(I,ISTRM),'T')
        CALL D0CLOSE(LUN(I,ISTRM), ' ', CLOSE_OK)
        CALL RLUNIT(88,LUN(I,ISTRM),IER)
      ENDDO
      GOTO 999   ! exit
C
C
      ENTRY EVGIVE_MULT(NUM_STR,STR_NAME,NEVT_STR,STR_TYPE)
      NUM_STR=NSTREAMS
      DO I=1,NSTREAMS
        STR_NAME(I)=STREAMS(I)(1:3)
        NEVT_STR(I)=NWRITES(I,1)
        STR_TYPE(I)='STA'
        STR_NAME(I+NSTREAMS)=STREAMS(I)(1:3)
        NEVT_STR(I+NSTREAMS)=NWRITES(I,2)
        STR_TYPE(I+NSTREAMS)='DST'
      ENDDO
      GOTO 999   ! exit
C
C
      ENTRY EVSUM_MULT(LOUT,ISTRM)
      DO I=1,NSTREAMS
        WRITE(LOUT,FMT=101) NWRITES(I,ISTRM),STREAMS(I)
      ENDDO
  101 FORMAT(10X,I5,' written to stream ',A5)
      GOTO 999 ! EXIT
      ENTRY EVWR_MULT_ZBPOS(ISTR,ITYP,IRECORD,IBYTE)
C     ISTR is the stream number, ITYP is the type, 1=STA, 2=DST
      IRECORD =IZBRECORD(ISTR,ITYP)
      IBYTE=IZBBYTE  (ISTR,ITYP)
  999 RETURN
      END

      SUBROUTINE HIST_TRIG_BOOK2
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  29-MAY-1992   K. Wyatt Merritt
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NMAX
      PARAMETER (NMAX = 20)
      INTEGER NSTREAM
      INTEGER K,ID,STATUS,ISTART
C
      CHARACTER*5 STREAM_NAME(NMAX)
      CHARACTER*50 TITLE
C----------------------------------------------------------------------
      CALL HCDIR('//PAWC',' ')
      CALL HMDIR('TRIGBITS','S')
C
      CALL EZGET('NSTREAM',NSTREAM,STATUS)
C
      DO K = 1,NSTREAM
        ISTART = (K-1)*6 + 1
        CALL EZGETC('STREAM_DESCRIP',ISTART,5,STREAM_NAME(K),STATUS)
        ID = K*100
        WRITE (TITLE,*) STREAM_NAME(K),' EVTS PASS L1 - L1 BITS'
        CALL HBOOK1(ID,TITLE,33,-0.5,32.5,0.)
        WRITE (TITLE,*) STREAM_NAME(K),' EVTS FAIL L1 - L1 BITS'
        CALL HBOOK1(ID+1,TITLE,33,-0.5,32.5,0.)
        WRITE (TITLE,*) STREAM_NAME(K),' EVTS PASS USER - L1 BITS'
        CALL HBOOK1(ID+4,TITLE,33,-0.5,32.5,0.)
        WRITE (TITLE,*) STREAM_NAME(K),' EVTS FAIL USER - L1 BITS'
        CALL HBOOK1(ID+5,TITLE,33,-0.5,32.5,0.)
        WRITE (TITLE,*) STREAM_NAME(K),' EVTS PASS STRIP - L1 BITS'
        CALL HBOOK1(ID+8,TITLE,33,-0.5,32.5,0.)
        WRITE (TITLE,*) STREAM_NAME(K),' EVTS FAIL STRIP - L1 BITS'
        CALL HBOOK1(ID+9,TITLE,33,-0.5,32.5,0.)
C
        WRITE (TITLE,*) STREAM_NAME(K),' EVTS PASS L2 - L2 BITS'
        CALL HBOOK1(ID+2,TITLE,129,-0.5,128.5,0.)
        WRITE (TITLE,*) STREAM_NAME(K),' EVTS FAIL L2 - L2 BITS'
        CALL HBOOK1(ID+3,TITLE,129,-0.5,128.5,0.)
        WRITE (TITLE,*) STREAM_NAME(K),' EVTS PASS USER - L2 BITS'
        CALL HBOOK1(ID+6,TITLE,129,-0.5,128.5,0.)
        WRITE (TITLE,*) STREAM_NAME(K),' EVTS FAIL USER - L2 BITS'
        CALL HBOOK1(ID+7,TITLE,129,-0.5,128.5,0.)
        WRITE (TITLE,*) STREAM_NAME(K),' EVTS PASS STRIP - L2 BITS'
        CALL HBOOK1(ID+10,TITLE,129,-0.5,128.5,0.)
        WRITE (TITLE,*) STREAM_NAME(K),' EVTS FAIL STRIP - L2 BITS'
        CALL HBOOK1(ID+11,TITLE,129,-0.5,128.5,0.)
      ENDDO
C
      CALL HCDIR('//PAWC',' ')
  999 RETURN
      END

      SUBROUTINE GET_TAGS(TAGS,NDIM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : unpack tags from uDST
C-
C-   Outputs : TAGS - array with tags
C-             NDIM - dimension of TAGS array
C-
C-   Created  16-OCT-1992   Ulrich Heintz
C-   Updated  18-MAR-2004   sss - use fdate instead of date.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:UDST_DIMENSIONS.PARAMS'
      INTEGER I,NDIM,NDIMX,IDMAX,J,INDX,N,K,IER,NMAX,MAXGRP(IGRP)
      INTEGER NDIMG(IGRP),NDIMG1(IGRP),NWORD(IGRP),MAP(IGRP,NGRP),NGROUP
      INTEGER ICHECK,LTAGS,IOFF,XPTR(IGRP),GET_INDEX,JOFF
      PARAMETER( NMAX=100 )
      REAL    X(NDIMX),XDATA(NGRP)
      CHARACTER*4 XGRP(IGRP),XGRP1(IGRP)
      CHARACTER*8 XTAGS(NGRP,IGRP),XTAGS1(NGRP,IGRP),TAGS(NDIM),DATUM*9
      CHARACTER*8 SPLIT_TAGS(NMAX),XCHR(NGRP)
      INTEGER*2 ISPLIT(2)
      REAL    RSPLIT
      EQUIVALENCE(RSPLIT,ISPLIT)
      LOGICAL QOUT,OK,QFIRST
C----------------------------------------------------------------------
      CALL READ_UTAG_BANK(IDMAX,NDIMG1,XTAGS1,XGRP1,IGRP,NGRP)
      CALL UDST_HEAD_TAGS(IDMAX,NDIMG1,XTAGS1,XGRP1,IGRP,NGRP)
C
      CALL EZPICK('MAKE_NT_RCP')
      CALL EZ_GET_CHARS('SPLIT_BIT_PATTERNS',N,SPLIT_TAGS,IER)
      IF(N.GT.NMAX)CALL ERRMSG('MAKE_NT_RCP','GET_TAGS',
     &  'too many tags to split','F')
      CALL EZRSET
      CALL EZPICK('TAGS_RCP')
      CALL EZGET_l('OUTPUT_TAGS',QOUT,IER)
      IF(QOUT)THEN
        CALL GTUNIT(9191,LTAGS,IER)
        CALL D0OPEN(LTAGS,'TAGS.RCP','O',OK)
        WRITE(LTAGS,4)'\START    TAGS_RCP'
        WRITE(LTAGS,4)'\SIZE        614        37'
        WRITE(LTAGS,1)
    1   FORMAT('!',70('-'))
        WRITE(LTAGS,4)'!    Name:       TAGS.RCP'
        CALL fDATE(DATUM)
        WRITE(LTAGS,4)'!    Created:    '//DATUM//'   MAKE_NT'
        WRITE(LTAGS,1)
        WRITE(LTAGS,4)'OUTPUT_TAGS    .false.'
        WRITE(LTAGS,4)'\ARRAY GROUPS'
        WRITE(LTAGS,2)(''''//XGRP1(I)//'''',I=1,IDMAX)
    2   FORMAT(11(1X,A6))
        WRITE(LTAGS,4)'\END'
        DO I=1,IDMAX
          WRITE(LTAGS,5)'N_'//XGRP1(I),1
    5     FORMAT(A6,I10)
          WRITE(LTAGS,4)'\ARRAY '//XGRP1(I)
          WRITE(LTAGS,3)(''''//XTAGS1(J,I)//'''',J=1,NDIMG1(I))
    3     FORMAT(7(1X,A10))
          WRITE(LTAGS,4)'\END'
        ENDDO
        WRITE(LTAGS,4)'\STOP    ! TAGS_RCP'
    4   FORMAT(A)
        CALL D0CLOSE(LTAGS,' ',OK)
        STOP 'TAGS_RCP written to TAGS.RCP'
      ENDIF
C
      CALL EZ_GET_CHARS('GROUPS',NGROUP,XGRP,IER)
      IF(NGROUP.GT.IGRP)THEN
        CALL ERRMSG('TAGS_RCP','GET_TAGS','too many groups','F')
      ENDIF
      DO I=1,IGRP
        NDIMG(I)=0  ! initialize
      ENDDO
      DO J=1,NGROUP
        K=0
        DO I=1,IDMAX
          IF(XGRP1(I).EQ.XGRP(J))K=I
        ENDDO
        IF(K.EQ.0)THEN
          CALL ERRMSG('TAGS_RCP','GET_TAGS',
     &      'group not found: '//XGRP(J),'W')
        ELSE
          IF(IER.EQ.0)CALL EZGET_i('N_'//XGRP(J),MAXGRP(K),IER)
          IF(IER.EQ.0)CALL EZ_GET_CHARS(XGRP(J),NDIMG(K),XCHR,IER)
          IF(NDIMG(K).GT.NGRP)THEN
            CALL ERRMSG('TAGS_RCP','GET_TAGS','too many tags','F')
          ENDIF
          IOFF=0
          DO I=1,NDIMG(K)
            IF(N.GT.0)THEN
              ICHECK=GET_INDEX(N,SPLIT_TAGS,XCHR(I))
              IF(ICHECK.NE.0)THEN
                IF(I+IOFF.GT.NGRP)CALL ERRMSG('TAGS_RCP','GET_TAGS',
     &            'too many tags after splitting','F')
                XTAGS(I+IOFF,K)=XCHR(I)
                IOFF=IOFF+1
              ENDIF
              IF(I+IOFF.GT.NGRP)CALL ERRMSG('TAGS_RCP','GET_TAGS',
     &          'too many tags after splitting','F')
            ENDIF
            XTAGS(I+IOFF,K)=XCHR(I)
          ENDDO
          NDIMG(K)=NDIMG(K)+IOFF
        ENDIF
      ENDDO
      IF(IER.NE.0)CALL ERRMSG('TAGS_RCP','GET_TAGS',
     &  'error getting RCP parameters','F')
      CALL EZRSET
C
      QFIRST=.TRUE.
      DO I=1,IDMAX
        DO J=1,NDIMG(I)
          MAP(I,J)=GET_INDEX(NDIMG1(I),XTAGS1(1,I),XTAGS(J,I))
          IF(MAP(I,J).LE.0)THEN
            CALL ERRMSG('TAGS_RCP','GET_TAGS',
     &        'tag not found: '//XTAGS(J,I),'W')
            MAP(I,J)=0
            XTAGS(J,I)='no_tag'
          ENDIF
          IF(N.GT.0)THEN
            ICHECK=GET_INDEX(N,SPLIT_TAGS,XTAGS(J,I))
            IF(ICHECK.NE.0)THEN
              IF(QFIRST)THEN
                INDX=INDEX(XTAGS(J,I),' ')
                IF(INDX.GT.7.OR.INDX.EQ.0)INDX=7
   14           FORMAT(A,A1)
                WRITE(XTAGS(J,I),14)XTAGS(J,I)(:INDX-1),'_'
                QFIRST=.FALSE.
              ELSE
                MAP(I,J)=-MAP(I,J)
                QFIRST=.TRUE.
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      INDX=0
      DO I=1,IDMAX
        DO J=1,MAXGRP(I)
          DO N=1,NDIMG(I)
            IF(INDX+N.GT.NDIM)THEN
              CALL ERRMSG('too many names','GET_TAGS','truncating','W')
              GOTO 997
            ENDIF
            IF(XGRP1(I).NE.'HEAD'.AND.XGRP1(I).NE.'GLOB')THEN
              K=INDEX(XTAGS(N,I),' ')
              IF(J .LT. 10)THEN
                IF(K.GT.8.OR.K.EQ.0)K=8
                WRITE(TAGS(INDX+N),13)XTAGS(N,I)(:K-1),J
   13           FORMAT(A,I1)
              ELSE
                IF(K.GT.7.OR.K.EQ.0)K=7
                WRITE(TAGS(INDX+N),15)XTAGS(N,I)(:K-1),J
   15           FORMAT(A,I2)
              ENDIF
            ELSE
              TAGS(INDX+N)=XTAGS(N,I)
            ENDIF
          ENDDO
          INDX=INDX+NDIMG(I)
        ENDDO
      ENDDO
      NDIM=INDX
C----------------------------------------------------------------------
  997 RETURN
C
      ENTRY GET_EVENT(X,NDIMX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill an object group
C-
C-   Outputs : X   - data array for ntuple
C-             NDIMX - dimension of X array
C-
C-   Created  16-OCT-1992   Ulrich Heintz
C-   Updated  25-FEB-1994   Ulrich Heintz  move call to UDST_GET_HEAD out of 
C-                                          READ_UDST_BANK
C-
C----------------------------------------------------------------------
      CALL VZERO(XDATA,NGRP)
      NWORD(1)=0
      JOFF=0
      CALL UDST_GET_HEAD(NWORD,XDATA,IGRP,NGRP)
      IF(NWORD(1).NE.0)JOFF=1
      CALL READ_UDST_BANK(NWORD(1+JOFF),XDATA,XPTR(1+JOFF),IGRP-JOFF,
     &  NGRP-JOFF)
C
C      CALL VZERO(X,NDIM)
      INDX=0
      DO I=1,IDMAX
        DO J=1,MIN(MAXGRP(I),NWORD(I)/NDIMG1(I))
          DO N=1,NDIMG(I)
            IF(INDX+N.GT.512)GOTO 999
            IF(MAP(I,N).GT.0)THEN
              IF(I.EQ.1)X(INDX+N)=XDATA((J-1)*NDIMG1(I)+MAP(I,N))
              IF(I.GT.1)X(INDX+N)=Q(XPTR(I)+(J-1)*NDIMG1(I)+MAP(I,N))
            ELSEIF(MAP(I,N).EQ.0)THEN
              X(INDX+N)=0.
            ELSE
              IF(I.EQ.1)RSPLIT=XDATA((J-1)*NDIMG1(I)+MAP(I,N-1))
              IF(I.GT.1)RSPLIT=Q(XPTR(I)+(J-1)*NDIMG1(I)+MAP(I,N-1))
              X(INDX+N-1)=ISPLIT(1)
              X(INDX+N)=ISPLIT(2)
            ENDIF
          ENDDO
          INDX=INDX+NDIMG(I)
        ENDDO
        DO J=J,MAXGRP(I)
          DO N=1,NDIMG(I)
            IF(INDX+N.GT.512)GOTO 999
            X(INDX+N)=0.
          ENDDO
          INDX=INDX+NDIMG(I)
        ENDDO
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
      END

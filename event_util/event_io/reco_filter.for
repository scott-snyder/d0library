      FUNCTION RECO_FILTER()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      select events using RECO bits information
C-      controlled by RECO_FILTER.RCP, see default in D0$GENERAL
C-
C-   ENTRY RECO_FILTER_INI
C-     initialize and read in RCP file
C-     
C-   Created  14-MAY-1992   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL RECO_FILTER,RECO_FILTER_INI
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER FILT_INFO(4,100),NOBJ_STREAM(20),I,K,N,L
      INTEGER NSTREAMS,NPREV,NTOT,J,J1,J2,IER,IPASS,IDSEL
      INTEGER NOBJ(10),ID_OBJ(40),ISTRM(20),IOS
      REAL    ET(40),ETSEL
      LOGICAL BITON(40),USED(100)
      CHARACTER*5 STRMS(20)
      LOGICAL FIRST
      SAVE FILT_INFO,NOBJ_STREAM,N,NSTREAMS,NPREV,FIRST,ISTRM
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      RECO_FILTER=.TRUE.
      IF(N.EQ.0.OR.LHEAD.EQ.0) GOTO 999                ! no filtering
C
      IOS=MOD(IQ(LHEAD+1),1000)
      IF(IOS.LT.5) GOTO 999     ! not an event record
C
      RECO_FILTER=.FALSE.
      CALL UNPACK_RECO_BITS(NOBJ,ID_OBJ,ET,BITON)
      NTOT=0
      DO I=1,10
        NTOT=NTOT+NOBJ(I)
      ENDDO
      IF(NTOT.EQ.0) GOTO 999               ! no RECO objects in event
C
C              loop over all streams
      J1=1
      DO I=1,NSTREAMS
        J2=J1+NOBJ_STREAM(I)-1
        IPASS=0
        DO J=J1,J2
          USED(J)=.FALSE.
        ENDDO
C
        DO 10 K=1,NTOT
C
          DO J=J1,J2
            IDSEL=FILT_INFO(2,J)
            ETSEL=FILT_INFO(3,J)
C
            IF(IDSEL.EQ.ID_OBJ(K).AND..NOT.USED(J)) THEN  ! object to try
C
              IF(ABS(ET(K)).GE.ETSEL)  THEN ! it passes ET cut
C
                IF (FILT_INFO(4,J).EQ.0  ) THEN 
                  IPASS=IPASS+1
                  USED(J)=.TRUE.
                  GOTO 10               ! found match
                ELSE               
                  IF(BITON(J)) THEN     ! level 2 match required
                    IPASS=IPASS+1
                    USED(J)=.TRUE.
                    GOTO 10             ! found match
                  ENDIF
                ENDIF
C
              ENDIF
C
            ENDIF
C
          ENDDO
C
   10   CONTINUE
C
        J1=J2+1
        IF(IPASS.EQ.NOBJ_STREAM(I)) THEN  ! found all required objects
          RECO_FILTER=.TRUE.
          CALL EVSET_STREAM(ISTRM(I))     ! turn on stream
        ENDIF
      ENDDO
C
      GOTO 999        ! done
C
C
      ENTRY RECO_FILTER_INI()
      RECO_FILTER_INI=.TRUE.
      IF ( FIRST ) THEN
        FIRST=.FALSE.
        CALL EVTIN_HEADER(.TRUE.)     ! tell EVTIN to read header first
C
        CALL INRCP('RECO_FILTER_RCP',IER)
        IF(IER.NE.0) 
     &      CALL ERRMSG('Need RECO_FILTER.RCP','RECO_FILTER_INI',
     &      ' ','F')
C
        CALL EZPICK('RECO_FILTER_RCP')
        CALL EZGETA('FILTER_OBJECTS',0,0,0,N,IER)
        IF(N.LE.0) THEN
          CALL ERRMSG(' No objects given','RECO_FILTER',
     &      'Filter package will not filter ','W')
        ELSE
          CALL EZGETA('FILTER_OBJECTS',1,N,1,FILT_INFO,IER)
          N=N/4
          DO I=1,20
            NOBJ_STREAM(I)=0
          ENDDO
          NSTREAMS=0
          NPREV=0
          DO K=1,N
            I=FILT_INFO(1,K)
            NOBJ_STREAM(I)=NOBJ_STREAM(I)+1
            IF ( I.NE.NPREV ) THEN
              NPREV=I
              NSTREAMS=NSTREAMS+1
              CALL EZGETS('STREAM_NAMES',I,STRMS(I),L,IER) 
              IF(IER.NE.0) 
     &          CALL ERRMSG(' Stream names missing','RECO_FILTER',
     &          ' ','F')
              CALL EVGET_STREAM(STRMS(I),ISTRM(I))
            ENDIF
          ENDDO
        ENDIF
C          tell utility to expect multiple streams
        IF(NSTREAMS.GT.0) CALL EVTWOS_MULT(.TRUE.)    
        CALL EZRSET
      ENDIF
C
  999 RETURN
      END

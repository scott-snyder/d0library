      LOGICAL FUNCTION UDST_TO_DST_INIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : process UTAG bank whenever new begin-run-record
C-                         is found.
C-
C-   Returned value  : .true.
C-   Inputs  : UTAG bank
C-   Outputs : COMMON block UDST_TO_DST
C-   Controls: TAGS_MAP.RCP
C-
C-   Created  14-DEC-1993   Sailesh Chopra
C-   Updated  24-JAN-1994   Ulrich Heintz  convert to logical function to be
C-                            called from SETUP_RUN hook
C-   Updated  31-MAR-1994   Ulrich Heintz  changed "not in UTAG/TAGS_MAP.."
C-                            error messages from "W" to "I" type  messages
C-   Updated  16-MAY-1994   Ian Adam  Do UDST/UTAG version comparison and fill
C-                            UTAG if not there using UDST version
C-   Updated   8-AUG-1994   Ian Adam  bug fix for handling multiple versions
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZUTAG.LINK'
      INCLUDE 'D0$LINKS:IZANLS.LINK'
      INCLUDE 'D0$INC:UTAG_DATA.INC'
      CHARACTER*4 XGRP(MAX_GRP)
      INTEGER I,IER,K,J,GET_INDEX,IOFF,LENOCC,LEN_TAG,ILEN,NGRP,LUTAG
      INTEGER LANLS,NS,LANLSR
      CHARACTER*8 XTAGS1(MAX_WORDS,MAX_GRP)
      CHARACTER*13 MAP_NAME,SSTR
      LOGICAL FIRST,NEW_UTAG
      DATA    FIRST/.TRUE./,NEW_UTAG/.FALSE./
      INTEGER LUDST,GZUDST,UDST_VERSION,UTAG_VERSION
C----------------------------------------------------------------------
      UDST_TO_DST_INIT=.true.
C
C... determine whether UTAG bank is present
C
      IF(LHEADR .GT. 0)THEN
        LANLS=LQ(LHEADR-11)
        IF(LANLS.GT.0)THEN
          LUTAG=LQ(LANLS-IZUTAG)
        ELSE
          LUTAG=0
        ENDIF
      ENDIF

C... determine UDST bank version and compare to UTAG version.  If no UTAG,
C... fill it from an include file based on UDST version, and move it to the
C... run division

      NEW_UTAG = .FALSE.

      LUDST=GZUDST()
      IF (LUDST.GT.0) THEN
        UDST_VERSION = IQ(LUDST+1)
        IF (LUTAG.GT.0) THEN
          UTAG_VERSION = IQ(LUTAG+1)
          IF ( UTAG_VERSION.NE.UDST_VERSION ) THEN
            CALL ERRMSG('rebuilding UTAG','UDST_TO_DST_INIT'
     &        ,'UDST/UTAG version mismatch','W')
            CALL MZDROP(IXCOM,LUTAG,'L')
            CALL UTAG_FROM_UDST(UDST_VERSION,LUTAG)
            NEW_UTAG = .TRUE.
          ENDIF
        ELSE
          CALL ERRMSG('rebuilding UTAG','UDST_TO_DST_INIT'
     &      ,'no UTAG bank found','W')
          CALL UTAG_FROM_UDST(UDST_VERSION,LUTAG)
          NEW_UTAG = .TRUE.
        ENDIF

        IF (LHEADR.LE.0) THEN
          NEW_UTAG = .FALSE.
          CALL MZCOPY(IXMAIN,LHEAD,IXDVR,LHEADR,1,' ')
          NS=IQ(LHEADR-2)             ! drop everything that hangs from HEAD
          DO I=1,NS                   ! except ANLS
            IF(I.NE.IZANLS.AND.LQ(LHEADR-I).GT.0)THEN
              CALL MZDROP(IXCOM,LQ(LHEADR-I),'L')
            ENDIF
          ENDDO
          LANLSR=LQ(LHEADR-IZANLS)    ! drop everything that hangs from ANLS
          NS=IQ(LANLSR-2)             ! except UTAG
          DO I=1,NS
            IF(I.NE.IZUTAG.AND.LQ(LANLSR-I).GT.0)THEN
              CALL MZDROP(IXCOM,LQ(LANLSR-I),'L')
            ENDIF
          ENDDO
        ELSE IF (NEW_UTAG) THEN
          NEW_UTAG = .FALSE.
          LANLSR=LQ(LHEADR-IZANLS)
          IF (LQ(LANLSR-IZUTAG).GT.0) CALL MZDROP(IXCOM,
     &      LQ(LANLSR-IZUTAG),'L')
          CALL MZCOPY(IXMAIN,LUTAG,IXDVR,LANLSR,-IZUTAG,' ')
        ENDIF

      ELSE
        CALL ERRMSG('NO UDST','UDST_TO_DST_INIT',' ','I')
      ENDIF

      IF(LHEADR.LE.0.OR.LUTAG.LE.0)THEN
        UDST_TO_DST_INIT=.FALSE.
        GOTO 999
      ENDIF
C
      IF(FIRST)THEN
        FIRST=.FALSE.
        CALL INRCP('FTRAKS_RCP',IER)
        IF(IER.NE.0)CALL ERRMSG('FTRAKS_RCP','UDST_TO_DST_INIT',
     &    'not found','F')
        CALL INRCP('TAGS_MAP_RCP',IER)
        IF(IER.NE.0)CALL ERRMSG('TAGS_MAP_RCP','UDST_TO_DST_INIT',
     &    'not found','F')
        CALL EZPICK('TAGS_MAP_RCP')
        CALL EZ_GET_CHARS('GROUPS',NGRP,XGRP,IER)
        CALL EZRSET
        IF(NGRP .GT. MAX_GRP)CALL ERRMSG('NGRP>MAX_GRP',
     &    'UDST_TO_DST_INIT', 'too many groups','F')
      ENDIF
C
      CALL READ_UTAG_BANK(IDMAX,NDIMG1,XTAGS1,XGRP1,MAX_GRP,
     &  MAX_WORDS)
      DO I = 1,NGRP
        K=0
        DO J = 1,IDMAX
          IF(XGRP(I) .EQ. XGRP1(J))THEN
            K=J
            GOTO 20
          ENDIF
        ENDDO
   20   CONTINUE
        IF (K .EQ. 0)THEN
          CALL ERRMSG('group '//XGRP(I)//' not in UTAG bank',
     &      'UDST_TO_DST_INIT',' ','W')
        ELSE
          CALL EZPICK('TAGS_MAP_RCP')
          CALL EZ_GET_CHARS(XGRP(I),NTAGS_GRP(K),TAGS_MAP(1,1,K)
     &      ,IER)
          CALL EZRSET
          IF(IER .NE.0) CALL ERRMSG('TAGS_MAP_RCP','UDST_TO_DST_INIT',
     &      'error reading RCP' ,'F')
          NTAGS_GRP(K) = NTAGS_GRP(K)/2
          IF(NTAGS_GRP(K).GT.MAX_WORDS)CALL ERRMSG(
     &      'NTAGS_GRP>MAX_WORDS','UDST_TO_DST_INIT',' ','F')
        ENDIF
      ENDDO
      DO I = 1,IDMAX
        DO J = 1,NDIMG1(I)
          K=1
          DO WHILE (K .LE. NTAGS_GRP(I))
            IF ( XTAGS1(J,I) .EQ. TAGS_MAP(1,K,I))THEN
              GOTO 10
            ENDIF
            K=K+1
          ENDDO
          CALL ERRMSG(XTAGS1(J,I)//' not in TAGS_MAP_RCP',
     &        'UDST_TO_DST_INIT',' ','I')
   10     CONTINUE
        ENDDO
      ENDDO
C-
      DO I = 1,IDMAX
        DO J=1,NTAGS_GRP(I)
          MAP(J,I)=GET_INDEX(NDIMG1(I),XTAGS1(1,I),TAGS_MAP(1,J,I))
          IF(MAP(J,I) .LE. 0)THEN
            CALL ERRMSG(TAGS_MAP(1,J,I)(1:8)//' not in UTAG bank',
     &        'UDST_TO_DST_INIT',' ','I')
            MAP(J,I)=0
            TAGS_MAP(2,J,I)='NULL'        !PUT MAP = 'NULL'
          ENDIF
          MAP_NAME = TAGS_MAP(2,J,I)
          LEN_TAG = LENOCC(TAGS_MAP(1,J,I))
          IF(TAGS_MAP(1,J,I)(LEN_TAG:LEN_TAG) .EQ. '_')THEN
            IOR(J,I) = 'B'
          ELSEIF(MAP_NAME(1:1) .EQ. 'I')THEN
            IOR(J,I) = 'I'
          ELSE
            IOR(J,I) = 'R'
          ENDIF
          IF (MAP_NAME(1:4) .EQ. 'NULL')THEN
            BANK_NAME(J,I) = 'NULL'
            IELE(J,I) = 0
          ELSEIF(MAP_NAME(1:4) .EQ. 'LINK')THEN
            LINK_BANK(J,I)=MAP_NAME(7:10)
            BANK_NAME(J,I) = 'LINK'
            IELE(J,I) = 0
          ELSEIF(MAP_NAME(1:4) .EQ. '----')THEN
            BANK_NAME(J,I)='NULL'
            IELE(J,I)=0
          ELSE
            BANK_NAME(J,I) =
     &        MAP_NAME(INDEX(MAP_NAME,'(')+2:INDEX(MAP_NAME,'+')-1)
            SSTR = MAP_NAME(INDEX(MAP_NAME,'+')+1:INDEX(MAP_NAME,')')-1)
            ILEN=INDEX(MAP_NAME,')')-INDEX(MAP_NAME,'+')-1
            IF(ILEN.EQ.1)THEN
              READ(SSTR,'(I1)')IELE(J,I)
            ELSEIF(ILEN.EQ.2)THEN
              READ(SSTR,'(I2)')IELE(J,I)
            ELSE
              READ(SSTR,'(I3)')IELE(J,I)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C-
  999 RETURN
      END

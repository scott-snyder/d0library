      LOGICAL FUNCTION UDST_TO_DST()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : expand event from UDST to DST ZEBRA tree
C-
C-   Created  13-DEC-1993   Sailesh Chopra
C-   Updated  24-JAN-1994   Ulrich Heintz  check for UDST, banks below PARH
C-                                         delete call to UDST_TO_DST_INIT
C-   Updated   6-JUN-1994   Ian Adam  call UDST_TO_DST_INIT whenever UDST
C-                                    version change
C-   Updated   8-AUG-1994   Ian Adam  call LIB$FIXUP_FLT for VAX processing
C-                                    of UNIX reconstructed data
C-   Updated  10-OCT-1995   Ulrich Heintz add call to UINT_TO_DST
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:UTAG_DATA.INC'
      INTEGER I,J,K,LUDST,XPTR(MAX_GRP),GZPARH,GZUDST
      INTEGER UDST_VERSION,LAST_UDST_VERSION
      REAL    XDATA(MAX_WORDS)
      LOGICAL FIRST,STATUS,UDST_TO_DST_INIT,UNP_QCD_MAKE_DST,QUTAG
      LOGICAL FLGCHK,UINT_TO_DST
      DATA    FIRST/.TRUE./,LAST_UDST_VERSION/-999/
C&IF VAXVMS
      EXTERNAL LIB$FIXUP_FLT
C&ENDIF
C----------------------------------------------------------------------
      UDST_TO_DST=.TRUE.
C
C... determine whether UDST bank is present
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        IF (.NOT. FLGCHK('NEW_UTAG')) CALL FLGBK('NEW_UTAG',1)
C&IF VAXVMS
        CALL SYS$SETEXV(%VAL(1),LIB$FIXUP_FLT,,)
C&ENDIF
      ENDIF

      LUDST=GZUDST()
      IF(LUDST .LE. 0)THEN
        CALL ERRMSG('UDST bank not found','UDST_TO_DST',
     &      'will not expand UDST','W')
        GOTO 999
      ENDIF
C
C... determine whether there are any banks below PARH
C
      LPARH=GZPARH()
      IF(LPARH.GT.0)THEN
        DO I=1,7
          IF(LQ(LPARH-I).GT.0)THEN
            CALL ERRMSG('banks below PARH','UDST_TO_DST',
     &          'will not expand UDST','W')
            GOTO 999
          ENDIF
        ENDDO
      ENDIF
C

      UDST_VERSION = IQ(LUDST+1)

      IF ( UDST_VERSION .NE. LAST_UDST_VERSION ) THEN
        QUTAG = UDST_TO_DST_INIT()
        LAST_UDST_VERSION = UDST_VERSION
        CALL FLGSET('NEW_UTAG',.TRUE.)
      ELSE
        CALL FLGSET('NEW_UTAG',.FALSE.)
      ENDIF


C
C... determine whether UTAG bank has been read
C
      IF(.NOT.QUTAG)THEN
        CALL ERRMSG('UTAG bank not found','UDST_TO_DST',
     &      'will not expand UDST','W')
        GOTO 999
      ENDIF
C
C... read UDST bank and fill UDST_DATA array
C
      CALL READ_UDST_BANK(NWORD,XDATA,XPTR,MAX_GRP,MAX_WORDS)
      DO I = 1,IDMAX
        IF (NWORD(I) .GT. MAX_WORDS)CALL ERRMSG('DIMENSION',
     &      'UDST_TO_DST','too many objects','F')
        IF(NDIMG1(I).GT.0)THEN
          NOBJ(I) = NWORD(I)/NDIMG1(I)
        ELSE
          NOBJ(I) = 0
        ENDIF
        DO J = 1,NOBJ(I)
          DO K=1,NTAGS_GRP(I)
            IF (MAP(K,I).NE. 0)THEN
              UDST_DATA(K+(J-1)*NTAGS_GRP(I),I)=
     &            Q(XPTR(I)+(J-1)*NDIMG1(I)+MAP(K,I))
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      CALL UDST_TO_DST_FILL
C... reshuffle QCD banks
      STATUS = UNP_QCD_MAKE_DST()
C... decompress UINT bank to recreate ESUM
      STATUS = UINT_TO_DST()
C
  999 RETURN
      END

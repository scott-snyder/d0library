      SUBROUTINE ERRMZB( IDIN, SUBRIN, VARIN, SEV )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Hangs ERMG containing a list of error
C-                         IDSTRG's and number of occurences under HSTR
C-
C-   Inputs  : IDIN        ID character string
C-             SUBRIN      Subroutine string
C-             VARIN      Informational string
C-             SEV         Error severity
C-   Outputs : Creates new ERMG bank if first occourance of this error,
C-             otherwise increments error count in appropriate ERMG bank.
C-   Controls:
C-
C-   ENTRY POINTS
C-      ERRZON             Enable Error logging to Zebra banks
C-      ERRZOF             Disable Error logging to Zebra banks (Default)
C-      ERRZGT             Return current status of Zebra logging
C-      ERRZDP             write currently buffered messages
C-
C-   Created   8-APR-1992   Andrew J. Milder
C-   Updated  16-APR-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$ERRMSG_UTIL$PARAMS:ERRMSG.PARAMS'
      INCLUDE 'D0$INC:ERRZST.INC'
      INCLUDE 'D0$INC:ERRZST_C.INC'
      CHARACTER*(MXCHR_ERMG) BIG_STRING,IDSTR
      CHARACTER*1 SEV
      CHARACTER*32 IDIN,SUBRIN
      CHARACTER*132 VARIN,LINE1,LINE2
      INTEGER I
      INTEGER LERMG,GZERMG,LENCHR,BIGLEN,TRULEN,NFIX
      LOGICAL DONE,ZEB,ZEBLOG
      DATA ZEBLOG / .FALSE. /
C----------------------------------------------------------------------
      IF ( ZEBLOG ) THEN
        CALL ERRDSP(IDIN,SUBRIN,SEV,VARIN,LINE1,LINE2)
        BIG_STRING = LINE1(1:TRULEN(LINE1))
        IF (TRULEN(LINE2).GT.0) THEN
          BIG_STRING = LINE1(1:TRULEN(LINE1))//' '//
     &      LINE2(1:MIN(TRULEN(LINE2),(MXCHR_ERMG-TRULEN(LINE1)-1)))
        ENDIF

        BIGLEN = TRULEN(BIG_STRING)
        IF (BIGLEN .EQ. 0) BIGLEN = 1
        DONE = .FALSE.
C
C  Look through errors waiting to go in banks
C
        DO I = 1, TOTWAIT
          IF ( BIG_STRING(1:BIGLEN) .EQ. IDWAIT(I)(1:LENWAIT(I)) )
     &         THEN
            NUMWAIT(I) = NUMWAIT(I) + 1 !claim only one possible match
            DONE = .TRUE.
          ENDIF
        ENDDO
        IF (.NOT.DONE) THEN
C
C  Search through ERMG banks for this string
C
          LERMG = GZERMG()
C
          DO WHILE ( (LERMG .GT. 0) .AND. (.NOT.DONE) )
            LENCHR = MIN(IQ(LERMG + 5),MXCHR_ERMG)
            NFIX = IQ(LERMG + 2)
            CALL UHTOC(IQ(LERMG+NFIX+1),LENCHR,IDSTR,LENCHR)
            IF ( BIG_STRING(1:BIGLEN) .EQ. IDSTR(1:LENCHR)) THEN
C
C  Already exists, increment count
C
              IQ(LERMG+3) = IQ(LERMG+3) + 1
              DONE = .TRUE.
            ENDIF
            LERMG = LQ(LERMG) !chase linear chain
          ENDDO
        ENDIF
C
C  First time this error has been called, book it
C
        IF ( .NOT. DONE ) THEN
          CALL ERMGFL(BIG_STRING,BIGLEN,SEV)
        ENDIF
      ENDIF
      RETURN
C--------------
      ENTRY ERRZON
      ZEBLOG = .TRUE.
      RETURN
C
C--------------
      ENTRY ERRZOF
      ZEBLOG = .FALSE.
      RETURN
C
C--------------
      ENTRY ERRZGT(ZEB)
      ZEB = ZEBLOG
      RETURN
C
C--------------
      ENTRY ERRZDP

      CALL ERMGFL(BIG_STRING,0,SEV)
C
  999 RETURN
      END

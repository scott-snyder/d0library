      SUBROUTINE ASKHST (PRG,NHST,HIST,DCHN,DLOW,DHIH,
     &                            BOOK,NCHN,XLOW,XHIH,BACK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prompt user for histograms and settings
C-
C-   Inputs:   PRG         Program prompt
C-             NHST        Maximum number of histograms ( < = 100 )
C-             HIST (NHST) Histrogram names
C-             DCHN (NHST) Default number of channels
C-             DLOW (NHST) Default lower limit
C-             DHIH (NHST) Default upper limit
C-
C-   Outputs : BOOK (NHST) Histogram requests (boolean flags)
C-             NCHN (NHST) Number of channels/ histogram
C-             XLOW (NHST) Lower limit
C-             XHIH (NHST) Upper limit
C-             BACK        If true go back to upper menu level
C-
C-
C-   Created  11-MAR-1988   Harrison B. Prosper
C-   Modified  9-MAY-1988
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL       FIRST,OK,BACK,ACTIVE
      INTEGER       NHST,NMAX,I,J,N,NN,DCHN(*),NCHN(*)
      LOGICAL       BOOK(*)
      CHARACTER*(*) HIST(*)
      REAL          DLOW(*),DHIH(*),XLOW(*),XHIH(*),NUMR(2)
      PARAMETER(    NMAX = 100 )
      CHARACTER*1   ANSWER,LEFT,RIGHT
      CHARACTER*4   INUM
      CHARACTER*32  STRLIM(NMAX),STRCHN(NMAX),STR,CMD
      CHARACTER*80  SETSTR(NMAX)
C
      INTEGER       NTYPE,NT
      PARAMETER( NTYPE  =  3)
      REAL          NUMBER(NTYPE)
      INTEGER       TYPE(NTYPE)
C
      CHARACTER*(*) PRG,ERRMSG,PRTHIS,PRTNUM
      PARAMETER( ERRMSG = ' %ASKHST-ERROR-')
      PARAMETER( PRTHIS = ' Histogram IDs (EXIT to exit) > ')
      PARAMETER( PRTNUM = ' Give (Channels,LOW-edge,HI-edge) Hist-')
C
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
C
C ****  DEFINE DEFAULTS
C
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
        IF ( NHST .GT. NMAX ) THEN
          CALL INTMSG 
     &    (ERRMSG//'No more than 100 histogram names can be displayed')
          NN = NMAX
        ELSE
          NN = NHST
        ENDIF
C
        DO 15 I = 1,NN
          BOOK(I) = .FALSE.
C
C **** Default histogram limits
C
          XLOW(I) = DLOW(I)
          XHIH(I) = DHIH(I)
          NUMR(1) = XLOW(I)
          NUMR(2) = XHIH(I)
          CALL VNUMR (2,NUMR,'[',',',']',STRLIM(I),N)
C
C **** Default number of channels
C
          NCHN(I) = DCHN(I)
          CALL VNUMI (1,NCHN(I),'[',',',']',STRCHN(I),N)
   15   CONTINUE
        ENDIF
C
C ****  ASK FOR HISTOGRAM LIMITS AND NUMBER OF CHANNELS
C
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE ) 
C
C ****  Display current setup and ask for histogram list
C
        CALL ERASE
        SETSTR(1)=PRG//' BOOK HISTOGRAMS '
        CALL INTMSG (SETSTR(1))
        DO 10 I = 1,NN
          SETSTR(I) = HIST(I)(1:40)//' '//STRCHN(I)(1:8)//STRLIM(I)
          CALL INTSTR (I,'. ',INUM,N)
          SETSTR(I) = ' '//INUM//SETSTR(I)
          IF ( BOOK(I) ) THEN
            SETSTR(I) = SETSTR(I)(1:74)//' <--'
          ENDIF
   10   CONTINUE
        CALL TYPAGE (SETSTR,NN,BACK)
        IF ( BACK ) GOTO 999
C
        CALL ASKLST (PRTHIS,NN,BOOK,STR,CMD,BACK)
        IF ( BACK ) GOTO 999
        IF ( CMD(1:4) .EQ. 'NONE' ) GOTO 999
C
C ****  Ask for limits and channels
C
        NT = 0 ! To force defaults
        DO 20 I = 1,NN
        IF ( BOOK(I) ) THEN
          IF ( CMD(1:5) .EQ. 'ALL/D' ) THEN
            LEFT = '['
            RIGHT= ']'
          ELSE
            LEFT = '('
            RIGHT= ')'
            CALL INTSTR (I,'> ',INUM,N)
            CALL ASKNUM (PRTNUM//INUM,NT,NUMBER,TYPE,BACK)
            IF ( BACK ) GOTO 999
          ENDIF
C
C ****  Check values
C
          IF ( NT .LE. 0 ) THEN
            NCHN(I) = DCHN(I)
            XLOW(I) = DLOW(I)
            XHIH(I) = DHIH(I)
C
          ELSEIF ( NT .EQ. 1 ) THEN
            IF ( TYPE(1) .GT. 0 ) THEN ! Accept only VALID number types
              NCHN(I) = NUMBER(1)
            ELSE
              NCHN(I) = DCHN(I)
            ENDIF
            XLOW(I) = DLOW(I)
            XHIH(I) = DHIH(I)
C
          ELSEIF ( NT .EQ. 2 ) THEN
            IF ( TYPE(1) .GT. 0 ) THEN
              NCHN(I) = NUMBER(1)
            ELSE
              NCHN(I) = DCHN(I)
            ENDIF
            IF ( TYPE(2) .GT. 0 ) THEN
              XLOW(I) = NUMBER(2)
            ELSE
              XLOW(I) = DLOW(I)
            ENDIF
            XHIH(I) = DHIH(I)
C
          ELSEIF ( NT .EQ. 3 ) THEN
            IF ( TYPE(1) .GT. 0 ) THEN
              NCHN(I) = NUMBER(1)
            ELSE
              NCHN(I) = DCHN(I)
            ENDIF
            IF ( TYPE(2) .GT. 0 ) THEN
              XLOW(I) = NUMBER(2)
            ELSE
              XLOW(I) = DLOW(I)
            ENDIF
            IF ( TYPE(3) .GT. 0 ) THEN
              XHIH(I) = NUMBER(3)
            ELSE
              XHIH(I) = DHIH(I)
            ENDIF
          ENDIF
C
C ****  Check for crazy values
C
          IF ( NCHN(I) .LE. 0 ) THEN
            NCHN(I) = DCHN(I)
          ENDIF
          IF ( XLOW(I) .GE. XHIH(I) ) THEN
            XLOW(I) = DLOW(I)
            XHIH(I) = DHIH(I)
          ENDIF
C
          NUMR(1) = XLOW(I)
          NUMR(2) = XHIH(I)
          CALL VNUMR (2,NUMR,LEFT,',',RIGHT,STRLIM(I),N)
          CALL VNUMI (1,NCHN(I),LEFT,',',RIGHT,STRCHN(I),N)
        ENDIF
   20   CONTINUE
      ENDDO
  999 RETURN
      END

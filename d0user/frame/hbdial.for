      SUBROUTINE HBDIAL
C----------------------------------------------------------
C-                                                        -
C-      Simple minded dialog to book histograms           -
C-                                                        -
C-                SDP Jan.,1987                           -
C-                                                        -
C----------------------------------------------------------
      IMPLICIT NONE 
C
C         Variables from COMPACK
      INTEGER PFNUM
C
      INTEGER INXBIN,INYBIN,DEFID,J,NIDENT,I
      REAL XMIN,XMAX,YMIN,YMAX
      CHARACTER*32 INTIT
      CHARACTER*80 MSG
      CHARACTER*40 PROMPT(9),PROMP
      CHARACTER*1 TYPARR(9)
      DATA PROMP/' 1 or 2 >'/
      DATA PROMPT/' Enter no. of identical histograms >',
     *            ' Enter first histogram # >',
     *            ' Enter histograms title >',     
     *            ' Enter # of X bins >',
     *            ' Enter min. X-value >',
     *            ' Enter max. X-value >',
     *            ' Enter # of Y bins >',
     *            ' Enter min. Y-value >',     
     *            ' Enter max. Y-value >'/
      DATA TYPARR/'I','I','C','I','R','R','I','R','R'/
    1 MSG=' Choose 1D or 2D histograms. Any number other than 1 or 2'
      CALL OUTMSG(MSG)
      MSG=' will terminate dialog.'
      CALL OUTMSG(MSG)
      CALL GETPAR(1,PROMP,TYPARR,J)
C
      IF(PFNUM().EQ.4) RETURN
C
      MSG=' For more than 1 histogram with identical definitions'
      CALL OUTMSG(MSG)
      MSG=' histograms will be incremented by 1 starting from first.' 
      CALL OUTMSG(MSG)
C
      IF(J.EQ.1) THEN                      ! 1D Histograms
        CALL GETPAR(-6,PROMPT,TYPARR,NIDENT,
     $  DEFID,INTIT,INXBIN,XMIN,XMAX)
        IF(PFNUM().EQ.4) RETURN
        INTIT=INTIT(1:31)//'$'
        DO 11 I=1,NIDENT
   11   CALL HBOOK1(DEFID-1+I,%ref(INTIT),INXBIN,XMIN,XMAX)
        GOTO 1
C
      ELSE IF(J.EQ.2) THEN                ! 2D Histograms
        CALL GETPAR(9,PROMPT,TYPARR,NIDENT,  
     $  DEFID,INTIT,INXBIN,XMIN,XMAX,INYBIN,YMIN,YMAX)
        IF(PFNUM().EQ.4) RETURN   
        INTIT=INTIT(1:31)//'$'
        DO 12 I=1,NIDENT
   12   CALL HBOOK2(DEFID-1+I,%ref(INTIT),INXBIN,XMIN,XMAX,
     $    INYBIN,YMIN,YMAX)
        GOTO 1
      ENDIF
C
      RETURN
      END

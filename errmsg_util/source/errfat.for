      SUBROUTINE ERRFAT(IDSTRG, SUBRID, VARSTR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fatal Error Resovery Routine. Must be declared
C-                         in the calling routine. Routine to called if
C-                         a fatal error occurs.
C-
C-   Inputs  : IDSTRG      A character string identifies the message
C-             SUBRID      A character string identifies calling routine
C-             VARSTR      A character string provides additional
C-                         information
C-   Outputs : None
C-   Controls: None
C-
C-   Created  18-DEC-1988   Jun-Jang Jeng (MSU)
C-   Updated   3-JAN-1989   James T. Linnemann
C-   Updated  21-JUN-1989   Jason McCampbell (MSU)
C-   Updated  15-JUL-1990   James T. Linnemann   
C-   Updated   5-NOV-1991   Krzysztof L. Genser  
C       to handle FATMEN long generic names 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) IDSTRG, SUBRID, VARSTR
      INTEGER LENG
      PARAMETER( LENG = 132 )
      CHARACTER*(LENG) LINE1, LINE2
      INTEGER I,J,N
      INTEGER LUN
      INCLUDE 'D0$ERRMSG_UTIL$PARAMS:ERRMSG.PARAMS'
      INCLUDE 'D0$INC:ERRCTL.INC'
      INCLUDE 'D0$INC:ERRCNT.INC'
      INCLUDE 'D0$INC:ERRIDS.INC'
C----------------------------------------------------------------------
C
      CALL ERRDSP(IDSTRG, SUBRID, 'F', VARSTR, LINE1, LINE2)
      CALL SWORDS(LINE1, I,J,N)
C&IF VAXELN
C&ELSE
C
C...Force some logging of the fatal error
      IF ( LULOG.GT.0 ) THEN
        LUN = LULOG
      ELSE
        LUN = 6
      ENDIF
      WRITE(UNIT=LUN, FMT=100) LINE1(I:J)
  100 FORMAT (1X,A)
C&ENDIF
C
C
C...fatal error processing
      CALL D0_ABORT(' '//LINE1(I:J))
  999 RETURN
      END

      SUBROUTINE ERRHAN_E(IDSTRG, SUBRID, VARSTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Error Recovery Routine. Must be declared in
C-                         the calling routine. Routine to be called if
C-                         a severe error occurs.
C-
C-   Inputs:   IDSTRG      A character string identifies the message
C-             SUBRID      A character string identifies calling routine
C-             VARSTR      A character string provides additional 
C-                         information
C-   Outputs : None
C-
C-   Controls: None
C-
C-   Created  18-DEC-1988   Jun-Jang Jeng (MSU)
C-   Updated   5-NOV-1991   Krzysztof L. Genser  
C       to handle FATMEN long generic names 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) IDSTRG, SUBRID
      CHARACTER*(*) VARSTR
      INCLUDE 'D0$ERRMSG_UTIL$PARAMS:ERRMSG.PARAMS'
      INCLUDE 'D0$INC:ERRCTL.INC'
      INCLUDE 'D0$INC:ERRCNT.INC'
      INCLUDE 'D0$INC:ERRIDS.INC'
      CHARACTER*132 LINE1,LINE2
      INTEGER I,J,N
C----------------------------------------------------------------------
C
      CALL ERRDSP(IDSTRG, SUBRID, 'F', VARSTR, LINE1, LINE2)
      CALL SWORDS(LINE1, I,J,N)
C&IF VAXELN
C&      CALL D0_ABORT(' E-level error'//LINE1(I:J))
C&ELSE
C&ENDIF
 999  RETURN
      END


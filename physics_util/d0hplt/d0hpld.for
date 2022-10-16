      SUBROUTINE D0HPLD(COMMAND)
C=====================================================================
C
C  Description:  Simple interactive histogram plotting package
C  ============
C                Main driving subroutine
C
C  Input:
C  COMMAND = instruction to D0H package
C
C  Author:
C  ========
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - July 1,1988
C-   Updated  22-APR-1992   SoftExp  (Harrison B. Prosper)
C-    Tidy up 
C-   Updated   8-MAY-1992   Harrison B. Prosper   
C
C=======================================================================
C
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
C  Local Declarations:
C  ===================
C 
      CHARACTER*(*) COMMAND
      INTEGER HNUM,I
      INTEGER DEVICE
      CHARACTER*5 TERM
      INTEGER IDALL(200)
      INTEGER NALL
      INTEGER IFLAG
      INTEGER NXZON,NYZON
      LOGICAL FLGVAL
      LOGICAL FIRST,LINT,INTAST
      CHARACTER*80 HEADER
      DATA FIRST/.TRUE./
      DATA DEVICE/1/
      DATA HNUM/0/
C
C  Executable Code:
C  =================
C
      IF (COMMAND(1:4) .EQ. 'PLOT') THEN
         CALL GETPAR(1,' Enter HISTOGRAM NUMBER ','I',HNUM)
         CALL D0HINT
         CALL D0HPID(HNUM)
         CALL D0HCOM
      ELSE IF (COMMAND(1:4) .EQ. 'LEGO') THEN
         CALL GETPAR(1,' Enter HISTOGRAM NUMBER ','I',HNUM)
         CALL D0HINT
         CALL D0HLEGO(HNUM)
         CALL D0HCOM
      ELSE IF (COMMAND(1:10).EQ.'STOP_UPLOT')THEN
         IFLAG=-1
         CALL D0HUPH(IFLAG)
      ELSE IF (COMMAND(1:5) .EQ. 'UPLOT') THEN
C Check if are initializing or in AUTO continue mode
         IF(.NOT.FLGVAL('D0HAUT'))THEN
           IFLAG=0
           CALL D0HUPH(IFLAG)
         ELSE
           IFLAG=1
           CALL D0HUPH(IFLAG)
         ENDIF
      ELSE IF (COMMAND(1:4) .EQ. 'SHOW') THEN
         CALL GETPAR(1,' Enter HISTOGRAM NUMBER ','I',HNUM)
         CALL D0HSHW(HNUM)
C
      ELSE IF (COMMAND(1:5) .EQ. 'INDEX') THEN
         CALL D0HINDEX
C
C ****  Suppress refresh of histogram list
C
         CALL D0HINDEX_REFRESH(.FALSE.)

      ELSE IF (COMMAND(1:4) .EQ. 'LIST') THEN
         CALL D0HLDIR
C
      ELSE IF (COMMAND(1:16) .EQ. 'CHANGE DIRECTORY') THEN
         IF(FULSCR)THEN
           CALL D0HCDIR
         ELSE
           CALL D0HCHD
         ENDIF
C
      ELSE IF (COMMAND(1:4) .EQ. 'TYPE') THEN
         CALL GETPAR(1,' Enter HISTOGRAM NUMBER [0 for ALL] ','I',HNUM)
         CALL D0HTYP(HNUM)
C   
      ELSE IF (COMMAND(1:5) .EQ. 'NZONE') THEN
         CALL GETPAR(1,' NUMBER ZONES IN X ','I',NXZON)
         CALL GETPAR(1,' NUMBER ZONES IN Y ','I',NYZON)
         CALL D0HINT
         CALL HPLZON(NXZON,NYZON,1,' ')
C
      ELSE IF (COMMAND(1:5) .EQ. 'CLEAR') THEN
         CALL GETPAR(1,' Enter HISTOGRAM NUMBER [0 for ALL] ','I',HNUM)
         CALL D0HCLR(HNUM)
C
      ELSE IF (COMMAND(1:5) .EQ. 'PRINT') THEN
         CALL GETPAR(1,' Enter HISTOGRAM NUMBER [0 for ALL] ','I',HNUM)
         CALL D0HPRT(HNUM)
C
      ELSE IF (COMMAND(1:5) .EQ. 'STORE') THEN
         CALL GETPAR(1,' Enter HISTOGRAM NUMBER [0 for ALL] ','I',HNUM)
         CALL D0HSTR(HNUM)
C
      ELSE IF (COMMAND(1:5) .EQ. 'BPLOT') THEN
         CALL HIDALL(IDALL,NALL)
         DO 32 I = 1,NALL
            IF (HNUM .EQ. IDALL(I)) GO TO 34
   32    CONTINUE
   34    CONTINUE
         HNUM = IDALL(I-1)
         CALL D0HINT
         CALL D0HPID(HNUM)
         CALL D0HCOM
C
      ELSE IF (COMMAND(1:5) .EQ. 'SPLOT') THEN
         CALL D0HINT
         CALL D0HPID(HNUM)
         CALL D0HCOM
C
      ELSE IF (COMMAND(1:5) .EQ. 'NPLOT') THEN
         CALL HIDALL(IDALL,NALL)
         DO 36 I = 1,NALL
            IF (HNUM .EQ. IDALL(I)) GO TO 38
   36    CONTINUE
   38    CONTINUE
         HNUM = IDALL(I+1)
         CALL D0HINT
         CALL D0HPID(HNUM)
         CALL D0HCOM
C
      ELSE IF (COMMAND(1:5) .EQ. 'LPLOT') THEN
         CALL GETPAR(1,' Enter HISTOGRAM NUMBER ','I',HNUM)
         CALL D0HINT
         CALL D0HMAKLAS(HNUM)
         CALL D0HLAS
C
      ELSE IF (COMMAND(1:4).EQ. 'HEAD') THEN
        CALL GETPAR1(' Enter HISTOGRAM HEADER ','U',HEADER)
        CALL HTITLE(HEADER)
      ELSE IF (COMMAND(1:10) .EQ. 'END HISPAK') THEN
         FIRST = .TRUE.
      ENDIF
C
      RETURN
      END

      SUBROUTINE ERRMSG( IDSTRG, SUBRID, VARSTR, SEVRTY )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Message handling routine will be to accept
C-                         messages, count their occurrence, and display
C-                         them on request. See ERRMSG.MEM in D0$DOCS
C-
C-   Inputs:   IDSTRG      A character string identifies the message
C-             SUBRID      A character string identifies calling routine
C-             VARSTR      A character string provides additional
C-                         information
C-             SEVRTY      A character string of length one. It can have
C-                         'I','S','W','E', AND 'F'.
C-   Outputs : Display the messages through routines ERRLOG, ERRWRN,
C-             ERRSCR, ERRHAN and ERRFAT.
C-
C-   Controls: SEVRTY:     'I'==>Write messages to logging devices.
C-                         'S'==>Write messages to warning device only.
C-                         'W'==>Write messages to warning devices.
C-                         'E'==>Call the error recovery routine.
C-                         'F'==>Call the fatal error recoverying routine.
C-
C-   Created  16-DEC-1988   Jun-Jang Jeng (MSU)
C-   Updated   3-JAN-1989   James T. Linnemann
C-   Updated  10-JUL-1990   James T. Linnemann
C-   Updated   5-NOV-1991   Krzysztof L. Genser   
C-       to handle FATMEN long generic names
C-   Updated  11-APR-1992   Andrew J. Milder   Added Zebra logging option
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) IDSTRG,SUBRID,VARSTR,SEVRTY
      CHARACTER*32 IDIN,IDUPC
      CHARACTER*32 SUBRIN,SUBUPC
      CHARACTER*132 VARIN
      CHARACTER*1   SEV
      INTEGER LENOCC
      INTEGER POS, I, J, N
      LOGICAL FOUND,USED
      INCLUDE 'D0$ERRMSG_UTIL$PARAMS:ERRMSG.PARAMS'
      INCLUDE 'D0$INC:ERRCTL.INC'
      INCLUDE 'D0$INC:ERRCNT.INC'
      INCLUDE 'D0$INC:ERRIDS.INC'
      DATA USED/.FALSE./
C----------------------------------------------------
C **** Initialize if first call
C----------------------------------------------------
      IF (.NOT. USED) THEN
        CALL ERRINT
        USED = .TRUE.
      ENDIF
C
C     Standardize length of inputs
C
      IDIN =   IDSTRG(1:MIN(LENOCC(IDSTRG),32))
      SUBRIN = SUBRID(1:MIN(LENOCC(SUBRID),32))
      VARIN =  VARSTR(1:MIN(LENOCC(VARSTR),132))
C
      CALL UPCASE( IDIN, IDUPC )
      CALL SWORDS( SEVRTY, I, J, N )
      IF (N.LE.0) THEN
        SEV = 'W'
      ELSE
        CALL UPCASE( SEVRTY(I:I) , SEV )
      ENDIF
C
      IF ( (SEV .NE. 'I') .AND. ( SEV .NE. 'S' ) .AND.
     &     (SEV .NE. 'E') .AND. ( SEV .NE. 'F' ) )      SEV = 'W'
      IF (SEV .EQ. 'S') THEN
        CALL ERRSCR (IDIN, SUBRIN, VARIN, SEV ) ! screen messages not counted
      ELSE
C
C **** Insert IDSTRG if not already there and count it
C
        CALL ERRFND( IDUPC, POS, FOUND )
        IF  ( (.NOT. FOUND) .AND. (.NOT. OVFL) ) THEN
          CALL ERRINS( IDUPC, POS )
        ENDIF
        IF ( OVFL ) THEN
          OVLCNT = OVLCNT + 1
        ELSE
          COUNT(POS) = COUNT(POS) + 1
        ENDIF
C
C **** Log in Zebra bank if enabled
C
        CALL ERRMZB(IDUPC,SUBRIN,VARIN,SEV)
C----------------------------------------------------------
C **** Message handling
C----------------------------------------------------------
        CALL ERRLOG (IDIN, SUBRIN, VARIN, SEV, POS )
        IF (SEV .NE. 'I') THEN
          CALL ERRWRN (IDIN, SUBRIN, VARIN, SEV, POS )
          IF ( SEV .EQ. 'E' ) THEN
            CALL ERRHAN_E( IDIN, SUBRIN ,VARIN )
          ELSEIF ( SEV .EQ. 'F' ) THEN
            CALL ERRFAT( IDIN, SUBRIN, VARIN )
          ENDIF
        ENDIF
      ENDIF
C
      OVFL = .FALSE.
C
  999 RETURN
      END


      SUBROUTINE NEWPAR(TYPE,CHARS,LIMIT1,LIMIT2,CHKLIM,PAROUT,TRANOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Put a new value of a parameter in a string.
C-                         Called from GETDIS.
C-
C-   Inputs  : TYPE:   Type of parameter
C-             CHARS:  String to use as new parameter
C-             LIMIT1: Lower limit for INTEGER or REAL
C-             LIMIT2: Upper limit for INTEGER or REAL
C-             CHKLIM: Flag indicating if limits should be checked
C-   Outputs : PAROUT: New string for parameter
C-   Controls: TRANOK:  Flag indicating translation and/or limits were OK
C-
C-   Created  22-APR-1987   Jan S. Hoftun
C-   Updated  25-APR-1989   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*1 TYPE
      CHARACTER*(*) CHARS,PAROUT
      INTEGER LIMIT1,LIMIT2
      LOGICAL CHKLIM,TRANOK
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER LOCLIM(2),IN,TRAINT,J,TRULEN,LIBPUT,ISTAT,I
      INTEGER LIBERL
      REAL  RLIMIT(2),XIN,TRAREA
      EQUIVALENCE (LOCLIM,RLIMIT)
      LOGICAL LIN
      CHARACTER*40 PAREAL,PARINT,PARLOG
      CHARACTER*80 TRANUP
C----------------------------------------------------------------------
      LOCLIM(1)=LIMIT1
      LOCLIM(2)=LIMIT2
      TRANOK=.TRUE.
      IF(FULSCR) ISTAT=LIBERL(PBROWS-2,1)
      IF(TYPE.EQ.'R') THEN
        XIN=TRAREA(CHARS)
        IF(XIN.NE.-999999.99) THEN
          IF((XIN.GE.RLIMIT(1).AND.XIN.LE.
     *                RLIMIT(2)).OR..NOT.CHKLIM) THEN
            PAROUT=CHARS
          ELSE
            TRANOK=.FALSE.
            IF(SPLFLG.OR..NOT.FULSCR) THEN
              CALL INTMSG('0REAL number outside limits!'//CHAR(7))
            ELSE
              ISTAT=LIBPUT('REAL number outside limits!'//CHAR(7),
     *                      PBROWS-2,1,1)
            ENDIF
          ENDIF
        ENDIF
      ELSEIF(TYPE.EQ.'I') THEN
        IN=TRAINT(CHARS)
        IF(IN.NE.9999999) THEN
          IF((IN.GE.LOCLIM(1).AND.IN.LE.LOCLIM(2))
     *           .OR..NOT.CHKLIM) THEN
            PAROUT=PARINT(IN)
          ELSE
            TRANOK=.FALSE.
            IF(SPLFLG) THEN
              CALL INTMSG('0INTEGER number outside limits!'//CHAR(7))
            ELSE
              ISTAT=LIBPUT('INTEGER number outside limits!'//CHAR(7),
     *                      PBROWS-2,1,1)
            ENDIF
          ENDIF
        ENDIF
      ELSEIF(TYPE.EQ.'L') THEN
        CHARS=TRANUP(CHARS)
        DO J=1,TRULEN(CHARS)
          IF (CHARS(1:1).EQ.' ') CHARS=CHARS(2:)
        ENDDO
C
C       Assume that T for .true. or Y for 'yes' are the only valid .TRUE. codes.
C
        IF(CHARS(1:1).EQ.'T'.OR.CHARS(1:1).EQ.'Y') THEN
          LIN=.TRUE.
          PAROUT='True'
        ELSE
          LIN=.FALSE.
          PAROUT='False'
        ENDIF
      ELSEIF(TYPE.EQ.'C') THEN
        PAROUT=CHARS
      ELSEIF(TYPE.EQ.'U') THEN
        PAROUT=TRANUP(CHARS)
      ENDIF
      RETURN
      END

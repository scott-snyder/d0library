C+
      SUBROUTINE BKSSEC (STATION, SECTION, NDATA, VALUE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create bank SSEC and fill it with geometric
C-                         parameters for SAMUS drift tubes description
C-
C-   Inputs  : STATION - SAMUS station number
C-             SECTION - SAMUS section number
C-             NDATA - number of data words
C-             VALUE(NDATA) - array with values for volume description
C-   Outputs : None
C-   Controls:
C-
C-   Created  30-APR-1991   Andrei Kiryunin
C-   Updated   4-SEP-1992   Alexander Efimov:  create bank 'SSTG' for
C-                          parameters for all SAMUS tubes. 
C-   Updated  19-NOV-1992   Alexander Efimov:  add Ailer angles of the
C-                          stations orientation.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER STATION, SECTION
      INTEGER LSSTA, GZSSTA, NDATA, VALUE(*)
      INTEGER NFORM, LSSEC, LSSTG, NTUBES, NWORDS, NHITS
      INTEGER NUMB, SAGTUB, SAGTBL, TUBE_TYPE(2)
      REAL    RTUB(3,2), VTUB(3,2), TUBE_LENGTH
      INTEGER TUBE, J, LA, LD, IOK
      CHARACTER*80 MSGSTR 
C
C- SSTG BANK format parameters
C
      INTEGER MAXFORM
      PARAMETER (MAXFORM=10)
      INTEGER NSSTG, JSSTG
      INTEGER SSTGTUBES(MAXFORM), SSTGFORM(MAXFORM)
      CHARACTER*20 CHFORM
C
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST /.TRUE./
      DATA NSSTG/0/
C
C ****  Initialization
C
      IF (FIRST) THEN
        CALL MZFORM ('SSEC', '5I 3F 3I -F', NFORM)
        FIRST = .FALSE.
      END IF
C
C ****  Check existence of the origin bank SSTA
C
      LSSTA = GZSSTA(STATION)
      IF (LSSTA .EQ. 0) THEN
        MSGSTR = ' *** BKSSEC: supporting bank SSTA does not exist '
        CALL INTMSG (MSGSTR)
        GOTO 999
      END IF
C
C ****  Check section number
C
      IF (VALUE(1) .NE. SECTION) THEN
        VALUE(1) = SECTION
        MSGSTR = ' *** BKSSEC: link number must equal section number '
        CALL INTMSG (MSGSTR)
      ENDIF
C
C ****  Book bank SSEC
C
      CALL MZBOOK (IDVSTP, LSSEC, LSSTA, -SECTION, 'SSEC', 1, 1,
     &             NDATA, NFORM, 0)
C
C ****  Fill bank SSEC with data
C
      DO J = 1, NDATA
        IC(LSSEC+J) = VALUE(J)
      END DO
C
C ****  Book and fill bank 'SSTG'
C
      NTUBES = VALUE(4)
      NHITS = 0
      DO J = 1, NTUBES
        NUMB = SAGTUB (STATION, SECTION, J, RTUB, VTUB, TUBE_TYPE)
        NHITS = NHITS + NUMB
      END DO
      NWORDS = NTUBES + 7 * NHITS
C
C ****  Create format 
C
      JSSTG = 0
      DO J = 1,NSSTG
        IF(SSTGTUBES(J).EQ.NTUBES)THEN
          JSSTG = J
          GO TO 10
        ENDIF
      ENDDO
 10   CONTINUE
      IF(JSSTG.EQ.0)THEN
        IF(NSSTG.GE.MAXFORM)CALL ERRMSG('BKSSEC','BKSSEC',
     &    'Too many SSTG formats', 'F')
        NSSTG = NSSTG + 1
        SSTGTUBES(NSSTG) = NTUBES
        WRITE(CHFORM,'(I3,''I -F'')')NTUBES
        CALL MZFORM ('SSTG', CHFORM, SSTGFORM(NSSTG))
        JSSTG = NSSTG
      ENDIF
      CALL MZBOOK (IDVSTP, LSSTG, LSSEC, -1, 'SSTG', 0, 0,
     &             NWORDS, SSTGFORM(JSSTG), 0)
      LD = NTUBES
      DO TUBE = 1, NTUBES
        IC(LSSTG+TUBE) = LD
        NUMB = SAGTUB (STATION, SECTION, TUBE, RTUB, VTUB, TUBE_TYPE)
        DO J = 1, NUMB
          CALL SAGTB0 (STATION, RTUB(1,J), VTUB(1,J))
          IOK = SAGTBL (TUBE_TYPE(J), TUBE_LENGTH)
          C(LSSTG+LD+1) = RTUB(1,J)
          C(LSSTG+LD+2) = RTUB(2,J)
          C(LSSTG+LD+3) = RTUB(3,J)
          C(LSSTG+LD+4) = VTUB(1,J)
          C(LSSTG+LD+5) = VTUB(2,J)
          C(LSSTG+LD+6) = VTUB(3,J)
          C(LSSTG+LD+7) = TUBE_LENGTH
          LD = LD + 7
        END DO
        IF (NUMB .EQ. 2) IC(LSSTG+TUBE) = - IC(LSSTG+TUBE)
      END DO
C
  999 RETURN
      END

      SUBROUTINE RGS_SELECT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get list of selected inputs and list of
C-   input files. Select RCP bank before calling me!
C-
C-   Inputs  : None
C-   Outputs : In /RGSCOM/
C-   Controls:
C-
C-   Created  28-APR-1995   Harrison B. Prosper
C-   Updated   7-JUL-1995   Susan K. Blessing  Allow use of absolute
C-    value, > and < cuts.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:RGSCOM.INC'
C----------------------------------------------------------------------
      INTEGER I,J,II,JJ,KK,STATUS, LFILENAME
      REAL    VALUE
C
      CHARACTER*80 STRING, REMARK
      CHARACTER*255 FILENAME
      LOGICAL FOUND,LSIG,LBKG
      LOGICAL PREFIX
C----------------------------------------------------------------------
      NSELECT = 0   ! Number of selected inputs
      NFILE   = 0   ! Number of input files
C
C ****  Get NTUPLE containing cuts
C
      CALL EZ_GET_CHARS('CUT_NTUPLE',I,STRING,STATUS)
      CALL WORD(STRING,II,JJ,KK)
      CUTFILE= STRING(II:JJ)
      STRING = STRING(JJ+1:)
      NCUT   = VALUE(STRING,II,JJ,KK)
C
      WRITE(6,'(/,''  --- CUT NTUPLE FILE --- '',/)')
      CALL FIND_FILE('ZZ.ZZ',5,FILENAME,LFILENAME,FOUND)  !To force reset
      CALL FIND_FILE(CUTFILE,LEN(CUTFILE),FILENAME,LFILENAME,FOUND)
      IF ( FOUND ) THEN
        WRITE(6,'(1X,2X,A)') FILENAME(1:LFILENAME)
      ELSE
        REMARK = 'Unable to find file '//CUTFILE
        CALL ERRMSG('FILENOTFOUND','RGS_SELECT',REMARK,'F')
      ENDIF
C
C ****  Get OUTPUT NTUPLE
C
      CALL EZ_GET_CHARS('OUT_NTUPLE',I,STRING,STATUS)
      CALL WORD(STRING,II,JJ,KK)
      OUTFILE= STRING(II:JJ)
C
C ****  Get INPUT NTUPLES
C
      CALL EZ_GET_CHARS('INP_NTUPLE',NFILE,FILE,STATUS)
      IF ( NFILE   .LE. 0 ) CALL ERRMSG('NO_FILES','RGS_SELECT',
     &    'No input files. Check RGS.RCP','F')
C
C
      DO I =  1, NFILE
C
C ****  FileName
C
        STRING = FILE(I)
        CALL WORD(STRING,II,JJ,KK)
        FILE(I)= STRING(II:JJ)
C
C ****  Output label
C
        STRING = STRING(JJ+1:)
        CALL WORD(STRING,II,JJ,KK)
        IF ( KK .GT. 0 ) THEN
          LABO(I) = STRING(II:JJ)
        ELSE
          CALL STRINT('COUNT',I,STRING,KK)
          LABO(I) = STRING(1:KK)
        ENDIF
C
C ****  Number of patterns requested
C
        STRING = STRING(JJ+1:)
        CALL WORD(STRING,II,JJ,KK)
        NPATTERN(I) = VALUE(STRING,II,JJ,KK)
C
C ****  Category Number of patterns requested
C
        STRING = STRING(JJ+1:)
        CALL WORD(STRING,II,JJ,KK)
        LSIG = INDEX(STRING(II:JJ),'S').GT.0
        LBKG = INDEX(STRING(II:JJ),'B').GT.0
        STRING = STRING(JJ+1:)
        CALL WORD(STRING,II,JJ,KK)
        IF(LBKG) THEN
          BKGW(I) = VALUE(STRING,II,JJ,KK)
          IF(BKGW(I).EQ.0) BKGW(I) = 1
        ELSE IF (LSIG) THEN
          SIGW(I) = VALUE(STRING,II,JJ,KK)
          IF(SIGW(I).EQ.0) SIGW(I) = 1
        END IF
      ENDDO
      NLABO = NFILE
C
C ****  WRITE OUT FILES
C
      WRITE(6,'(/,''  --- INPUT NTUPLE FILES --- '',/)')
      DO I =  1, NFILE
        CALL FIND_FILE('ZZ.ZZ',5,FILENAME,LFILENAME,FOUND)  !To force reset
        CALL FIND_FILE(FILE(I),LEN(FILE(I)),FILENAME,LFILENAME,FOUND)
        IF ( FOUND ) THEN
          WRITE(6,'(1X,2X,A)') FILENAME(1:LFILENAME)
        ELSE
          REMARK = 'Unable to access file '//FILE(I)
          CALL ERRMSG('FILENOTFOUND','RGS_SELECT',REMARK,'F')
        ENDIF
      ENDDO
C
C ****  Get Inputs
C
      CALL EZ_GET_CHARS('PATTERNS_INPUTS',NLABI,LABI,STATUS)
      IF ( STATUS .NE. 0 ) THEN
        CALL ERRMSG('NO_INPUTS','RGS_SELECT',
     &        'Unable to access array PATTERNS_INPUTS','F')
      ENDIF
C
C ****  Count number of selected inputs
C
      WRITE (6,'(/,'' SELECTED INPUTS '',/)')
      J = 0
      IWGT = 0
      DO I =  1, NLABI
        IF ( LABI(I)(1:1) .EQ. '@' ) THEN
          WRITE(6,'(1X,2X,A)') 'WEIGHT ', LABI(I)
          IWGT = I
        ELSE IF ( LABI(I)(1:1) .NE. '*' ) THEN
C
          J = J + 1
          PATTERN_SELECT(J) = I
C
C Check for special symbols indicating absolute value (|) or cutting on
C < or >.
          PREFIX = .TRUE.
          USE_ABS(J) = .FALSE.
          UPPER_LIM(J) = .FALSE.
          LOWER_LIM(J) = .FALSE.
          DO WHILE (PREFIX)
            IF (LABI(I)(1:1).EQ.'|') THEN
              USE_ABS(J) = .TRUE.
            ELSE IF (LABI(I)(1:1).EQ.'<') THEN
              UPPER_LIM(J) = .TRUE.
            ELSE IF (LABI(I)(1:1).EQ.'>') THEN
              LOWER_LIM(J) = .TRUE.
            ELSE
              PREFIX = .FALSE.
            END IF
C Remove symbol from label.
            IF (PREFIX) THEN
              LABI(I) = LABI(I)(2:)
            END IF
          END DO
C
          WRITE(6,'(1X,2X,A)') LABI(I)
        ENDIF
      ENDDO
C
      NSELECT = J
      IF ( NSELECT .LE. 0 ) CALL ERRMSG('NO_SELECTION','RGS_SELECT',
     &    'No inputs selected. Check RGS.RCP','F')
  999 RETURN
      END

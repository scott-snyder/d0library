      SUBROUTINE NEURAL_SELECT(NFILE,FILE,INPUTS,OUTPUTS,Q,SWIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return indices of the selected inputs and
C-   outputs. Select RCP bank before calling me!
C-
C-   Inputs  : None
C-   Outputs : NFILE        [I]   Number of files
C-             FILE(*)      [C]   FileNames
C-             INPUTS(*)    [I]   Selected inputs
C-             OUTPUTS(10,*)[F]   Desired outputs
C-             Q(*)         [F]   Weights/file; if -1.0 then weights to
C-                                be obtained from ntuples
C-   Controls: SWIT         [I]   0 Train; 1 Test
C-
C-   Created   8-MAY-1992   K. Wyatt Merritt, Harrison B. Prosper
C-   Updated   7-MAR-1995   Harrison B. Prosper
C-      Implement event weighting
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NFILE
      CHARACTER*(*) FILE(*)
      INTEGER INPUTS(*)
      REAL    OUTPUTS(10,*), Q(*)
      INTEGER SWIT
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:JETNET.INC'
      INCLUDE 'D0$INC:JNDAT1.INC'
C----------------------------------------------------------------------
      INTEGER I,J,II,JJ,KK,STATUS, LFILENAME
      CHARACTER*80 STRING, REMARK
      CHARACTER*255 FILENAME
      INTEGER NBINS,ID
      INTEGER NODE_OFF,SCALE_OFF,FILE_OFF, NA, IT(MAXOUT+1)
      REAL    A(MAXOUT+1)
      LOGICAL FOUND
C----------------------------------------------------------------------
      NINPUTS  = 0
      NOUTPUTS = 0
C
C ****  Get Training (or test) files, weights and desired outputs
C
      IF ( SWIT .EQ. 0 ) THEN
C
C ****  TRAINING
C
        CALL EZ_GET_CHARS('TRAINING_SET',NFILE,FILE,STATUS)
        WRITE(6,'(/,''  --- TRAINING FILES --- '',/)')
C
        DO I =  1, NFILE
          CALL WORD(FILE(I),II,JJ,KK)
          STRING = FILE(I)(JJ+1:)
          FILE(I)= FILE(I)(II:JJ)
          CALL VALUSY(STRING,A,IT,NA,0)
          Q(I)  = A(1)            ! Weight/file
          NA    = NA - 1          ! Number of outputs; should agree with NOUT
          CALL UCOPY(A(2),OUTPUTS(1,I),NA)
        ENDDO
      ELSE
C
C ****  TESTING
C
        CALL EZ_GET_CHARS('TEST_SET',NFILE,FILE,STATUS)
        WRITE(6,'(/,''  --- TEST FILES --- '',/)')
      ENDIF
C
C ****  WRITE OUT FILES
C
      WRITE(6,'(''  '')')
      DO I =  1, NFILE
        CALL FIND_FILE('ZZ.ZZ',5,FILENAME,LFILENAME,FOUND)  !To force reset
        CALL FIND_FILE(FILE(I),LEN(FILE(I)),FILENAME,LFILENAME,FOUND)
        IF ( FOUND ) THEN
          WRITE(6,'(1X,2X,A)') FILENAME(1:LFILENAME)
        ELSE
          REMARK = 'Unable to access file '//FILE(I)
          CALL ERRMSG('FILENOTFOUND','NEURAL_SELECT',REMARK,'F')
        ENDIF
      ENDDO
C
C
      CALL EZ_GET_CHARS('PATTERNS_INPUTS',NLABI,LABI,STATUS)
      IF ( STATUS .NE. 0 ) THEN
        CALL ERRMSG('NO_INPUTS','NEURAL_SELECT',
     &        'Unable to access array PATTERNS_INPUTS','F')
      ENDIF
C
      CALL EZ_GET_CHARS('PATTERNS_OUTPUTS',NLABO,LABO,STATUS)
      IF ( STATUS .NE. 0 ) THEN
        CALL ERRMSG('NO_PATTERN_OUTPUTS','NEURAL_SELECT',
     &        'Assume 1 output called OUTPUT','I')
        NLABO = 1
        LABO(1) = 'OUTPUT'
      ENDIF
      CALL EZGET('HISTO_BINS',NBINS,STATUS)
      IF ( STATUS .NE. 0 ) THEN
        CALL ERRMSG('NO_HISTO_BINS','NEURAL_SELECT',
     &        'HISTO_BINS set to 50','I')
        NBINS = 50
      END IF
C
C ****  GET HISTOGRAM ID OFFSETS
C
      CALL EZGET('INPUT_NODE_OFFSET',NODE_OFF,STATUS)
      IF ( NODE_OFF .LE. 0 ) NODE_OFF=1
      CALL EZGET('FILE_OFFSET',FILE_OFF,STATUS)
      IF ( FILE_OFF .LE. 0 ) FILE_OFF=100
      CALL EZGET('SCALE_OFFSET',SCALE_OFF,STATUS)
      IF ( SCALE_OFF .LE. 0 ) SCALE_OFF=1000
C
C ****  Count number of inputs
C
      WRITE (6,'(/,'' INPUT variables used in the network'',/)')
      J = 0
      DO I =  1, NLABI
        IF ( LABI(I)(1:1) .NE. '*' ) THEN
          WRITE(6,'(1X,2X,A)') LABI(I)(1:16)
          J = J + 1
          INPUTS(J) = I
          ID = SCALE_OFF+NODE_OFF*J
          CALL HBOOK1(ID,LABI(I)(1:16),NBINS,0,1.0,0)
        ENDIF
      ENDDO
C
C ****  SET NUMBER OF INPUTS (in /JETNET/)
C
      NINPUTS = J
C
C ****  Count number of outputs
C
      WRITE (6,'(/,'' OUTPUT variables used in the network'',/)')
      DO I =  1, NLABO
        WRITE(6,'(1X,2X,A)') LABO(I)(1:16)
        ID = SCALE_OFF+NODE_OFF*(NINPUTS+I)
        CALL HBOOK1(ID,LABO(I)(1:16),NBINS,0,1.0,0)
      ENDDO
C
C ****  SET NUMBER OF OUTPUTS (in /JETNET/)
C
      NOUTPUTS = NLABO
C
      IF ( SWIT .EQ. 0 ) THEN
        IF ( NOUTPUTS .NE. NA ) THEN
          CALL ERRMSG('WRONG_NO_OF_WEIGHTS','NEURAL_SELECT',
     &        'Check TRAINING_SET and PATTERN_OUTPUTS','W')
        ENDIF
      ENDIF
  999 RETURN
      END

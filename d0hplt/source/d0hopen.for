      SUBROUTINE D0HOPEN(LUNI,FILENAME,OPT,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Opens a file machine independent
C-
C-   Inputs  : LUIN      [I]: Unit to use to open the file
C-             FILENAME [C*]: file to open
C-             OPT      [C*]: Character option
C-            'I': Input (VAX status OLD, READONLY)
C-            'O': Output (VAX status NEW, SGI UNKNOWN)
C-            'A' append (this option may not be available on all machine
C-            'F' formatted                                              
C-            'U' unformatted                                            
C-            'D' delete after close (only allowed combined with O,      
C-                                   i.e. 'OD' or 'DO')                 
C-            'L' Carriagecontrol = 'LIST'
C-           combinations allowed but some are illegal (i.e. 'UF')      
C-           defaults 'I' and 'F' (i.e. ' ' equivalent to 'IF' or 'FI') 
C-           
C-   Outputs : IER      [I]: 0 IF OK 
C-
C-   Created  24-JAN-1992   Lupe Howell
C-   Updated  16-MAR-1992   Lupe Howell  Tidy up
C-   Updated   2-SEP-1992   sss - compile on ibm
C-   Updated   3-Jan-1996   sss - Compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER LUNI,IER
      CHARACTER*(*) FILENAME
      CHARACTER*(*) OPT
C----------------------------------------------------------------------
      INTEGER L,I,J,K,FLEN,IOPT
      CHARACTER*12 FORM_CODE
      CHARACTER*4  STATUS_CODE
C----------------------------------------------------------------------
C
C ****  Check on input file name
C
      CALL WORD(FILENAME,I,J,FLEN)
      IF( FLEN .EQ. 0 ) THEN
        GOTO 100 
      ENDIF
C
C ****  Check on option
C
      IER = 0
      STATUS_CODE = 'OLD'
      FORM_CODE = 'FORMATTED'
      L = LEN(OPT)

      IF ( I .EQ. 0 .OR. OPT .EQ. ' ') THEN
        IOPT = 1
      ELSE
        IOPT = 0

        DO 10 I = 1, L 
          IF( OPT(I:I) .EQ. 'U' ) FORM_CODE = 'UNFORMATTED'
   10   CONTINUE

        DO 20 I = 1, L
          IF( OPT(I:I) .EQ. 'F' ) THEN
            IF ( FORM_CODE .EQ. 'UNFORMATTED' ) GOTO 100
          ENDIF
   20   CONTINUE

          DO 30 I = 1, L
            IF( OPT(I:I) .EQ. 'I' ) IOPT = 1 
   30     CONTINUE

          DO 40 I = 1, L
            IF( OPT(I:I) .EQ. 'O' ) THEN
              IF( IOPT .EQ. 0 ) STATUS_CODE = 'NEW'
              IOPT = 2
            ENDIF
   40     CONTINUE

          DO 50 I = 1, L 
            IF( OPT(I:I) .EQ. 'A' ) THEN
              IOPT = 3
            ENDIF
   50     CONTINUE

          DO 60 I = 1, L
            IF( OPT .EQ. 'D' ) THEN
              IOPT = 4
            ENDIF
   60     CONTINUE

          DO 70 I = 1, L
            IF( OPT(I:I) .EQ. 'L' ) THEN
              IOPT = 5
              IF(( STATUS_CODE .NE. 'NEW' ) .AND. 
     &           ( FORM_CODE   .NE. 'FORMATTED')) THEN
                GOTO 100
              ENDIF
            ENDIF
   70     CONTINUE
        ENDIF

        
C
C ****  READONLY Case
C
      IF ( IOPT .LT. 2 ) THEN
C&IF VAXVMS  
        OPEN( LUNI,FORM=FORM_CODE,STATUS=STATUS_CODE,READONLY,
     &        FILE=FILENAME,ERR=100)
C&ELSEIF IBMAIX,LINUX
C&        OPEN( LUNI,FORM=FORM_CODE,STATUS='UNKNOWN',
C&     &        FILE=FILENAME,ERR=100)
C&ELSE
C&        OPEN( LUNI,FORM=FORM_CODE,STATUS='UNKNOWN',READONLY,
C&     &        FILE=FILENAME,ERR=100)
C&ENDIF

C
C ****  INPUT Case
C
      ELSEIF( IOPT .EQ. 2  ) THEN
C&IF VAXVMS 
        OPEN(UNIT=LUNI,FORM=FORM_CODE,STATUS=STATUS_CODE,
     &      FILE=FILENAME,ERR=100)                
C&ELSE
C&        OPEN(UNIT=LUNI,FORM=FORM_CODE,STATUS='UNKNOWN',
C&     &      FILE=FILENAME,ERR=100)                
C&ENDIF

C
C ****  APPEND Option
C
      ELSEIF( IOPT .EQ. 3 ) THEN
C&IF VAXVMS 
        OPEN(UNIT=LUNI,FORM=FORM_CODE,STATUS=STATUS_CODE,
     &       ACCESS='APPEND',FILE=FILENAME,ERR=100)     
C&ELSEIF IBMAIX
C&        OPEN(UNIT=LUNI,FORM=FORM_CODE,STATUS='OLD',
C&     &       FILE=FILENAME,ERR=100)
C&ELSE
C&        OPEN(UNIT=LUNI,FORM=FORM_CODE,STATUS='UNKNOWN',
C&     &       ACCESS='APPEND',FILE=FILENAME,ERR=100)
C&ENDIF

C
C ****  DELETE After Close
C
      ELSEIF( IOPT .EQ. 4 ) THEN
C&IF VAXVMS
        OPEN(UNIT=LUNI,FORM=FORM_CODE,STATUS=STATUS_CODE,
     &       DISPOSE='DELETE',FILE=FILENAME,ERR=100)
C&ELSEIF IBMAIX,LINUX
C&        OPEN(UNIT=LUNI,FORM=FORM_CODE,STATUS='SCRATCH',ERR=100)       
C&ELSE
C&        OPEN(UNIT=LUNI,FORM=FORM_CODE,STATUS='UNKNOWN',
C&     &       DISPOSE='DELETE',FILE=FILENAME,ERR=100)       
C&ENDIF

      ELSEIF( IOPT .EQ. 5 ) THEN
C&IF VAXVMS
        OPEN( LUNI,FORM=FORM_CODE,STATUS=STATUS_CODE,
     &        FILE=FILENAME,CARRIAGECONTROL='LIST',ERR=100)
C&ELSEIF IBMAIX,LINUX
C&        OPEN( LUNI,FORM=FORM_CODE,STATUS=STATUS_CODE,
C&     &        FILE=FILENAME,ERR=100)
C&ELSE
C&        OPEN( LUNI,FORM=FORM_CODE,STATUS=STATUS_CODE,
C&     &        FILE=FILENAME,CARRIAGECONTROL='LIST',ERR=100)
C&ENDIF

      ENDIF
  999 RETURN
C
C ****  Error message
C
  100 IER = -1
      RETURN
      END

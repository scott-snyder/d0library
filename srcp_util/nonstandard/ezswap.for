      PROGRAM EZSWAP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Replace xxSRCP names with EZxxxx for given
C-                         set of text files.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-MAR-1989   Harrison B. Prosper
C-   Updated  12-Feb-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*160 FILE_SPEC,EXLIST
      CHARACTER*80  STRING,FILE_NAME,PATH_NAME,OLD_FILE
      INTEGER       N,STATUS,CONTEXT,I,J,K,NNAMES,II,JJ,NN
      INTEGER ISTART,IEND
      INTEGER LUNINP,LUNOUT,LUNLOG
      PARAMETER( LUNINP = 10 )
      PARAMETER( LUNOUT = 20 )
      PARAMETER( LUNLOG = 30 )
      PARAMETER( NNAMES = 42 )
      LOGICAL       ACTIVE, OK
      CHARACTER*6   NAME(2,NNAMES),FORM
      INTEGER       KEX,KEXMAX
      PARAMETER( KEXMAX = 20 )
      INTEGER       LEX(KEXMAX)
      CHARACTER*64  EXCLUDE(KEXMAX)
C
      INTEGER LIB$FIND_FILE
      DATA NAME/
     &  'GTSRCP','EZGSET',
     &  'RDSRCP','EZRDF ',
     &  'PRSRCP','EZDUMP',
     &  'GZSRCP','EZLOC ',
     &  'GXSRCP','EZGET ',
     &  'ERSRCP','EZERR ',
     &  'FTSRCP','EZIN  ',
     &  'SLSRCP','EZPICK',
     &  'RSSRCP','EZRSET',
     &  'RNSRCP','EZRNAM',
     &  'RXSRCP','EZREAD',
     &  'SXSRCP','EZSET ',
     &  'SVSRCP','EZOUT ',
     &  'RYSRCP','EZRDAR',
     &  'ASSRCP','EZASIZ',
     &  'CKSRCP','EZZCHK',
     &  'CLSRCP','EZZCLK',
     &  'CRSRCP','EZBOOK',
     &  'CVSRCP','EZCDAT',
     &  'DBSRCP','EZDBUG',
     &  'ENSRCP','EZEND ',
     &  'GNSRCP','EZPAR ',
     &  'GRSRCP','EZGETD',
     &  'HCSRCP','EZHDRC',
     &  'HDSRCP','EZHDRI',
     &  'MVSRCP','EZMOVE',
     &  'NASRCP','EZNAME',
     &  'RASRCP','EZRDA ',
     &  'SZSRCP','EZSIZE',
     &  'WRSRCP','EZFILL',
     &  'DCSRCP','EZZDCD',
     &  'DRSRCP','EZZDRC',
     &  'EXSRCP','EZZEXT',
     &  'GPSRCP','EZZGPT',
     &  'IRSRCP','EZZIRC',
     &  'LCSRCP','EZZLOC',
     &  'PXSRCP','EZZDMP',
     &  'RFSRCP','EZZRFM',
     &  'NONAME','EZGETR',
     &  'NONAME','EZGREM',
     &  'NONAME','EZSETR',
     &  'NONAME','EZSREM'/

C----------------------------------------------------------------------
      WRITE (6,10)
   10 FORMAT(' _File [/EX=string-list]: ',$)
      READ (5,11) FILE_SPEC
   11 FORMAT(A160)
      CALL STR$UPCASE (FILE_SPEC,FILE_SPEC)
C
C ****  Create exclusion list
C
      I = INDEX(FILE_SPEC,'/EX')
      IF ( I .GT. 0 ) THEN
C
        J = INDEX(FILE_SPEC(I:),'=')
        IF ( J .GT. 0 ) THEN
          EXLIST = FILE_SPEC(I+J:)
          FILE_SPEC = FILE_SPEC(:I-1)
          CALL SWORDS (EXLIST,I,J,N)
          IF ( EXLIST(I:I) .EQ. '(' ) I = I + 1
          IF ( EXLIST(J:J) .EQ. ')' ) J = J - 1
          EXLIST = EXLIST(I:J)
C
          KEX = 0                       ! Zero exclusion list counter
          ACTIVE = .TRUE.
          DO WHILE (ACTIVE)
            I = INDEX(EXLIST,'"')
            IF ( I .GT. 0 ) THEN
              J = INDEX(EXLIST(I+1:),'"')
              IF ( J .GT. 0 ) THEN
                KEX = KEX + 1
                EXCLUDE(KEX) = EXLIST(I+1:I+J-1)
                LEX(KEX) = J - 1
                EXLIST = EXLIST(I+J+1:)
              ELSE
                WRITE(6,15) EXLIST(I:)
                GOTO 999
              ENDIF
C
            ELSE
              ACTIVE = .FALSE.
            ENDIF
          ENDDO
        ELSE
          WRITE(6,15) FILE_SPEC(I:)
   15     FORMAT(' %EZSWAP-E-SYNTAX, Something wrong here: ',A32/)
          GOTO 999
        ENDIF
      ENDIF
C
C ****  Open LOG file
C
      CALL D0OPEN (LUNLOG,'EZSWAP.LOG','OFL',OK)
      IF(.NOT.OK)CALL D0_ABORT(' D0OPEN failed')
C
      FORM      = '(A)'
      CONTEXT   = 0
      OLD_FILE  = ' '
      ACTIVE    = .TRUE.
      CALL SWORDS (FILE_SPEC,II,JJ,N)
C
      DO WHILE ( ACTIVE )
C&IF VAXVMS
        STATUS = LIB$FIND_FILE
     &      (FILE_SPEC(II:JJ),PATH_NAME,CONTEXT,,,,)
C
        IF ( .NOT. STATUS ) THEN
C&ENDIF
          ACTIVE = .FALSE.
C&IF VAXVMS
        ELSE
C&ENDIF
C
C ****  Get file name
C
          I = INDEX(PATH_NAME,']')+1
          J = INDEX(PATH_NAME,';')-1
          FILE_NAME = PATH_NAME(I:J)
C
C ****  Open files
C
          CALL D0OPEN (LUNINP,PATH_NAME,'IF',OK)
          IF(.NOT.OK)CALL D0_ABORT(' D0OPEN failed')
          CALL D0OPEN (LUNOUT,FILE_NAME,'OFL',OK)
          IF(.NOT.OK)CALL D0_ABORT(' D0OPEN failed')
C
          DO WHILE ( ACTIVE )
            READ (UNIT=LUNINP,FMT=20,END=50) STRING
   20       FORMAT(A)
C
            CALL SWORDS (STRING,ISTART,IEND,NN)
C
            I = INDEX(STRING,'SRCP')
C
            IF ( I .GT. 0 ) THEN
C
C ****  Apply exclusion list
C
              IF ( KEX .GT. 0 ) THEN
                DO 21 K =  1,KEX
                  IF (INDEX(STRING,EXCLUDE(K)(1:LEX(K))) .GT. 0)
     &            GOTO 40
   21           CONTINUE
              ENDIF
C
              DO 30 J =  1,NNAMES
C
C ****  Find matching name
C
                IF ( STRING(I-2:I+3) .EQ. NAME(1,J) ) THEN
C
                  IF ( FILE_NAME .NE. OLD_FILE ) THEN
                    WRITE(6,25) PATH_NAME
                    WRITE(LUNLOG,25) PATH_NAME
   25               FORMAT
     &                  (1X,'*************************************'/,
     &                     1X,A72,/)
                    OLD_FILE = FILE_NAME
                  ENDIF
C
C ****  Update LOG
C
                  WRITE(6,FMT=FORM) ' '//STRING(1:IEND)
                  WRITE(LUNLOG,FMT=FORM) STRING(1:IEND)
C
                  STRING = STRING(1:I-3)//NAME(2,J)//STRING(I+4:)
C
                  WRITE(6,FMT=FORM) ' '//STRING(1:IEND)
                  WRITE(LUNLOG,FMT=FORM) STRING(1:IEND)
                  WRITE(UNIT=6,FMT='(''  '')')
                  WRITE(UNIT=LUNLOG,FMT='(''  '')')
C
                  GOTO 40
                ENDIF
   30         CONTINUE
            ENDIF
C
C ****  Write to output file
C
   40       CONTINUE
            WRITE(UNIT=LUNOUT,FMT=FORM) STRING(1:IEND)
          ENDDO
C
   50     CONTINUE
          CLOSE (UNIT=LUNINP)
          CLOSE (UNIT=LUNOUT)
C&IF VAXVMS
        ENDIF
C&ENDIF
      ENDDO
      CLOSE (UNIT=LUNLOG)
C
  999 CONTINUE
      END

      SUBROUTINE PX_FIND_FILE_EXT(FILENAME,DIRNAME,FILE_LENGTH,
     &  DIR_LENGTH,EXT,FOUND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a file name, a directory, and an extension 
C-   it will search for the file using the extension plus a number between 9 and
C-   1.  For example it will search for dirname//filename.//*ext//#, where # 
C-   is number from 9 to 1.  
C-
C-   Inputs  : FILENAME  [C*]: Name of the file
C-             DIRNAME   [C*]: Directory of the file
C-             FILE_LENGTH[I]: Length of the file name
C-             DIR_LENGTH [I]: Length of the directory
C-             EXT       [C*]: Extension to use in search
C-             
C-   Outputs : FOUND      [L]: Flag that is set to TRUE if the file was found
C-
C-   Created  27-MAY-1993   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) FILENAME
      CHARACTER*(*) DIRNAME
      CHARACTER*(*) EXT
      INTEGER FILE_LENGTH,DIR_LENGTH
      LOGICAL FOUND
C
      CHARACTER*160 DATFILNM,STRINGDF,DATFILE,DATAFILE,TEMP_EXT
      CHARACTER*1 V
      INTEGER INDEX,KF,IV,I,J,K,IDF,NDF,LDF,CONTXT,ISTAT,NM
      INTEGER LIB$FIND_FILE,LIB$FIND_FILE_END,EXTI,EXTJ
      LOGICAL ACTIVE,NOTFOUND,ODD,FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
C ****  Defining the ODD function
C
        ODD(NM) = 2*INT (NM/2).NE.NM
C
C ****  Find where the "." is to modify the length of the filename
C ****  excluding the extension
C
      KF = INDEX(FILENAME,'.')
      IF ( KF .GT. 0 ) THEN
        FILE_LENGTH = KF - 1
      ENDIF

      DATFILNM = DIRNAME(1:DIR_LENGTH)//FILENAME(1:FILE_LENGTH)
      STRINGDF = DATFILNM
      CALL SWORDS(STRINGDF,IDF,NDF,LDF)
C
C ****  Searching for the file with the extension .EXT(9-1)
C
      NOTFOUND = .TRUE.
      ACTIVE = .TRUE.
      IV = 9
      CALL SWORDS(EXT,EXTI,EXTJ,K)
      DO WHILE ( ACTIVE )
        CALL PXITOC(IV,1,V)
C&IF SIUNIX
C&             CALL CUTOL(EXT)
C&ENDIF
        TEMP_EXT = '.*'//EXT(EXTI:EXTJ)//V(1:1)//' '
        CALL SWORDS(TEMP_EXT,I,J,K)
        DATFILNM=DATFILNM(1:LDF)//TEMP_EXT(I:J)
        STRINGDF=DATFILNM
        CALL SWORDS(STRINGDF,IDF,NDF,LDF)
        DATFILNM=DATFILNM(IDF:LDF)
C
C *** Find the file
C
        CONTXT = 0
        DATAFILE = ' '
        ISTAT = LIB$FIND_FILE(DATFILNM(IDF:NDF),DATAFILE,CONTXT,0,0,0,0)
C
C ****  If the file is NOT found drop the extension and
C ****  continue search
C
        IF (.NOT.ODD(ISTAT)) THEN
          KF = INDEX(DATFILNM,'.')
          IF ( KF .GT. 0 ) THEN
            LDF = KF - 1
          ENDIF
          ISTAT  = LIB$FIND_FILE_END(CONTXT)
          IV = IV - 1
C
C ****  If the file IS FOUND exit
C
        ELSE
          NOTFOUND = .FALSE.
          ISTAT    = LIB$FIND_FILE_END(CONTXT)
        ENDIF
        ACTIVE =  ( IV .GT. 0 ) .AND. ( NOTFOUND )
      ENDDO
      FOUND = .NOT. NOTFOUND
  999 RETURN
      END

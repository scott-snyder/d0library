      SUBROUTINE PBD_COMMAND
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  This routine parses the PBD command qualifiers,
C-                          verifies them and sets up various tables for
C-                          use by other PBD modules. It outputs the error
C-                          messages for any invalid input qualifiers. 
C-
C-   Inputs  : PBD user's runtime commands
C-   Outputs : 
C-   Controls: Common error flag ERROR_FLAG
C-            ( .TRUE. if fatal error, .FALSE. if no error ) 
C-
C-   Modules called by this routine:  PBD_GET_COM, PBD_IS_VALID, PBD_MSG, 
C-                                    PBD_REM_BLANK, PBD_UP_CASE,
C-                                    LIB$GET_FOREIGN, LIB$SIGNAL
C-                                    
C-  
C-   Based on the PASCAL procedure Build_Initialization_Routine of the old
C-   Program Builder.
C-
C-   Created  21-JUN-1991   Hyon Joo Kehayias
C-   Modified 07-NOV-1991   Hyon Joo Kehayias
C-   Updated  17-DEC-1991   Herbert Greenlee
C-       UNIX compatible version
C-   Updated  14-MAR-1992   H. Kehayias for a new qualifier HSTRFL
C-   Updated  24-May-1992   H. Greenlee
C-       Additional changes for UNIX
C-   Updated  23-OCT-1992   H. Kehayias 
C-      ( Runtime parameters are converted to upper case for UNIX )
C-   Updated  05-FEB-1993   H. Kehayias 
C-      ( Increased string size of INPQUAL, QUALVAL, TEMPSTR )  
C-   Updated  28-MAY-1993   H. Kehayias 
C-      ( Removed code for checking package names and generating global
C-        package names table ) 
C-   Updated  01-NOV-1993   H. Kehayias
C-      ( Added a new qualifier SWITCH ) 
C-   Updated  10-Jan-1994   H. Greenlee (from C. Silva)
C-      ( Added /UNIX switch to allow generation of unix scripts).
C-        
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PROGRAM_BUILDER$INC:PBD_COMMON.INC'

C&IF VAXVMS
      INCLUDE '($SSDEF)'
C&ELSE
C&      INCLUDE 'D0$INC:SSDEF.DEF'
C&ENDIF
      INTEGER*4 LIB$GET_FOREIGN,LIB$SIGNAL  ! RUN-TIME LIBRARY MODULES
      INTEGER*4 COM_SIZE    ! INPUT ZEBRA BANK SIZE
      INTEGER*2 PARAMLEN    ! RUN TIME COMMAND STRING LENGTH
      INTEGER*4 STATUS      ! RUN-TIME LIB. CALL RETURN STATUS
      INTEGER*2 ENDPOS,EQUPOS
      INTEGER*2 TEMPINDX, TEMPINDX1, I, I1, J, K, L
      INTEGER*2 QUALLEN           ! QUALIFIER STRING LENGTH
      INTEGER*2 VALLEN            ! QUALIFIER VALUE STRING LENGTH
      INTEGER*2 STRTPOS, QUALINDX, TEMPLEN
      INTEGER*2 OFFSET            ! OFFSET TO PACKAGE NAME TABLE
      CHARACTER*512 RUNPARAM      ! RUN TIME COMMAND LINE
      CHARACTER*256 QUALVAL       ! QUALIFIER VALUE
      CHARACTER*20 QUALIFIER(NUMQUAL) ! PBD COMMAND QUALIFIERS
      CHARACTER*256 INPQUAL       ! INPUT QUALIFIER BEING PROCESSED
      CHARACTER*256 TEMPSTR       ! TEMPORARY CHARACTER STRING
      CHARACTER*80 ERROR_BUF(12)  ! ERROR MESSAGE BUFFER
      CHARACTER*80 MESSAGE(4)     ! MESSAGES
      CHARACTER*80 MSG_BUF        ! MESSAGE BUFFER
      CHARACTER*10 COM_NAME       ! ZEBRA COMMON NAME
      CHARACTER*1 BLANK
      CHARACTER*1 COMMA
      CHARACTER*1 LEFTPAREN
      CHARACTER*1 RIGHTPAREN
      CHARACTER*1 EQUALSIGN
      CHARACTER*1 QUAL_DELIM      ! QUALIFIER DELIMITER
      LOGICAL DONE, FOUND, SKIP
      LOGICAL RETFLAG             ! ZEBRA COMMON ERROR FLAG
      LOGICAL OPENED
      DATA BLANK /' '/
      DATA COMMA /','/
      DATA LEFTPAREN /'('/
      DATA RIGHTPAREN /')'/
      DATA EQUALSIGN /'='/
      DATA QUAL_DELIM /'/'/
C
C     Initialize qualfier string
C
      DATA QUALIFIER / 'FRAMEWORK',
     &                 'NAME',
     &                 'PACKAGES',
     &                 'DIRECTORY',
     &                 'ZEBCOM',
     &                 'ZEBSTP',
     &                 'ZEBWRK',
     &                 'PAWC',
     &                 'GCBANK',
     &                 'PRODID',
     &                 'VERSION',
     &                 'PASS',
     &                 'LOG',
     &                 'NOLOG',
     &                 'COMPILE',
     &                 'NOCOMPILE',
     &                 'HISTORY_BANK',
     &                 'NOHISTORY_BANK',
     &                 'HSTRFL',
     &                 'SWITCH',
     &                 'UNIX'/
C
      DATA ERROR_BUF /
     & '-E-Empty package name',
     & '-E-Right parenthesis missing in package list',
     & '-E-Invalid PBD qualifier ',
     & '-E-No package name provided ',
     & '-E-Invalid package list specification',
     & '-E-Null input qualifier',
     & '-E-Missing frame name',
     & '-E-Missing combined package name',
     & '-E-Missing directory name',
     & '-E-Missing package name',
     & '-E-Multiple packages - combined package name required ',
     & '-W-Duplicate package name '/
C
      DATA MESSAGE /
     & '-W-Log file open error - LOG function disabled',
     & '-- Validating input PBD command qualifiers --',
     & '-- PBD LOG function enabled - Log File : PBD.LOG ',
     & '-- PBD Processing Started -- '/ 
C
      ERROR_FLAG = .FALSE.
C
C     Get run time parameters 
C
      STATUS = LIB$GET_FOREIGN(RUNPARAM,%VAL(0),PARAMLEN,%VAL(0))
      IF ( STATUS .NE. SS$_NORMAL ) STATUS = LIB$SIGNAL(%VAL(STATUS))
      CALL PBD_UP_CASE (RUNPARAM(1:PARAMLEN),RUNPARAM(1:PARAMLEN))
C
C
C     First determine whether LOG option specified.  If so, open user
C     log file PBD.LOG
C
      LOG = .FALSE.
      STATUS = 0
      IF ( INDEX ( RUNPARAM,'NOLOG') .EQ. 0 ) THEN 
        IF ( INDEX ( RUNPARAM,'LOG') .GT. 0 ) THEN
          CALL D0OPEN(4, 'PBD.LOG', 'OFL', OPENED) 
          IF ( OPENED ) THEN
            LOG = .TRUE.
            CALL PBD_MSG ( MESSAGE(3) )
          ELSE
            CALL PBD_MSG ( MESSAGE(1) )
          END IF
        END IF
      END IF

      CALL PBD_MSG ( MESSAGE(4) )
      CALL PBD_MSG ( MESSAGE(2) )
C
C     SKIP THE FIRST '/'
C
      STRTPOS = INDEX ( RUNPARAM, QUAL_DELIM)
      RUNPARAM = RUNPARAM ( STRTPOS+1: PARAMLEN )
      PARAMLEN = PARAMLEN - STRTPOS
C
C     Revmoe all blanks in the input command string
C
      CALL PBD_REM_BLANK ( RUNPARAM(1:PARAMLEN),PARAMLEN )
      STRTPOS = 1
      DONE = .FALSE.
      ERROR_FLAG = .FALSE.              ! Set default - no qualifier error
C
C     Set default qualifer flags
C
      DO I = 1,NUMQUAL
        QUALFLAG(I) = .FALSE.
      END DO
C&IF VAXVMS
C&ELSE
C&      QUALFLAG(21) = .TRUE.
C&ENDIF
C
C     Set default compile option
C 
      QUALFLAG( 15 ) = .TRUE.

      DO WHILE ( STRTPOS .LE. PARAMLEN .AND. .NOT. DONE )

        INPQUAL = ' '
        QUALLEN = 0
        ENDPOS = INDEX ( RUNPARAM(STRTPOS:PARAMLEN), QUAL_DELIM)
        IF ( ENDPOS .GT. 0 ) THEN
C
C         Set end pointer in accordance with the whole RUNPARAM string
C
          ENDPOS = STRTPOS + ENDPOS - 1
C
C         Extract the qualifier
C
          INPQUAL = RUNPARAM(STRTPOS:ENDPOS-1)
          QUALLEN = ENDPOS - STRTPOS
C
C         Update start pointer for the next qualifier
C
          STRTPOS = ENDPOS + 1
C
        ELSE
C
C         Must be the last qualifier
C
          INPQUAL = RUNPARAM(STRTPOS:PARAMLEN)
          QUALLEN = PARAMLEN - STRTPOS + 1
          DONE = .TRUE.
        END IF

       IF ( QUALLEN .EQ. 0 ) THEN
C
C        Empty qualifier, send error message
C
         ERROR_FLAG = .TRUE.
         CALL PBD_MSG ( ERROR_BUF (6) )
       
       ELSE

C
C        Get the associated qualifier value if exists
C
         EQUPOS = INDEX ( INPQUAL(1:QUALLEN), EQUALSIGN )
         QUALVAL = '  '
         VALLEN = 0
         IF ( EQUPOS .GT. 0 ) THEN
           QUALVAL = INPQUAL ( EQUPOS+1:QUALLEN )
           VALLEN = QUALLEN - EQUPOS
           INPQUAL = INPQUAL(1:EQUPOS-1)
           QUALLEN = EQUPOS - 1

         END IF
C
C       Verify the input qualifier against the qualifier table
C
        J = 1
        FOUND = .FALSE.
        DO WHILE ( J .LE. NUMQUAL .AND. .NOT.FOUND )
          SKIP = .FALSE.
          K = 1
          DO WHILE ( K .LE. QUALLEN .AND. .NOT. SKIP )
            IF ( INPQUAL(K:K) .NE. QUALIFIER (J) (K:K) ) THEN
              SKIP = .TRUE.
            END IF

            K = K + 1
          END DO

          IF ( .NOT. SKIP ) FOUND = .TRUE.
          J = J + 1
        END DO
C
C       If valid qualifier, get the value
C
        IF ( FOUND ) THEN
          QUALINDX = J - 1
          QUALFLAG( QUALINDX ) = .TRUE.

          IF ( QUALINDX .EQ. FRAME_QUAL ) THEN
C
C           Store the framework name in common data area
C
            IF ( VALLEN .GT. 0 ) THEN
              FRAME_NAME = QUALVAL
              FRAME_LEN = VALLEN
            ELSE                        ! Missing frame name, send error
              ERROR_FLAG = .TRUE.
              CALL PBD_MSG ( ERROR_BUF(7) )
            END IF

          ELSE IF ( QUALINDX .EQ. NAME_QUAL ) THEN
C
C           Store the combined package name in common data area
C
            IF ( VALLEN .GT. 0 ) THEN
              COMBINED_NAME = QUALVAL
              COMBINED_LEN = VALLEN
            ELSE                        ! Missing comb. package name, send error
              ERROR_FLAG = .TRUE.
              CALL PBD_MSG ( ERROR_BUF(8) )
            END IF


          ELSE IF ( QUALINDX .EQ. LIBRARY_QUAL ) THEN
C
C           Store the directory name in common data area
C
            IF ( VALLEN .GT. 0 ) THEN

              LIBRARY_NAME = QUALVAL
              LIBRARY_LEN = VALLEN
C
C             If logical name entered for directory, attatch ':' at the end
C
              IF ( INDEX( LIBRARY_NAME,'[' ) .EQ. 0 .AND.
     &             INDEX( LIBRARY_NAME,']' ) .EQ. 0 .AND.
     &             LIBRARY_NAME(LIBRARY_LEN:LIBRARY_LEN) .NE. ':')THEN
                LIBRARY_LEN = LIBRARY_LEN + 1
                LIBRARY_NAME(LIBRARY_LEN:LIBRARY_LEN) = ':'
              END IF
       
            ELSE                        ! Missing directory name, send error
              ERROR_FLAG = .TRUE.
              CALL PBD_MSG ( ERROR_BUF(9) )
            END IF
C
C         Process zebra common block qualifiers
C
          ELSE IF ( QUALINDX .EQ. ZEBCOM_QUAL ) THEN
C
C           Validate the ZEBCOM size
C
            COM_NAME = 'ZEBCOM'
            CALL PBD_GET_COM( QUALVAL,VALLEN, COM_NAME(1:6), 
     &           ZEBCOM_SIZE, ZEBCOM_LEN, RETFLAG ) 
            IF ( RETFLAG ) ERROR_FLAG = .TRUE.
          
          ELSE IF ( QUALINDX .EQ. ZEBSTP_QUAL ) THEN
C
C           Validate the ZEBSTP size
C
            COM_NAME = 'ZEBSTP'
            CALL PBD_GET_COM( QUALVAL,VALLEN, COM_NAME(1:6), 
     &           ZEBSTP_SIZE, ZEBSTP_LEN, RETFLAG ) 
            IF ( RETFLAG ) ERROR_FLAG = .TRUE.
          
          ELSE IF ( QUALINDX .EQ. ZEBWRK_QUAL ) THEN
C
C           Validate the ZEBWRK size
C
            COM_NAME = 'ZEBWRK'
            CALL PBD_GET_COM( QUALVAL,VALLEN, COM_NAME(1:6), 
     &           ZEBWRK_SIZE, ZEBWRK_LEN, RETFLAG ) 
            IF ( RETFLAG ) ERROR_FLAG = .TRUE.
      
          ELSE IF ( QUALINDX .EQ. PAWC_QUAL ) THEN
C
C           Validate the PAWC size
C
            COM_NAME = 'PAWC'
            CALL PBD_GET_COM( QUALVAL,VALLEN, COM_NAME(1:4), 
     &           PAWC_SIZE, PAWC_LEN, RETFLAG ) 
            IF ( RETFLAG ) ERROR_FLAG = .TRUE.
      
          ELSE IF ( QUALINDX .EQ. GCBANK_QUAL ) THEN
C
C           Validate the GCBANK size
C
            COM_NAME = 'GCBANK'
            CALL PBD_GET_COM( QUALVAL,VALLEN, COM_NAME(1:6), 
     &           GCBANK_SIZE, GCBANK_LEN, RETFLAG ) 
            IF ( RETFLAG ) ERROR_FLAG = .TRUE.
C
C         Process PRODID, VERSION, PASS qualifiers
C
          ELSE IF ( QUALINDX .EQ. PRODID_QUAL ) THEN
C
C           Store the production id in common data area
C
            PRODID = QUALVAL
            PRODID_LEN = VALLEN

          ELSE IF ( QUALINDX .EQ. VERSION_QUAL ) THEN
C
C           Store the production version number in common data area
C
            VERSION = QUALVAL
            VERSION_LEN = VALLEN

          ELSE IF ( QUALINDX .EQ. PASS_QUAL ) THEN
C
C           Store the production pass number in common data area
C
            PASS = QUALVAL
            PASS_LEN = VALLEN

          ELSE IF ( QUALINDX .EQ. PACK_QUAL ) THEN
C
C          Process package names and Store them in common data area
C
           IF ( VALLEN .EQ. 0 ) THEN   ! Missing package name, send error
             ERROR_FLAG = .TRUE.
             CALL PBD_MSG ( ERROR_BUF(10) )
           
           ELSE
             
             TEMPINDX = INDEX ( QUALVAL(1:VALLEN), LEFTPAREN )
             IF ( TEMPINDX .LE. 0 ) THEN
C
C              single package name expected
C
               IF ( INDEX ( QUALVAL(1:VALLEN),RIGHTPAREN) .GT. 0
     &            .OR. INDEX ( QUALVAL(1:VALLEN),COMMA) .GT. 0) THEN
C
C                Invilid packange name list, send error message
C
                 ERROR_FLAG = .TRUE.
                 CALL PBD_MSG ( ERROR_BUF(5) )
               ELSE
                 LOC_PACK_NAME(1) = QUALVAL
                 TOT_INP_PACK = 1
                 LOC_PACK_LEN( TOT_INP_PACK ) = VALLEN
               END IF

             ELSE
C
C              Multiple package names, get the package list
C 
               TEMPINDX1 = INDEX ( QUALVAL(1:VALLEN),RIGHTPAREN)
               IF ( TEMPINDX1 .GT. 0 ) THEN
                
                TEMPSTR = QUALVAL( TEMPINDX+1:TEMPINDX1-1 )
                TEMPLEN = TEMPINDX1 - TEMPINDX
C
C               Extract each pacakge name from the package list
C
                TOT_INP_PACK = 1
                L = 0
C
C               Scan the package name field, extract individual package
C               name and store in common data area
C
                DO K = 1, TEMPLEN
                  IF ( TEMPSTR(K:K) .EQ. COMMA ) THEN
C
C                   Save the package name size
C
                    IF ( L .GT. 0 ) THEN
                      LOC_PACK_LEN( TOT_INP_PACK ) = L
                    ELSE
                      ERROR_FLAG = .TRUE.
                      CALL PBD_MSG ( ERROR_BUF(1) )
                    END IF
C
C                   A package name completed, update package counter
C
                    TOT_INP_PACK = TOT_INP_PACK + 1

                    L = 0               ! Reset the package name pointer

                  ELSE IF ( TEMPSTR(K:K) .NE. BLANK ) THEN

                    L = L + 1
                    LOC_PACK_NAME(TOT_INP_PACK)(L:L) = TEMPSTR(K:K)

                  END IF

                END DO
C
C               Store the last package name string size
C
                IF ( L .GT. 0 ) THEN
                  LOC_PACK_LEN( TOT_INP_PACK ) = L
                ELSE
                  ERROR_FLAG = .TRUE.
                  CALL PBD_MSG ( ERROR_BUF(1) )
                END IF

              ELSE
C
C               Syntax error in the package name list
C
                ERROR_FLAG = .TRUE.
                CALL PBD_MSG ( ERROR_BUF(2) )

              END IF

            END IF                 ! End of package name processing

           END IF                  ! End of package name length checking

          END IF                   ! End of a qualifier processing

        ELSE
C
C         Invalid qualifier - send error message
C
          MSG_BUF = ERROR_BUF(3)(1:25) // INPQUAL(1:QUALLEN)
          ERROR_FLAG = .TRUE.
          CALL PBD_MSG ( MSG_BUF )

        END IF                          ! Checking valid qualifier 

       END IF                           ! Qualifier exists

      END DO                            ! Processing of all input qualifiers
C
C     Set the global qualifier flags - COMPILE and HISTORY_BANK
C
      COMPILE = .NOT. QUALFLAG(16)     ! If NOCOMPILE option, reset
      HISTORY = QUALFLAG(17)           ! Set if HISTORY option entered
C
C     If fatal error, return.  Otherwise check if the required command
C     qualifiers are provided.
C
      IF ( ERROR_FLAG ) RETURN
C
C     Check if the frame name is provided.  If not, send error message
C
      IF ( FRAME_LEN .EQ. 0 ) THEN
        ERROR_FLAG = .TRUE.
        CALL PBD_MSG ( ERROR_BUF(7) )
        RETURN
      END IF
C
C     If neither the combined package name nor package name entered,
C     send error messages
C
      IF ( TOT_INP_PACK .EQ. 0 .AND. COMBINED_LEN .EQ. 0 ) THEN
        ERROR_FLAG = .TRUE.
        CALL PBD_MSG ( ERROR_BUF(4) )
        RETURN
      END IF
C
C     Check if the combined package name is provided for the multiple packages
C
      IF ( TOT_INP_PACK .GT. 1 .AND. COMBINED_LEN .EQ. 0 ) THEN
        ERROR_FLAG = .TRUE.
        CALL PBD_MSG ( ERROR_BUF(11) )
      END IF
        
      RETURN
      END

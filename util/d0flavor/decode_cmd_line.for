      SUBROUTINE DECODE_CMD_LINE
     & (QUALIFIER,NQUAL,VLDIM,PDIM,PLDIM,PRESENT,VALUE,LENV,NVLIST,
     &  PARAM,LENP,NPLIST,NPARAM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Extract parameters and qualifiers from
C-                          a command line WITHOUT the use of the CLI
C-                          procedures.
C-
C-   Inputs  : QUALIFIER(*)  [C*]  Array of qualifier names
C-
C-             NQUAL  [I]   Number of elements in QUALIFIER(*) array
C-                          - Dimension for 1st index of VALUE(*,*) array
C-             VLDIM  [I]   Max number of elements allowed in a value list
C-                          - Dimension for 2nd index of VALUE(*,*) array
C-             PDIM   [I]   Max number of parameters allowed
C-                          - Dimension for 1st index of PARAM(*,*) array
C-             PLDIM  [I]   Max number of elements allowed in a parameter list
C-                          - Dimension for 2nd index of PARAM(*,*) array
C-
C-   Outputs : PRESENT(*)  [L]   Array of logicals. TRUE if qualifier present,
C-                               FALSE otherwise
C-
C-
C-             VALUE(*,*)  [C*]  Array of qualifier values
C-             LENV(*,*)   [I]   Array of qualifier value lengths
C-             NVLIST(*)   [I]   Array of number of elements per qualifier value
C-                               - The index corresponds to the qualifier name
C-
C-             For the above two 2-D arrays, the first index corresponds to
C-             the qualifier name and the second to the Nth value list element.
C-             NQUAL is the max value for the first index and NVLIST(K) is
C-             the max value of the second index for a given first index with
C-             value K.
C-
C-
C-             PARAM(*,*)  [C*]  Array of parameters
C-             LENP(*,*)   [I]   Array of parameter lengths
C-             NPLIST(*)   [I]   Array of number of elements per parameter
C-                               - The index corresponds to the Nth parameter
C-             NPARAM      [I]   Number of parameters
C-
C-             For the above two 2-D arrays, the first index corresponds to the
C-             Kth parameter and the second to the Nth parameter list element.
C-             NPARAM is the max value for the first index and NPLIST(K) is
C-             the max value of the second index for a given first index with
C-             value K.
C-
C-   Controls:
C-
C-   Created   2-JUN-1992   Rich Mueller
C-                          - Started from the files in
C-                            DECODE_COMMAND_LINE.FOR and created similar
C-                            routines that do not use any CLI procudures
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER       NQUAL,VLDIM,PDIM,PLDIM
      CHARACTER*(*) PARAM(PDIM,PLDIM)
      INTEGER       LENP(PDIM,PLDIM)
      INTEGER       NPLIST(PDIM)
      INTEGER       NPARAM
      CHARACTER*(*) QUALIFIER(NQUAL)
      LOGICAL       PRESENT(NQUAL)
      CHARACTER*(*) VALUE(NQUAL,VLDIM)
      INTEGER       LENV(NQUAL,VLDIM)
      INTEGER       NVLIST(NQUAL)
C
      INTEGER I,J
      CHARACTER*1023 COMMAND,RAWCMD
      CHARACTER*1 CH
      INTEGER*2 RLEN
      INTEGER RAWLEN,LENGTH
      INTEGER STATUS,FLAGS
      LOGICAL INQUO,INPAR
C
C ****  Define run-time library entry points
C
      INTEGER  LIB$GET_FOREIGN
C
C----------------------------------------------------------------------
C
C&IF VAXVMS
      CH='/'
C&ELSE
C&      CH='-'
C&ENDIF
C
C ****  Get command line
C
      FLAGS=0
      STATUS = LIB$GET_FOREIGN(RAWCMD,'_OPTIONS: ',RLEN,FLAGS)
      CALL CLTOU(RAWCMD)
C
C ****  Parse command line
C
      IF( RLEN .GE. 0 ) THEN
        RAWLEN=RLEN
      ELSE
        RAWLEN=RLEN+65536
      ENDIF
      IF( RAWLEN .GT. 0 ) THEN
        COMMAND = ' '//RAWCMD(1:RAWLEN)
        RAWCMD = COMMAND
        RAWLEN=RAWLEN+1
      ELSE
        RAWCMD=' '
        RAWLEN=1
      ENDIF
C      PRINT*,'RAWCMD=',RAWCMD(1:RAWLEN)
C
      J=1
      LENGTH=RAWLEN
      COMMAND=' '
      INQUO=.FALSE.
      DO I=1,RAWLEN
        IF( RAWCMD(I:I) .EQ. '"' ) THEN
          INQUO= .NOT. INQUO
        ENDIF
C
C  Remove single spaces before and after ','s , before and after '='s ,
C  before '('s , after ')'s , and after CHs (/ or -)s, provided none of
C  these symbols are within double quotes (").
C  Command line processor replaces multiple spaces outside of
C  quotes by single spaces.
        IF( RAWCMD(I:I) .EQ. ' ' .AND. .NOT. INQUO .AND.
     &      (RAWCMD(I+1:I+1).EQ.','
     &      .OR.RAWCMD(I+1:I+1).EQ.'='.OR.RAWCMD(I+1:I+1).EQ.')'
     &      .OR.(I.GT.1.AND.(RAWCMD(I-1:I-1).EQ.CH
     &      .OR.RAWCMD(I-1:I-1).EQ.','.OR.RAWCMD(I-1:I-1).EQ.'='
     &      .OR.RAWCMD(I-1:I-1).EQ.'('))) ) THEN
          LENGTH=LENGTH-1
C&IF VAXVMS
C   Add a space before a '/' if one is not already there and the '/' is not
C   within double quotes.
        ELSEIF( RAWCMD(I:I) .EQ. '/' .AND. RAWCMD(I-1:I-1) .NE. ' '
     &          .AND. .NOT. INQUO ) THEN
          COMMAND(J:J+1)=' /'
          J=J+2
          LENGTH=LENGTH+1
C&ELSE
C&ENDIF
        ELSE
          COMMAND(J:J)=RAWCMD(I:I)
          J=J+1
        ENDIF
      ENDDO
      IF( INQUO ) THEN
        PRINT*,'ERROR!  Invalid command line -- Quote not closed'
        CALL EXIT(2)
      ENDIF
C
C  Check for the validity of parentheses usage outside of quotes.
C  A set of parentheses is only allowed around a list of qualifier values.
      INPAR=.FALSE.
      DO I=1,LENGTH
        IF( COMMAND(I:I) .EQ. '"' ) THEN
          INQUO= .NOT. INQUO
        ELSEIF( COMMAND(I:I) .EQ. '(' .AND. .NOT. INQUO ) THEN
          IF( COMMAND(I-1:I-1) .EQ. '=' .AND. .NOT. INPAR) THEN
            INPAR= .TRUE.
          ELSE
            PRINT*,'ERROR!  Invalid command line -- Incorrect'
     &              // ' parentheses usage'
            CALL EXIT(2)
          ENDIF
        ELSEIF( COMMAND(I:I) .EQ. ')' .AND. .NOT. INQUO ) THEN
          IF( INPAR .AND. COMMAND(I+1:I+1) .EQ. ' ' ) THEN
            INPAR= .FALSE.
          ELSE
            PRINT*,'ERROR!  Invalid command line -- Incorrect'
     &              // ' parentheses usage'
            CALL EXIT(2)
          ENDIF
        ENDIF
      ENDDO
      IF( INPAR ) THEN
        PRINT*,'ERROR!  Invalid command line -- Parenthesis not closed'
        CALL EXIT(2)
      ENDIF
C
C      PRINT*,'COMMAND=',COMMAND(1:LENGTH)
C
      CALL DECODE_QUALS(COMMAND,LENGTH,QUALIFIER,NQUAL,VLDIM,
     &                  PRESENT,VALUE,LENV,NVLIST)
      CALL GET_PARAMS(COMMAND,LENGTH,PDIM,PLDIM,
     &                PARAM,LENP,NPLIST,NPARAM)
C
  999 RETURN
      END
C
      SUBROUTINE DECODE_QUALS (COMMAND,LENGTH,QUALIFIER,NQUAL,
     &                         VLDIM,PRESENT,VALUE,LENV,NVLIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  For an input array of qualifier names, this
C-   procedure returns an output array of logicals that indicate which
C-   qualifiers are present in the command line.  Also, this procedure
C-   returns the values of the qualifiers that have any.
C-
C-   Inputs  : COMMAND       [C*]  Command line
C-             LENGTH        [I]   Command line length
C-             QUALIFIER(*)  [C*]  Array of qualifier names
C-
C-             NQUAL  [I]   Number of elements in QUALIFIER(*) array
C-                          - Dimension for 1st index of VALUE(*,*) array
C-             VLDIM  [I]   Max number of elements allowed in a value list
C-                          - Dimension for 2nd index of VALUE(*,*) array
C-
C-   Outputs : PRESENT  [L*]  Array of logicals. TRUE if qualifier present,
C-                            FALSE otherwise
C-
C-
C-             VALUE(*,*)  [C*]  Array of qualifier values
C-             LENV(*,*)   [I]   Array of qualifier value lengths
C-             NVLIST(*)   [I]   Array of number of elements per qualifier value
C-                               - The index corresponds to the qualifier name
C-
C-             For the above two 2-D arrays, the first index corresponds to
C-             the qualifier name and the second to the Nth value list element.
C-             NQUAL is the max value for the first index and NVLIST(K) is
C-             the max value of the second index for a given first index with
C-             value K.
C-
C-   Controls:
C-
C-   Created  2-JUN-1992   Rich Mueller
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*)  COMMAND
      INTEGER        LENGTH
      INTEGER        NQUAL,VLDIM
      CHARACTER*(*)  QUALIFIER(NQUAL)
      LOGICAL        PRESENT(NQUAL)
      CHARACTER*(*)  VALUE(NQUAL,VLDIM)
      INTEGER        LENV(NQUAL,VLDIM)
      INTEGER        NVLIST(NQUAL)
      INTEGER        LI,IQUAL,ST,ED
      CHARACTER*1    CH
      LOGICAL        INQUO,QVAL
      INTEGER        H,I,L,CNT,SPOS,NSPOS
      INTEGER        LENOCC
C
      INTEGER        LDIM
      PARAMETER      (LDIM=200)
      CHARACTER*1023 PLIST(LDIM)
      INTEGER        LLIST(LDIM)
      INTEGER        NLIST
C
C----------------------------------------------------------------------
C
C&IF VAXVMS
      CH='/'
C&ELSE
C&      CH='-'
C&ENDIF
C
      DO I=1,NQUAL
        PRESENT(I) = .FALSE.
        NVLIST(I)=0
      ENDDO
C
C  Find the starting position of a qualifier (' /' or ' -')
      SPOS=INDEX(COMMAND(1:LENGTH),' '//CH) + 1
C
      IF ( SPOS .GT. 1) THEN
        INQUO=.FALSE.
        QVAL=.FALSE.
        DO H=1,LENGTH+1
          IF( H .LE. LENGTH .AND. COMMAND(H:H) .EQ. '"' ) THEN
            IF( .NOT. INQUO .AND. H .GT. SPOS .AND. .NOT. QVAL ) THEN
              PRINT*,'ERROR!  Invalid command line -- quotes not'
     &               // ' allowed in qualifier names'
              CALL EXIT(2)
            ENDIF
C
C  If the present starting qualifier position (' /' or ' -') is found
C  within quotes, then find the starting position of the next qualifier.
            IF( INQUO .AND. H .GT. SPOS .AND. .NOT. QVAL ) THEN
              NSPOS=INDEX(COMMAND(H:LENGTH),' '//CH) + 1
              IF( NSPOS .EQ. 1 ) GOTO 999
              SPOS=NSPOS + H - 1
            ENDIF
C
            INQUO= .NOT. INQUO
          ENDIF
          IF( H .GT. SPOS .AND. .NOT. INQUO .AND.
     &        (H .EQ. LENGTH+1 .OR. COMMAND(H:H) .EQ. '='
     &                         .OR. COMMAND(H:H) .EQ. ' ') ) THEN
            IF( SPOS+1 .GT. H-1 ) THEN
              IF( QVAL ) THEN
                PRINT*,'ERROR!  Invalid command line -- Qualifier'
     &                 // ' missing value'
              ELSE
                PRINT*,'ERROR!  Invalid command line -- missing'
     &                 // ' qualifier name'
              ENDIF
              CALL EXIT(2)
            ENDIF
C
            IF( QVAL ) THEN
C
C  Find the present qualifier's value list
C
              IF( COMMAND(H:H) .EQ. '=' ) THEN
                PRINT*,'ERROR!  Invalid qualifier value ',
     &                 COMMAND(SPOS+1:H)
                CALL EXIT(2)
              ENDIF
C
C  ST and ED are the starting and ending positions of a qualifier value list
              ST=SPOS+1
              ED=H-1
C  Remove the parentheses around a qualifier value list, if they are present
              IF( COMMAND(ST:ST) .EQ. '('
     &            .AND. COMMAND(ED:ED) .EQ. ')' ) THEN
                ST=ST+1
                ED=ED-1
                IF( ST .GT. ED ) THEN
                  PRINT*,'ERROR!  Invalid command line -- Qualifier'
     &                   // ' missing value'
                  CALL EXIT(2)
                ENDIF
              ELSEIF( COMMAND(ST:ST) .EQ. '(' ) THEN
C  Parentheses contain more than just the value list
                PRINT*,'ERROR!  Invalid command line -- Incorrect'
     &                  // ' parentheses usage'
                CALL EXIT(2)
              ENDIF
C
              CALL DECODE_LIST( COMMAND(ST:ED),ED-ST+1,LDIM,
     &                          PLIST,LLIST,NLIST )
              NLIST=MIN(NLIST,VLDIM)
              DO LI=1,NLIST
                VALUE(IQUAL,LI)=PLIST(LI)
                LENV(IQUAL,LI)=LLIST(LI)
              ENDDO
              NVLIST(IQUAL)=NLIST
              QVAL=.FALSE.
            ELSE
C
C  Determine if the present qualifier is valid
C
              CNT=0
              DO I = 1, NQUAL
C  Use Cernlib function LENOCC to return length of qualifier string
                L=LENOCC(QUALIFIER(I))
                IF ( INDEX(QUALIFIER(I)(1:L),
     &               COMMAND(SPOS+1:H-1)) .EQ. 1 ) THEN
                  PRESENT(I) = .TRUE.
                  IQUAL=I
                  CNT=CNT+1
                ENDIF
              ENDDO
              IF( CNT .EQ. 0 ) THEN
                PRINT*,'ERROR!  Invalid qualifier ',
     &                 COMMAND(SPOS+1:H-1)
                CALL EXIT(2)
              ELSEIF( CNT .GT. 1 ) THEN
                PRINT*,'ERROR!  Ambiguous qualifier ',
     &              COMMAND(SPOS+1:H-1), '  - Supply more characters'
                CALL EXIT(2)
              ENDIF
            ENDIF
            IF( H .LE. LENGTH .AND. COMMAND(H:H) .EQ. '=' ) THEN
              IF( H .EQ. LENGTH .OR. COMMAND(H+1:H+1) .EQ. CH ) THEN
                PRINT*,'ERROR!  Invalid command line -- Qualifier'
     &                 // ' missing value'
                CALL EXIT(2)
              ENDIF
C  The qualifier has a value
              SPOS=H
              QVAL=.TRUE.
            ELSEIF( H .LE. LENGTH ) THEN
C  The qualifier does not have a value, so find the starting
C  position (' /' or ' -') of the next qualifier.
              NSPOS=INDEX(COMMAND(H:LENGTH),' '//CH) + 1
              IF( NSPOS .EQ. 1 ) GOTO 999
              SPOS=NSPOS + H - 1
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
  999 RETURN
      END
C
      SUBROUTINE GET_PARAMS(COMMAND,LENGTH,PDIM,PLDIM,PARAM,LENP,
     &                      NPLIST,NPARAM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Return parameter values from the
C-                          command line.
C-
C-   Inputs  : COMMAND  [C*]  Command line
C-             LENGTH   [I]   Command line length
C-
C-             PDIM   [I]   Max number of parameters allowed
C-                          - Dimension for 1st index of PARAM(*,*) array
C-             PLDIM  [I]   Max number of elements allowed in a parameter list
C-                          - Dimension for 2nd index of PARAM(*,*) array
C-
C-   Outputs : PARAM(*,*)  [C*]  Array of parameters
C-             LENP(*,*)   [I]   Array of parameter lengths
C-             NPLIST(*)   [I]   Array of number of elements per parameter
C-                               - The index corresponds to the Nth parameter
C-             NPARAM      [I]   Number of parameters
C-
C-             For the above two 2-D arrays, the first index corresponds to the
C-             Kth parameter and the second to the Nth parameter list element.
C-             NPARAM is the max value for the first index and NPLIST(K) is
C-             the max value of the second index for a given first index with
C-             value K.
C-
C-   Controls:
C-
C-   Created  13-MAY-1992   Rich Mueller
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*)  COMMAND
      INTEGER        LENGTH
      INTEGER        PDIM,PLDIM
      CHARACTER*(*)  PARAM(PDIM,PLDIM)
      INTEGER        LENP(PDIM,PLDIM)
      INTEGER        NPLIST(PDIM)
      CHARACTER*1    CH
      INTEGER        NPARAM,SPOS,NSPOS,LI,H
      LOGICAL        INQUO,EMPTY,QUOT,LOOKING
C
      INTEGER        LDIM
      PARAMETER      (LDIM=200)
      CHARACTER*1023 PLIST(LDIM)
      INTEGER        LLIST(LDIM)
      INTEGER        NLIST
C
C&IF VAXVMS
      CH='/'
C&ELSE
C&      CH='-'
C&ENDIF
C
      NPARAM=0
      SPOS=1
      LOOKING=.TRUE.
C  Find the starting position (SPOS) of the first parameter
      DO WHILE( LOOKING )
        NSPOS=INDEX(COMMAND(SPOS:LENGTH),' ')
        SPOS=NSPOS + SPOS - 1
        IF( NSPOS .EQ. 0 .OR. COMMAND(SPOS+1:SPOS+1) .NE. CH ) THEN
          LOOKING=.FALSE.
        ELSE
          SPOS=SPOS+1
        ENDIF
      ENDDO
C
      IF ( NSPOS .GT. 0) THEN
        INQUO=.FALSE.
        EMPTY=.FALSE.
        QUOT=.FALSE.
        DO H=1,LENGTH+1
          IF( H .LE. LENGTH .AND. COMMAND(H:H) .EQ. '"' ) THEN
            IF( EMPTY ) THEN
              IF( COMMAND(H+1:H+1) .NE. '"' ) THEN
C  Empty string (nothing between quotes)
                INQUO= .FALSE.
              ELSE
C  String starts with a quote ("""...")
C  Two consecutive quotes within quotes represent one quote in the string
                QUOT=.TRUE.
              ENDIF
              EMPTY=.FALSE.
            ELSEIF( COMMAND(H+1:H+1) .EQ. '"' .AND. .NOT. INQUO ) THEN
C  String either empty ("") or starts with a quote ("""...")
C  Two consecutive quotes within quotes represent one quote in the string
              EMPTY= .TRUE.
              INQUO= .TRUE.
            ELSEIF( COMMAND(H+1:H+1) .NE. '"' ) THEN
              IF( .NOT. QUOT ) THEN
C  Quote either opens or closes a string
                INQUO= .NOT. INQUO
              ELSE
C  Quote is the second of two consecutive quotes in a string that represent one
                QUOT=.FALSE.
              ENDIF
            ELSEIF( COMMAND(H+1:H+1) .EQ. '"' .AND. INQUO ) THEN
C  Quote is the first of two consecutive quotes in a string that represent one
              QUOT=.TRUE.
            ENDIF
          ENDIF
          IF( H .GT. SPOS .AND. .NOT. INQUO .AND. SPOS .NE. LENGTH
     &        .AND. (H .EQ. LENGTH+1 .OR. COMMAND(H:H) .EQ. ' ' ) ) THEN
            CALL DECODE_LIST( COMMAND(SPOS+1:H-1),H-SPOS-1,LDIM,
     &                        PLIST,LLIST,NLIST )
            NLIST=MIN(NLIST,PLDIM)
            NPARAM=NPARAM+1
            DO LI=1,NLIST
              PARAM(NPARAM,LI)=PLIST(LI)
              LENP(NPARAM,LI)=LLIST(LI)
            ENDDO
            NPLIST(NPARAM)=NLIST
            IF( NPARAM .EQ. PDIM ) GOTO 999
C
            IF( H .LE. LENGTH ) THEN
              SPOS=H
              LOOKING=.TRUE.
C  Find the starting position (SPOS) of the next parameter
              DO WHILE( LOOKING )
                NSPOS=INDEX(COMMAND(SPOS:LENGTH),' ')
                SPOS=NSPOS + SPOS - 1
                IF( NSPOS .EQ. 0 .OR.
     &              COMMAND(SPOS+1:SPOS+1) .NE. CH ) THEN
                  LOOKING=.FALSE.
                ELSE
                  SPOS=SPOS+1
                ENDIF
                IF( NSPOS .EQ. 0 ) GOTO 999
              ENDDO
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
  999 RETURN
      END
C
      SUBROUTINE DECODE_LIST(CMDLIST,LLEN,LDIM,PLIST,LLIST,NLIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Extract elements from a
C-                          parameter (or value) list.
C-
C-   Inputs  : CMDLIST  [C*]  Command line list
C-             LLEN     [I]   Command line list length
C-             LDIM     [I]   Max number of elements allowed
C-                            for PLIST(*) and LLIST(*)
C-
C-   Outputs : PLIST(*)  [C*]  Array of parameter list values
C-             LLIST(*)  [I*]  Array of parameter value lengths
C-             NLIST     [I]   Array of number of parameter list elements
C-   Controls:
C-
C-   Created  2-JUN-1992   Rich Mueller
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*)  CMDLIST
      INTEGER        LLEN
      INTEGER        LDIM
      CHARACTER*(*)  PLIST(LDIM)
      INTEGER        LLIST(LDIM)
      INTEGER        NLIST,I,J,K,ST,LI
      CHARACTER*1023 TEMP
      LOGICAL        INQUO
C
      LI=0
      ST=1
      INQUO=.FALSE.
      DO I=1,LLEN+1
        IF( I .LE. LLEN .AND. CMDLIST(I:I) .EQ. '"' ) THEN
          INQUO= .NOT. INQUO
        ENDIF
C
C  Separate parameter elements from the list and store in array PLIST
        IF( I .EQ. LLEN+1 .OR. (CMDLIST(I:I) .EQ. ','
     &      .AND. .NOT. INQUO) ) THEN
          LI=LI+1
          PLIST(LI)=CMDLIST(ST:I-1)
          IF( INDEX(PLIST(LI),'"') .EQ. 0 ) THEN
            LLIST(LI)=I-ST
          ELSE
C  Remove opening and closing quotes from parameter list element,
C  and replace two consecutive quotes within quotes with one quote.
            K=0
            TEMP=' '
            DO J=1,I-ST
              IF( PLIST(LI)(J:J) .EQ. '"' ) THEN
                IF( INQUO .AND.
     &              PLIST(LI)(J+1:J+1) .EQ. '"' ) THEN
C  The first of two consecutive quotes within quotes found.
C  Keep it and set INQUO flag so that the second quote will be
C  removed the the next time through the loop.
                  K=K+1
                  TEMP(K:K)=PLIST(LI)(J:J)
                  INQUO= .FALSE.
                ELSEIF( .NOT. INQUO .OR.
     &                  PLIST(LI)(J+1:J+1) .NE. '"' ) THEN
C  Either opening or closing quote found, or the second of two consecutive
C  quotes wihin quotes encountered.  Remove it by not writing it to TEMP.
                  INQUO= .NOT. INQUO
                ENDIF
              ELSE
                K=K+1
                TEMP(K:K)=PLIST(LI)(J:J)
              ENDIF
            ENDDO
            PLIST(LI)=TEMP
            LLIST(LI)=K
          ENDIF
          IF( LI .EQ. LDIM ) GOTO 900
C  Set ST to be the start of the next parameter element in the list.
          ST=I+1
        ENDIF
      ENDDO
  900 CONTINUE
      NLIST=LI
C
  999 RETURN
      END

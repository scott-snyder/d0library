      SUBROUTINE PUEXEC(PIXIE_PACKAGE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Activate specified PIXIE package.
C-
C-   Inputs  : PIXIE_PACKAGE  [C*]    Package to be activated.
C-   Outputs : None
C-   Controls: None
C-
C-   Created   8-SEP-1990   Harrison B. Prosper
C-   Updated  19-SEP-1990   Harrison B. Prosper, Lupe Howell
C-   Updated  27-SEP-1990   Harrison B. Prosper
C-   Updated  28-NOV-1990   Harrison B. Prosper  
C-      Use OLD_PIXIE_PACKAGE in entry point 
C-   Updated  23-MAR-2004   compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PIXIE_PACKAGE
      INTEGER I,J,K,L,II,JJ,KK,LL
      LOGICAL PBD_SET_FLAG
      logical ltmp
C
      CHARACTER*(*) ACTIVE_PACKAGE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXCOMK.INC'
C----------------------------------------------------------------------
      CHARACTER*32 OLD_PIXIE_PACKAGE
      LOGICAL FIRST
      SAVE FIRST,OLD_PIXIE_PACKAGE
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF ( NPACKAGE .GT. 0 ) THEN
C
C ****  Turn OFF all packages 
C
        IF ( FIRST ) THEN
          FIRST = .FALSE.
C
          DO I = 1, NPACKAGE
            CALL WORD(PACKAGE(I),J,K,L)
            IF ( .NOT. PBD_SET_FLAG(PACKAGE(I)(J:K),.FALSE.) ) THEN
              CALL ERRMSG('PIXIE','PUEXEC',
     &        'Unable to find package '//PACKAGE(I)(J:K),'S')
              CALL INTMSG
     &        (' corresponding to RCP bank PX_'//PACKAGE(I)(J:K)//
     &         '_RCP.')
              CALL INTMSG
     &        (' Check the name of your PIXIE package.')
            ENDIF
          ENDDO
C
C ****  Turn on specified package
C
          CALL WORD(PIXIE_PACKAGE,J,K,L)
          ltmp = PBD_SET_FLAG(PIXIE_PACKAGE(1:L),.TRUE.)
          OLD_PIXIE_PACKAGE = PIXIE_PACKAGE(1:L)
        ENDIF
C
C ****  Turn ON specified package and turn OFF previous package
C
        CALL WORD(PIXIE_PACKAGE,J,K,L)
        IF ( PIXIE_PACKAGE(1:L) .NE. OLD_PIXIE_PACKAGE(1:L)  ) THEN
          ltmp = PBD_SET_FLAG(PIXIE_PACKAGE(1:L),.TRUE.)
          CALL WORD(OLD_PIXIE_PACKAGE,JJ,KK,LL)
          ltmp = PBD_SET_FLAG(OLD_PIXIE_PACKAGE(1:LL),.FALSE.)
        ENDIF
        OLD_PIXIE_PACKAGE = PIXIE_PACKAGE(1:L)
      ENDIF
  999 RETURN
      ENTRY PU_ACTIVE_PACKAGE(ACTIVE_PACKAGE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Retunrs the name of the active package
C-
C-   Inputs  : None
C-   
C-   Outputs : ACTIVE_PACKAGE [C*]: Name of the active pacakge
C-
C-   Created  16-NOV-1990   Lupe Howell
C-
C----------------------------------------------------------------------
      ACTIVE_PACKAGE = OLD_PIXIE_PACKAGE
      RETURN
      END

      SUBROUTINE PXMODIFY_ACTIONS(RCPFILE,SCRENAME,PARNAME,
     &  OCURRENCE,VALUE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Modifies a parameter (PARNAME) inside the
C-   *ACTION_RCP file corresponding to RCPFILE.  The parameter should
C-   be inside an array of the form:
C-            \ARRAY  ARRAY_NAME
C-              'Name Submenu'
C-              'NAME SUBMENU'
C-              0
C-              'Parameter name'  Parameter Value  'Remark'
C-                    :                   :              :
C-            \END
C-
C-   Inputs  : RCPFILE [C* ]: RCP file name PX_XX_RCP
C-             SCRENAME[C* ]: Screen name 
C-             PARNAME [C* ]: Name of the parameter to be modified
C-             OCCURENCE[I ]: Number of ocurrence to be modify
C-             VALUE    [R ]: New value
C-
C-   Outputs : None
C-
C-   Created  14-MAY-1991   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCPFILE
      CHARACTER*(*) SCRENAME
      CHARACTER*(*) PARNAME
      INTEGER OCURRENCE,IER
      REAL    VALUE
C
      INTEGER I,II,J,K,JJ,PTR,TYPE,LPARAM,LENPAR,PAR_PTR,IMENU
      INTEGER IVAL,LCVAL,LREMARK,ISUBM,IACTION
      LOGICAL FOUND,ACTION_FOUND,SKIP,EZERROR
      CHARACTER*80 PARAM,CVAL,REMARK
      CHARACTER*32 CURRENT_ACTION
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXBUILDCOM.INC'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
C
C ****  Get menu, submenu and action indexes
C
      CALL PXBUILD_INDEX(RCPFILE,SCRENAME,IMENU,ISUBM,IACTION,IER)
      CALL SWORDS(PARNAME,I,J,LENPAR)
C
C ****  If indexes found select RCP action file to modify its parameters
C
      IF ( IER .EQ. 0 ) THEN
        CURRENT_ACTION = ACTION_NAME(IMENU,ISUBM,IACTION)
        CALL WORD(MENU_NAME(IMENU),I,J,K)
        CALL EZPICK(MENU_NAME(IMENU)(1:K)//'_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL INTMSG(' Could not find file '//MENU_NAME(IMENU)(1:K)
     &        //'_RCP')
          GOTO 999
        ENDIF
        SKIP = .TRUE.
        PTR = 1
        IER = 0
        FOUND = .FALSE.
C
C ****  Walk through RCP action file skipping the menu names
C
        DO WHILE (( IER .EQ. 0 ) .AND. ( .NOT. FOUND ))
          IF ( SKIP ) THEN            ! Skip Menu Definition
            CALL EZGET_NEXT_VALUE_TYPE(CURRENT_ACTION,
     &                  IVAL,CVAL,TYPE,LCVAL,IER,PTR)
            SKIP = TYPE .GT. VTCHR    ! Character string
          ELSE
C
C ****  Get triplet (Param-name, value, remark)
C
            PAR_PTR = PTR
            CALL EZGET_NEXT_VALUE_TYPE(CURRENT_ACTION,
     &            II,PARAM,JJ,LPARAM,IER,PTR)
            CALL EZGET_NEXT_VALUE_TYPE(CURRENT_ACTION,
     &            IVAL,CVAL,TYPE,LCVAL,IER,PTR)
            CALL EZGET_NEXT_VALUE_TYPE(CURRENT_ACTION,
     &            II,REMARK,JJ,LREMARK,IER,PTR)
C
C ****  Modify parameter if it match
C
            IF ( PARNAME(1:LENPAR) .EQ. PARAM(1:LENPAR) ) THEN
              CALL EZ_SET_ELEMENT
     &        (CURRENT_ACTION,PARNAME(1:LENPAR),PAR_PTR,
     &         OCURRENCE,VALUE,IER)
              IF ( IER .NE. 0 ) THEN
                CALL INTMSG(' Could not modify '//
     &            PARNAME(1:LENPAR))
                GOTO 999
              ELSE
                FOUND = .TRUE.
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        CALL EZRSET
      ELSE
        CALL INTMSG(' Could not find action routine for '//
     &         RCPFILE)
      ENDIF
  999 RETURN
      END

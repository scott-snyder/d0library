      SUBROUTINE GUIGET(NNMENU,NNCOMD,NNPAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Switching routine for 2d level menus
C-                              Called  by ====> GINCOM
C-              Menu 6   Help
C-              Menu 7   D0
C-              Menu 8   CEN
C-              Menu 9   CAL
C-              Menu 10  MUO
C-              Menu 11  LV0
C-              Menu 12  TBM
C-
C-   Inputs  : NNMENU    Menu number
C-             NNCOMD    Command within menu
C-             NNPAR     Number of parameters passed
C-   Outputs :
C-   Controls:
C-
C-   Created   1-MAY-1987   Steve Linn
C-   Updated   6-DEC-1988   A.M.Jonckheere  Added LV0 and made into standard
C-                              form.
C-   Updated   6-JUN-1989   Harrison B. Prosper
C-   Added hooks LUHELP, LUMENU. Added D0COM.INC.
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NNMENU,NNCOMD,NNPAR
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:D0COM.INC'
C----------------------------------------------------------------------
C
      NMENU = NNMENU                    ! Transfer to D0COM
      NCOMD = NNCOMD
      NPAR  = NNPAR
C
      IF ( NMENU .GT. IDMAXI ) GOTO 999
      IF ( NMENU .EQ. IDHELP ) THEN
C
        IF ( NCOMD .GT. 3 ) GOTO 999
        GOTO ( 100,200,300 ) NCOMD
C
  100   CONTINUE
        WRITE(LOUT,*)'=====>ZCEDEX  Menues:'
        WRITE(LOUT,*)' *1 - System  '
        WRITE(LOUT,*)' *2 - Editor '
        WRITE(LOUT,*)' *3 - Functions '
        WRITE(LOUT,*)' *4 - Functions '
        WRITE(LOUT,*)' *5 - Vector '
        WRITE(LOUT,*)' *6 - Vector '
        WRITE(LOUT,*)' *7 - Macro  '
        WRITE(LOUT,*)' *8 - Macro '
        GOTO 999
C
  200   CONTINUE
        WRITE(LOUT,*)'=====>GEANT3  Menues:'
        WRITE(LOUT,*)'  1 - Drawing'
        WRITE(LOUT,*)'  2 - Graphics'
        WRITE(LOUT,*)'  3 - Geometry'
        WRITE(LOUT,*)'  4 - General'
        WRITE(LOUT,*)'  5 - RZEBRA'
        GOTO 999
C
  300   CONTINUE
        WRITE(LOUT,*)'=====>D0 Menues: '
        WRITE(LOUT,*)'  6 - D0 Help'
C
C ************************
C ****  USER HOOK LUHELP
C ************************
        CALL LUHELP
C
      ELSE
C
C ************************
C ****  USER HOOK LUMENU
C ************************
        CALL LUMENU
      ENDIF
C
  999 RETURN
      END

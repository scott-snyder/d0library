      SUBROUTINE EAS_ROTATE_3D(NSEG,NREAL,RSEGTYP,RLIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Handle 3D Rotation on EVANS & SUTHERLAND
C-
C-   Input   : NSEG       [I]: Number of retained segements
C-             RSEGTYP [I(*)]: Array with types of retained segments
C_
C-   Outputs : None
C-
C-   Warning!  : DON'T call this routine between JROPEN and JRCLOS.
C-
C-   Modified  28-AUG-1992   N. Oshima
C-                Save updated viewing params by rotating and disconnect
C-                hardware dials.
C-   Modified  14-MAY-1992   N. Oshima Fixed 'CALL ECONNS' problem.
C-   Created  13-MAY-1992   Lupe Howell  and Harrison B. Prosper
C-                          Based on PUROTATE by Nobu Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C-
      INCLUDE 'D0$INC:DEVSTS.INC'
C----------------------------------------------------------------------
      INTEGER NSEG,NREAL,RSEGTYP(*)
      REAL    RLIST(*)
C----------------------------------------------------------------------
      INTEGER I,ISEG,ILISTS(64)
      INTEGER IRESP,IR,OLDNSTR
      INTEGER NDUMM,NSTR,NSTRT,INSTR,NSTRE
      INTEGER LISSTN(512),OLDLIS(512)
      REAL    RDUMM(10)
C-
      INTEGER ICHAR
      INTEGER ISTAT,LIB$WAIT
      REAL XV,YV, XW,YW,ZW
      CHARACTER*80 PCOMMAND
      LOGICAL  HDISCO
      EXTERNAL ERRHND
C-
      DATA NDUMM,RDUMM  /10, 1.0E5, 9*0./
      DATA HDISCO /.TRUE./
C----------------------------------------------------------------------
C-
C---   ****************************************
C---   * Rotation with the Evans & Sutherland *
C---   ****************************************
C-
C--- Setup all retain segments
C-
      IF ( HDISCO ) THEN
        HDISCO = .FALSE.
        CALL VZERO(ILISTS,64)
C--- To setup the transfomation network.
C-
        CALL JIQDEV(1,17,IRESP)
        IF(IRESP.NE.1) THEN
          CALL INTMSG(' THIS DEVICE DOES NOT HAVE 3D CAPABILITY')
          GOTO 999
        END IF
        CALL JDD3D(.TRUE.)
        CALL ECONNT
C-
C--- Setup 3D
C-
        ISEG = 0
        DO I=1,NSEG
          IF (RSEGTYP(I) .EQ. -1) THEN
            ISEG = ISEG + 1
            ILISTS(ISEG) = I
          ENDIF
        ENDDO
C-
C--- To connect the segments to the transformation network.
        NSTR = 0
C-
        DO I=1,NSEG
          NSTR = NSTR+1
          LISSTN(NSTR) = ILISTS(I)
          OLDLIS(NSTR) = LISSTN(NSTR)
        ENDDO
        OLDNSTR = NSTR
C-
        NSTRT=NSTR/10+1
        DO 200 IR=1,NSTRT
          INSTR =1 + (IR-1)*10
          IF(IR.EQ.NSTRT) THEN
            NSTRE=10 + NSTR- 10*NSTRT
          ELSE
            NSTRE=10
          END IF
C-
C---  Connect 3D
C-
          RDUMM(10) = RLIST(21)   ! set coarse scale dial factor
          CALL ECONNS(NSTRE,LISSTN(INSTR),NDUMM,RDUMM)
  200   CONTINUE
      ENDIF
C-
C--- Waiting for user picking which can be a trigger of disconnect dials
C-
      CALL UHTOC(RLIST,4,PCOMMAND,80)
      CALL VZERO(RLIST(1),6)
C--- ECHLV = -2 is only for picking with Hardware Rotation.
C-
      CALL JLOCAT( 1, 1, -2, ICHAR, XV, YV )
C-
C--- Check EXIT( Disconnect 3D ) or NOT...
C--- If NOT, Update NORML and UPVEC vector and disconnect 
C--- Hardware Dials.
C-
      IF (ICHAR .EQ. 8) THEN      ! EXIT
        NSEG = 0
        GO TO 999
      ELSEIF (ICHAR .EQ. 600) THEN
        CALL EROTVC_RESET
      ELSE
        CALL EROTVC(PCOMMAND)
      ENDIF
C-
      LASTCO = .FALSE.
C-
      CALL JCONVW( XV, YV, XW, YW,ZW )
C-
C--- Return Picking & Rotate flags
C-
      NSEG = 2
      RSEGTYP(1) = 1  ! Rotate
      RSEGTYP(2) = 4  ! Pick
C-
      RETURN
C-
C...  ENTRY HDIAL_OFF
      ENTRY HDIAL_OFF
C-
C--- Clear Hardware Connection and set HDISCO = .TRUE.
C--- This is called by EAS_JCLEAR ( Nobu. 03-NOV-1992 )
C-
      IF ( .NOT. HDISCO ) THEN
        HDISCO = .TRUE.
        LASTCO = .TRUE.
C-
C--- Clear Hardware Connection
        NSTRT = OLDNSTR/10 + 1
        DO 100 IR=1,NSTRT
          INSTR =1 + (IR-1)*10
          IF(IR .EQ. NSTRT) THEN
            NSTRE = 10 + NSTR - 10*NSTRT
          ELSE
            NSTRE = 10
          END IF
C-
C---  DisConnect 3D
C-
          CALL EDISCS (NSTRE,OLDLIS(INSTR))
  100   CONTINUE
C-
        CALL EDISCT
C---
        CALL JDEVOF(1)
        CALL JDEND(1)
        CALL JEND
C-
        ISTAT = LIB$WAIT(5.)
C-
        CALL JBEGIN
        CALL JDINIT(1)
        CALL JDEVON(1)
      ENDIF
C---
C-
  999 RETURN
      END

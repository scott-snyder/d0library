      SUBROUTINE DBARC_INSERT(PATH,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : PATH - Data base tree structure
C-   Outputs : IOK   - .TRUE. if ok
C-   Controls: 
C-
C-   Created  23-MAR-1991   S. Abachi (made by modifying dbclb_insert)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CALIB.DEF'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:LKCALIB.INC'
      INCLUDE 'D0$INC:DBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C
      INTEGER LBANK
      INTEGER KEY(NKYS),KEYO(NKYS),KEYN(NKYS),KEYC(NKYS)
      INTEGER I,LD,LK,LDAT,LKEY
      INTEGER       INPID               ! Process ID
      CHARACTER*8   CHPID
C
      CHARACTER*(*) PATH
      LOGICAL IOK
      EQUIVALENCE (CALIB_LNK(1),LBANK)
C
C&IF  VAXVMS
      INCLUDE      '($JPIDEF)'
      INTEGER       ISTAT, LIB$GETJPI
C
C----------------------------------------------------------------------
C
      ISTAT = LIB$GETJPI (JPI$_PID,,,INPID,,)
      WRITE (CHPID,1000) INPID
 1000 FORMAT (Z8.8)
C&ELSE
C&                              ! change this for a specific machine
C&                              ! to get a unique identifier    
C&      CHPID = 'D0CALIB'
C&ENDIF
C
      CALL VZERO(KEY,NKYS)
      KEY(3) = IC(LBANK+6)              ! start validity
      KEY(4) = IC(LBANK+5)              ! end validity
      KEY(8) = IC(LBANK+9)              ! Crate Number
      KEY(11) = IC(LBANK+6)             ! Run number
C
      CALL RZCDIR(PATH,' ')
      CALL DBPKTS((IC(LBANK+7)/100 + MOD(IC(LBANK+7),100)*10000),
     &             IC(LBANK+8), KEY(9))
      IF (IQUEST(1).NE.0) THEN
        CALL ERRDB('DBPKTS:  Error packing date and time')
        IOK = .FALSE.
        GO TO 999
      ENDIF
C
      CALL DBENTR(PATH,LK,LD,IDVSTP,LBANK,NKYS,KEY,0,'R')
      IF (IQUEST(1).NE.0) THEN
        CALL ERRDB('DBENTR:  Error in entering data in data base')
        IOK = .FALSE.
        GO TO 998
      ELSE
        IOK = .TRUE.
        GOTO 999
      ENDIF
C
  998 CONTINUE
      CALL DBCLB_FINISH
C
  999 RETURN
      END

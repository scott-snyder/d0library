      PROGRAM DZERO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : MAIN program for D0GEANT
C-
C-   Created  ??            ??
C-   Updated  15-DEC-1988   A.M.Jonckheere  Moved LDDUM to here, and added
C-                              DIDUM (for DIGEANT)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL LNKINC
      INTEGER NEVENT(4)
      INTEGER PROCESS,ACP_PROCESS_NUMBER
      DATA NEVENT / 4*0 /
      DATA LNKINC/.TRUE./
C----------------------------------------------------------------------
C
      CALL ACP_INIT
      CALL ACP_UPDATE_USER_STATUS(4,NEVENT)
      CLOSE(5)
      OPEN(UNIT=5,FILE='STDIN',ACCESS='SEQUENTIAL',FORM='FORMATTED')
C&IF IBMAIX
C&      PROCESS = ACP_PROCESS_NUMBER(PROCESS)
C&      PROCESS = 10*PROCESS
C&      CALL SLEEP(%VAL(PROCESS))
C&ENDIF
      CALL ACP_SIGHOLD

      CALL D0MAIN   !MAIN ROUTINE
C
C  THE FOLLOWING CALL TO LDDUM IS A DUMMY CALL JUST TO GET GEANT ROUTINES
C  LOADED THAT ARE ONLY CALLED FROM GEANT AND RESIDE IN GEANT.OLB.  
C       THE CALL MUST NEVER ACTUALLY BE EXECUTED.......
      IF(.NOT.LNKINC) CALL LDDUM
C&IF DI3000
C&      IF(.NOT.LNKINC) CALL DIDUM
C&ENDIF
      STOP
      END

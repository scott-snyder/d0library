      SUBROUTINE PX_ROTATE(COMMAND)
C----------------------------------------------------------------------
C-   Purpose and Methods : Initialize screen parameters to rotate
C-   the current view(s).  VERSION FOR DI3GL (SGI)
C-   Inputs  : COMMAND  [C*]    Screen Command
C-   Outputs : COMMAND  [C*]    Screen Command
      CHARACTER*(*) COMMAND
      COMMON/RSEGMS/NSEG,IRSEG(512)
C
C  GET LIST OF ROTATABLE SEGMENTS (WILL BE -1 FOR ROTATABLE)
      CALL PU_GET_SEGMENT_TYPE(NSEG,IRSEG)
C
C  CALL THE FAST MANIPULATION MENU
      CALL J_RZP3D
C
  999 RETURN
      END

      SUBROUTINE ACP_MOUNT(FDSCRP,ACCESS,TAPE,VSN,NSTAT)

C Modified: 16-JUL-1992 Kirill Denisenko
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:ACPDEF.PARAMS'
      INCLUDE 'D0$INC:ACP_USER.INC'
      INCLUDE 'D0$INC:ACPOUT.INC'

      INTEGER FDSCRP,NSTAT,RBSTATUS,TLEN,DSTATUS
      CHARACTER*(*) ACCESS
      CHARACTER*80 DEVNAM,MSG,NAME
      CHARACTER*40 TDRIVE
      CHARACTER*7  VSN
      CHARACTER*5  TDRV
      LOGICAL TAPE


      NSTAT = 0

      IF( TAPE ) THEN
        CALL GETENV('CPS_DRIVE',TDRIVE)
        TLEN = INDEX(TDRIVE,' ') -1
        IF (TLEN .LE. 0) TLEN = LEN(TDRIVE)
        DEVNAM = '/usr/cpsdev/'//TDRIVE(1:TLEN)//'_nrnsv.8200:V=#2'
        CALL RBMOUNT(FDSCRP,DEVNAM,ACCESS,RBSTATUS) 
        IF(RBSTATUS.EQ.0) THEN
          CALL RBVSN(FDSCRP,VSN,RBSTATUS)
          MSG = '[ACP_MOUNT]'//VSN//' MOUNTED ON '//TDRIVE(1:TLEN)
          CALL FTYPE(MSG,1,1)
        ELSE 
          MSG = '[ACP_MOUNT] FAILED TO MOUNT '//VSN
          CALL FTYPE(MSG,RBSTATUS,1)
          NSTAT = -1
        ENDIF
      ELSE
        CALL FTYPE('[ACP_MOUNT] MOUNT REQUEST NOTED',0,0)
        VSN = 'VG0000'
      ENDIF

      RETURN


      ENTRY ACP_DISMOUNT(FDSCRP,TAPE,NSTAT)

      IF( TAPE ) THEN
        CALL RBFNAME(FDSCRP,NAME,RBSTATUS)
        TDRV = NAME(13:17)
        CALL ACP_SIGHOLD
        CALL RBUMOUNT(FDSCRP,RBSTATUS)
        CALL ACP_SIGRELEASE
        DSTATUS = ACPSTA_DONE_WITH_TAPE(TDRV)
        IF (DSTATUS .NE. ACP$OK) THEN
          NSTAT = -1
          MSG = '[ACP_DISMOUNT]FAILED TO DEALLOCATE TAPE'
          CALL FTYPE(MSG,DSTATUS,RBSTATUS)
        ELSE
          MSG = '[ACP_DISMOUNT]'//TDRV//' SUCCESSFULLY DEALLOCATED'
          CALL FTYPE(MSG,DSTATUS,RBSTATUS)
        ENDIF
      ELSE
        CALL ACP_SIGHOLD
        CALL RBFCLOSE(FDSCRP,RBSTATUS)
        CALL ACP_SIGRELEASE
        CALL FTYPE('[ACP_DISMOUNT] DISMOUNT REQUEST NOTED',RBSTATUS,1) 
      ENDIF

      RETURN

      END
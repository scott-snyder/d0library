      LOGICAL FUNCTION VTX_END_RUN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End run handler for VTX_EXM package
C-
C-   Returned value: .TRUE. if evr'thaing was just fine, .FALSE. if
C-                   sump'n gone wrong.
C-
C-   Created   8-MAR-1994   Justin R. Bendich
C-   Modified 20-MAR-1994   JRB put most of functionality into VTX_FIND_BEAM
C-   Updated   3-OCT-1994   Liang-ping Chen Never open a new version 
C-                          of USR$OUT:BEAM_POSITION.DAT to prevent losses due
C-                          to purge command; Do not make an entry if 
C-                          NUsed is less than 500.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:VTX_ACCUM_TRACKS.INC'
      REAL EstErr, ZCut
      DOUBLE PRECISION Params(4), Errs(4), ZCenter, Chi2
      INTEGER IUSER, LUN, Ptr, Err, I, Run, NUsed, ISL, Store, RUNNO
      LOGICAL First, Closed, UserOut, VTX_FIND_BEAM
      CHARACTER*13 Message
      CHARACTER*14 BegDate, EndDate
      CHARACTER*25 RunFile, BeamFile
      COMMON /CALIB_FILE/ LUN, Closed, IUSER, UserOut, Store, BegDate
      COMMON /SLATE/ ISL(6)
      DATA BeamFile/'USR$OUT:BEAM_POSITION.DAT'/, First/.TRUE./
      DATA ZCut/28/
   10 FORMAT('XYBEAM_',I6.6,'.DAT')
   20 FORMAT('USR$OUT:XYBEAM_',I6.6,'.DAT')
   30 FORMAT('IOSTAT =',I5)
   40 FORMAT(' Run',I7,' Store',I5,', Lay3, Beg. time ',A14,
     &       ', End time ',A14)
   50 FORMAT(' Ax=',I7,', Xb=',D11.4,'+-',E10.4,', Ay=',I7,
     &       ', Yb=',D11.4,'+-',E10.4)
   60 FORMAT(I2.2,'/',I2.2,'/',I2.2,I3.2,':',I2.2)
   70 FORMAT(' dX/dZ =',D11.3,', dY/dZ =',D11.3,', Z0 =',F6.2)
C
      IF(First) THEN
        First = .FALSE.
        CALL EZLOC('VTRAKS_RCP', Ptr)
        IF(Ptr .LE. 0) THEN
          CALL INRCPE('VTRAKS_RCPE', Err)
        ELSE
          Err = 0
        ENDIF
        IF(Err .EQ. 0) THEN
          CALL EZPICK('VTRAKS_RCP')
          CALL EZGET('Z_CUT', ZCut, Err)
          CALL EZRSET
        ENDIF
      ENDIF
C
C ****  Close VTXCALIB file written by WRITE_VTXT
C
      IF(.NOT. Closed) THEN
        CLOSE(LUN)
        Closed = .TRUE.
      ENDIF
      CALL DHDIR('VTRAKS_RCP','HBOOK_DIRECTORY',Err,' ')
      IF(.NOT. VTX_FIND_BEAM(.TRUE., Params, Errs, NUsed,
     &                       ZCenter, Chi2)) GOTO 1000
      Run = RUNNO()
      IF(UserOut) THEN
        WRITE(RunFile,20) Run
      ELSE
        WRITE(RunFile,10) Run
      ENDIF
      OPEN(LUN, FILE=RunFile, STATUS='NEW', IOSTAT=Err)
      IF(Err .EQ. 0) THEN
        WRITE(LUN,*) NUsed, ' tracks used.'
        WRITE(LUN,*) 'Zmin : ', ZCenter - ZCut
        WRITE(LUN,*) 'Zmax : ', ZCenter + ZCut
        WRITE(LUN,*) '  X0 : ', Params(1), ' +-', Errs(1)
        WRITE(LUN,*) '  Y0 : ', Params(2), ' +-', Errs(2)
        WRITE(LUN,*) 'dXdZ : ', Params(3), ' +-', Errs(3)
        WRITE(LUN,*) 'dYdZ : ', Params(4), ' +-', Errs(4)
        WRITE(LUN,*) 'Chi2 : ', Chi2
        CLOSE(LUN)
      ELSE
        WRITE(Message,30) Err
        CALL ERRMSG('Can''t open run file', 'VTX_END_RUN', Message,
     &              'W')
      ENDIF
      IF(UserOut.and.NUsed.ge.500) THEN
        OPEN(LUN, FILE=BeamFile, STATUS='OLD', ACCESS='APPEND',
     &       IOSTAT=Err)
        IF(Err .NE. 0) THEN
          WRITE(Message,30) Err
          CALL ERRMSG('Can''t open beam file','VTX_END_RUN',Message,
     &                  'W')
          GOTO 1000
        ENDIF
        CALL DATIME(I, Ptr)                    ! writes into COMMON /SLATE/
        WRITE(EndDate,60) ISL(2),ISL(3),MOD(ISL(1), 100),ISL(4),ISL(5)
        WRITE(LUN,40) Run, Store, BegDate, EndDate
C
C ****  The following number is hard-coded to indicate that something more
C ****  intelligent should be done.
C
        EstErr = 0.133 / SQRT(REAL(NUsed))
        WRITE(LUN,50) NUsed, Params(1), EstErr, NUsed, Params(2), EstErr
        WRITE(LUN,70) Params(3), Params(4), SNGL(ZCenter)
        CLOSE(LUN)
      ENDIF
      VTX_END_RUN = .TRUE.
      GOTO 1500
 1000 VTX_END_RUN = .FALSE.
 1500 NTracks = 0
      END

      LOGICAL FUNCTION GETDEV()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return .TRUE. if on a VT100 or equivalent
C-                         terminal. VAX-specific.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-   Updated 17-SEP-1991  Herbert Greenlee
C-       Always return true on UNIX.
C-
C----------------------------------------------------------------------
C&IF VAXVMS
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INTEGER DVI$_DEVCHAR,DVI$_DEVCLASS,DVI$_DEVTYPE
      PARAMETER        (DVI$_DEVCHAR = 2)  ! Device characteristics - VALUE - 
C                                          ! 4 bytes
      PARAMETER (DVI$_DEVCLASS = 4)        ! Device class - VALUE - 1 byte
      PARAMETER (DVI$_DEVTYPE = 6)         ! Device type - VALUE - 1 byte
      INTEGER DT$_VT100
      PARAMETER (DT$_VT100 = 96)           ! VT100
C
C  $GETDVI
C
C    Get Device/Volume Information
C
C     $GETDVI  [efn] ,[chan] ,[devnam] ,itmlst ,[iosb] ,[astadr]
C              ,[astprm] ,[nullarg]
C
C     efn    = event flag to be set at completion
C
C     chan   = number of a channel assigned to the device or
C              0 if device is specified by the devnam parameter
C
C     devnam = address of device name or logical name descriptor
C
C     itmlst = address of a list of item descriptors
C
C     iosb   = address of a quadword I/O status block
C
C     astadr = address of entry mask of AST routine
C
C     astprm = value to be passed to AST routine
C
C     nullarg = reserved argument
C
C
      INTEGER*2 IOS(4),ITMLST(8)
      INTEGER ITM(3),BUF(2),ISTAT,SYS$GETDVIW
      EQUIVALENCE (ITM(1),ITMLST(3))
C----------------------------------------------------------------------
      IF(SMGON.AND..NOT.ONEFLG) THEN     ! Force it to use line
C                                       ! mode when only one command was entered
        ITMLST(1)=4
        ITMLST(2)=DVI$_DEVTYPE
        ITM(1)=%LOC(BUF(1))
        ITM(2)=%LOC(BUF(2))
        ISTAT=SYS$GETDVIW(,,'SYS$COMMAND:',%REF(ITMLST),IOS,,,,)
        IF(ISTAT.NE.1) CALL MSGSCR(ISTAT,' GETDEV-->')
        IF(BUF(1).GE.DT$_VT100) THEN
          GETDEV=.TRUE.
        ELSE
          GETDEV=.FALSE.
        ENDIF
      ELSE
        GETDEV=.FALSE.
      ENDIF
C&ELSE
C&      GETDEV=.TRUE.
C&ENDIF
      RETURN
      END

      SUBROUTINE D0DAD_DFCHECK(DFNAME,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Read a D0DAD file checking for logical errors.
C-     (eg, duplicate fid and positions appearing as different runs or
C-      multiple occurances of a given event)
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-Jul-1994   John D. Hobbs
C-   Modified 16-Sep-1994  JDH Rename to D0DAD_DFCHECK from D0DAD_CHECK
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dadcom.inc'
      INCLUDE 'D0$INC:d0dad.inc'
      CHARACTER*(*) DFNAME
      INTEGER  IERR,IEVT,I,LREF,LCOMP,IEVREC(7,2),IDUMMY,IERR_FULL
      INTEGER  IR,IE,IFID,IZR,IZB,IDATE(2),J
      LOGICAL  D0DAD_IV_EQUAL
      COMMON/D0DAD_ORDERING/IR,IE,IFID,IZR,IZB,IDATE
C-----------------------------------------------------------------------
C
C  Open reference version of input file and read first two entries
C
      IERR_FULL=0
      CALL D0DAD_OPEN(JFDF,DFNAME,'R',LREF,IERR)
      IF( IERR.NE.0 ) GOTO 901
      CALL DFGET(LREF,IR,IE,IFID,IZR,IZB,IDATE,IERR)
      IF( IERR.EQ.1 ) GOTO 999
      IF( IERR.NE.0 ) GOTO 902
      CALL UCOPY(IR,IEVREC(1,1),7)
      CALL DFGET(LREF,IR,IE,IFID,IZR,IZB,IDATE,IERR)
      IF( IERR.EQ.1 ) GOTO 999
      IF( IERR.NE.0 ) GOTO 902
      CALL UCOPY(IR,IEVREC(1,2),7)
      IF( D0DAD_IV_EQUAL(IEVREC(3,1),IEVREC(3,2),3) ) THEN
        IERR_FULL = IERR_FULL + 1
        IF( LDDBG.GT.0 ) THEN
          WRITE(*,1001)
          WRITE(*,1002) IEVT,(IEVREC(J,1), J=1,7)
          WRITE(*,1003) I,(IEVREC(J,2), J=1,7)
 1001     FORMAT(' Duplicate entry:')
 1002     FORMAT('  Reference  (Record ',I6,'): ',7I7)
 1003     FORMAT('  Comparison (Record ',I6,'): ',7I7)
        ENDIF
      ENDIF
C
      CALL D0DAD_OPEN(JFDF,DFNAME,'R',LCOMP,IERR)
      IF( IERR.NE.0 ) GOTO 903
C
C  Loop on reference version and do a brute force check by opening a 
C  sectond copy of the input file.
C
      IEVT=3
 10   CONTINUE
        CALL DFGET(LREF,IR,IE,IFID,IZR,IZB,IDATE,IERR)
        IF( IERR.EQ.1 ) GOTO 999
        IF( IERR.NE.0 ) GOTO 904
        CALL UCOPY(IR,IEVREC(1,1),7)
        IF( LDDBG.GT.4 .AND. MOD(IEVT,50).EQ.0 ) WRITE(*,1004) IEVT
 1004   FORMAT(I8,' events checked')
        DO I=1,IEVT-1
          CALL DFGET(LCOMP,IR,IE,IFID,IZR,IZB,IDATE,IERR)
          IF( IERR.NE.0 ) GOTO 905
          CALL UCOPY(IR,IEVREC(1,2),7)
          IF( D0DAD_IV_EQUAL(IEVREC(3,1),IEVREC(3,2),3) ) THEN
            IERR_FULL = IERR_FULL + 1
            IF( LDDBG.GT.0 ) THEN
              WRITE(*,1001)
              WRITE(*,1002) IEVT,(IEVREC(J,1), J=1,7)
              WRITE(*,1003) I,(IEVREC(J,2), J=1,7)
            ENDIF
          ENDIF
        ENDDO
        CALL DFHRD(LCOMP,IERR)    ! Read header ==> REWIND to beginning
        IF( IERR.NE.0 ) GOTO 906
        IEVT=IEVT+1
      GOTO 10
C
 999  CONTINUE
      IF( LDDBG.GT.3 ) WRITE(*,2001)
 2001 FORMAT(/,' ==> CHECK COMPLETED <==',/)
      CALL D0DAD_CLOSE(LREF,IDUMMY)
      CALL D0DAD_CLOSE(LCOMP,IDUMMY)
      IERR=IERR_FULL
      RETURN
C
 901  CONTINUE
      IERR=-1
      RETURN
C
 902  CONTINUE
      IERR=-2
      RETURN
C
 903  CONTINUE
      IERR=-3
      RETURN
C
 904  CONTINUE
      IERR=-4
      RETURN
C
 905  CONTINUE
      IERR=-5
      RETURN
C
 906  CONTINUE
      IERR=-6
      RETURN
C
      END

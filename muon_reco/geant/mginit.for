      SUBROUTINE MGINIT(FILEN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To get GEANT physics, material, geometry stuff.
C-                         D0$STP:MURECO_GSAVE.DAT is read in.( same as
C-                         GSAVE.DAT but produced with diffeent geant cards)
C-   Inputs  : FILEN  Name of the input file
C-   Outputs :
C-   Controls:
C-
C-   Created  18-JUN-1990   SHAHRIAR ABACHI
C-   Updated  02-DEC-1990   SHAHRIAR ABACHI    Gsave.dat is now read from stp
C-   Updated  17-DEC-1991   SHAHRIAR ABACHI    Modified for GEANT314
C-   Updated   9-Mar-1992   Herbert Greenlee
C-       Fixed UNIX problems (concatentaion).  Changed OPEN to D0OPEN.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'
      INCLUDE 'D0$INC:GCLIST.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:GCPHYS.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      CHARACTER*4 KEYSU
      CHARACTER*72 ENTRY(5), VD0GEANT
      CHARACTER*80 CTEMP
      LOGICAL OK
      CHARACTER*(*) FILEN
C
      INTEGER IERR
      INTEGER IER,IDENT,IOS,LUN
      DATA IER  /0/
      DATA IDENT/1/
C
C-    Initialize GEANT
C
      CALL GINIT
C
C -  ZEBRA structures
C
      CALL GZINIT                       ! Init Geant Zebra structures
      CALL INZSTP                       ! Init D0 Zebra structure  ZEBSTP
C
      NGET = 1
      KEYSU='INIT'
C
      CTEMP = ' Read Static Parameter File '//FILEN
      CALL INTMSG(CTEMP)
C
      CALL GTUNIT(888,LUN,IER)
      CALL D0OPEN (LUN, FILEN, 'IU', OK)
      IF(.NOT.OK) THEN
        CALL INTMSG(' MGINIT: Can not open file: ')
        GO TO 999
      END IF
      CALL GOPEN(LUN,'I',0,IER)
      CALL GGET(LUN,KEYSU,-NGET,IDENT,IER)
      CALL FZENDI(LUN,'T')
      CLOSE(UNIT=LUN)
      CALL RLUNIT(888,LUN,IER)
C
      CALL GPHYSI
C
      CALL GGCLOS
C----------------------------------------------------------------------
  999 RETURN
      END

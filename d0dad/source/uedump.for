      SUBROUTINE UEDUMP(ILUN,IOPTS,NOPTS,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write a formattted dump of an unsorted
C-     event catalog.  The file should be opened using D0DAD_OPEN
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  14-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$inc:zebcom.inc'
      INCLUDE  'D0$INC:d0dadcom.inc/NOLIST'
      INTEGER  ILUN,NOPTS,IOPTS(NOPTS),IERR,LC
      INTEGER  IRUN,IEVT,IZRN,IZBO,NEV,NFILES,NEVTOT,IMSK(2),LF,LG,LT
      LOGICAL  LFIRST
      INTEGER  LENOCC
      EXTERNAL LENOCC
      CHARACTER*8 CFID
      INTEGER  IREC(JRECUE),I,NUEFN,NUEGN,NUETAP,NUECOM,UE_VER
      LOGICAL D0DAD_RE_RANGE
      EXTERNAL D0DAD_RE_RANGE
C----------------------------------------------------------------------
C
      CALL UELSET(ILUN,IERR)
      IF( IERR.NE.0 ) THEN
        IERR = -4
        GOTO 999
      ENDIF
C
      NEVTOT=0
      NEV=0
      NFILES=0
      LFIRST=.TRUE.
      UE_VER=IQ(LUEHD+NDUE+3)
      NUEFN=IQ(LUEHD+JUEFN)
      NUEGN=IQ(LUEHD+JUEGN)
      NUETAP=IQ(LUEHD+JUETAP)
      NUECOM=IQ(LUEHD+JUECOM)
C
      CFID='D0DAD '//CFTYPE(JFUE)
      WRITE(*,1000) CFID,UE_VER,NUEFN,NUEGN,NUETAP,NUECOM
 1000 FORMAT(/,/,
     +       '   ID Field: ',A,/,
     +       '   Version: ',I5,/,
     +       '   File Name Size: ',I4,/,
     +       '   Generic Name Size: ',I4,/,
     +       '   Tape Info field Size: ',I4,/,
     +       '   Comment field Size: ',I4,/,/)
C
 10   CONTINUE
         CALL UEGET(ILUN,IRUN,IEVT,IMSK,IZRN,IZBO,CFNAME,CGNAME,CTAPE,
     +    CFCCOM,IERR)
         IF( IERR.EQ.1 ) GOTO 998
         IF( IERR.LT.0 ) THEN
            IF( LDDBG.GT.0 ) WRITE(*,9001) IERR,ILUN,IRUN,IEVT
 9001       FORMAT(' UEDUMP: Error ',I4,' returned from UEGET (lun=',
     +           I3,') for r/e: ',2I8)
            IERR = -1
            RETURN
         ENDIF
         IF( .NOT.D0DAD_RE_RANGE(IOPTS(2),IOPTS(4),IRUN,IEVT)) GOTO 10
         IF( IRUN.EQ.-1 ) THEN
            CALL D0DAD_CPAD(CFNAME)
            CALL D0DAD_CPAD(CGNAME)
            CALL D0DAD_CPAD(CTAPE)
            CALL D0DAD_CPAD(CFCCOM)
            LF=LENOCC(CFNAME)
            LG=LENOCC(CGNAME)
            LT=LENOCC(CTAPE)
            LC=LENOCC(CFCCOM)
            IF( IOPTS(1).NE.0 ) THEN
               IF( .NOT.LFIRST ) WRITE(*,1001) NFILES,NEVTOT,NEV
 1001          FORMAT('    File ',I5,' done after ',I8,' events (',
     +            I6,' in file)')
               WRITE(*,2)CFNAME(1:LF),CGNAME(1:LG),CTAPE(1:LT),
     +            CFCCOM(1:LC)
 2             FORMAT(/,' New File: ',A,/,
     +               ' Generic Name: ',A,/,
     +               ' Tape Information: ',A,/,
     +               ' Comment: ',A,/)
            ENDIF
            NEV=0
            NFILES=NFILES+1
            LFIRST=.FALSE.
         ELSE
            NEVTOT=NEVTOT+1
            NEV=NEV+1
            IF( IOPTS(1).EQ.0 ) GOTO 10
            WRITE(*,1002)IRUN,IEVT,IMSK(1),IMSK(2),IZRN,IZBO
 1002       FORMAT(10X,'R/E: ',2I8,' Mask: ',2Z8.8,' Rec/Offs: ',2I8)
CJDH 1002       FORMAT(10X,'R/E: ',2I8,' Mask: ',2(I10,X),' Rec/Offs: ',2I8)
         ENDIF
      GOTO 10
C
 998  CONTINUE
      IF( IOPTS(1).NE.0 ) WRITE(*,1001) NFILES,NEVTOT,NEV
C
 999  CONTINUE
      WRITE(*,1003) NEVTOT,NFILES
 1003 FORMAT(/,'  Found ',I10,' events in ',I8,' files.',/,/)
      IERR=0
      RETURN
      END

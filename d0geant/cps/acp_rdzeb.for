      SUBROUTINE RDZEB(INHEAD,IUHEAD,ISTATS)
C-----------------------------------------------------------------------
C-    This routine reads the standard D0-Zebra structure from input file
C-  in following sequence.
C-      1) reads user header record from the file.
C-      2) check if the user header record (IUHEAD) in file matches to
C-         one we want (INHEAD).    If it dose not match, set ISTATS=-1
C-         and suspend the event.
C-      3) If IUHEAD matches to INHEAD, set ISTATS=0 and read the event.
C-
C-  INPUT(A):  INHEAD =  header record to be read.
C-                          -1 =  no header record, just read in data.
C-                           1 =  begin run record
C-                           2 =  end run record
C-                           3 =  event record
C-  INPUT(C):  logical unit number of input file, from /ZEBIO/
C-  INPUT(B):  bank pointer LHEAD.
C-
C-  OUTPUT(A): IUHEAD =  first user header record read in.
C-  OUTPUT(B): LHEAD bank structure
C-
C-  S.Kunori     Feb.,1986
C-   Updated  29-NOV-1989   Alan M. Jonckheere   Simplify
C-   Updated  26-FEB-1990   Alan M. Jonckheere   Add UFFARM
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBIO.INC'
      INCLUDE 'D0$INC:ZEBIOG.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:GCLIST.INC'
      INCLUDE 'D0$INC:GCFLAG.INC'
      INCLUDE 'D0$LINKS:IZGEAN.LINK'
C&IF UFFARM
C&      INCLUDE 'D0$INC:MIOFLG.INC'                             ! Multi
C&      CHARACTER*60 NAMIN,NAMCHK,TABLE                         ! Multi
C&      INTEGER I,LERROR                                        ! Multi
C&ENDIF
      INTEGER INHEAD,IUHEAD(14),ISTATS,NUHEAD
      INTEGER IUCOMP,CHKBRR,IRREC
      INTEGER HITS,DIGI,JXYZ
      EXTERNAL ACP_FZIO
      DATA HITS/4hHITS/,DIGI/4hDIGI/,JXYZ/4hJXYZ/
      CHARACTER*7 DISP,CHOPT
      INTEGER BINTIM(2),DUMMY
      DATA BINTIM/2*0/
      LOGICAL FIRST,OK
      DATA    FIRST/.TRUE./
C-----------------------------------------------------------------------
C
C ****  Do we need to open input?
   10 IF ( FIRST ) THEN
C&IF UFFARM
C&C
C&C--- CLEAR MULTI I/O FLAGS                                    ! Multi
C&C                                                             ! Multi
C&      DO I=1,3                                                ! Multi
C&        MFLAG(I)=0                                            ! Multi
C&      ENDDO                                                   ! Multi
C&C                                                             ! Multi
C&C--- GET NAME OF 1ST MULTI INPUT FILE                         ! Multi
C&C                                                             ! Multi
C&      CALL MAP_GET_DATASET(NAMIN)                             ! Multi
C&      IF(NAMIN .NE. '  ') THEN                                ! Multi
C&      TABLE='LNM$JOB'                                         ! Multi
C&        CALL MLB_SETLOGICAL('FOR031',TABLE, NAMIN, LERROR)    ! Multi
C&      ENDIF                                                   ! Multi
C&      MFLAG(1)=1                                              ! Multi
C&ENDIF
C&IF UFFARM
C&C ****  Open the requested file
C&        CALL D0OPEN(IRDUNI,NAMIN,IRDCHR,OK)
C&        IF(.NOT.OK)  GO TO 980
C&        CALL XZRECL(IRREC,CHOPT)
C&ENDIF
C&IF VAXELN
C&C FARM version
C&        CALL FARM_OPEN(IRDUNI,'BEGRUN')  !FARM Specific
C&ENDIF
        IF(IRDFLN.EQ.' ') THEN
C&IF VAXVMS
          WRITE(IRDFLN,101) IRDUNI
101       FORMAT('FOR',I3.3)
C&ELSE
C&          IF(IRDUNI.LT.10) WRITE(IRDFLN,102) IRDUNI
C&          IF(IRDUNI.GE.10) WRITE(IRDFLN,103) IRDUNI
C&102       FORMAT('fort.',I1.1)
C&103       FORMAT('fort.',I2.2)
C&ENDIF
        ENDIF
C&IF VAXVMS,SIUNIX,IBMAIX,ULTRIX
C        CALL D0OPEN(IRDUNI,IRDFLN,IRDCHR,OK)
C        IF(.NOT.OK)  GO TO 980
C        CALL XZRECL(IRREC,CHOPT)
        IRDREC = 900
        IRDCHR = 'CSI'
        CALL FTYPE(' RDZEB: IRDREC = ',IRDREC,0)
        CALL FZFILE(IRDUNI,IRDREC,IRDCHR)
        CALL FZHOOK(IRDUNI,ACP_FZIO,DUMMY)
C&ENDIF
C        CALL FZFILE(IRDUNI,IRREC,CHOPT)
        FIRST = .FALSE.
        DISP = 'KEEP'
      ELSE
C&IF VAXELN
C&C FARM version
C&        CALL FARM_OPEN(IRDUNI,'EVENTOUT')  !FARM Specific
C&        DISP = 'DELETE'
C&ENDIF
      ENDIF
C
      ISTATS = 0
C
  950 IF ( LHEAD.NE.0 ) CALL MZWIPE(IXCOM+21)
      NUHEAD = 1
      CALL FZIN(IRDUNI,IXMAIN,LHEAD,1,' ',NUHEAD,IUHEAD)
      IF ( IQUEST(1).GT.2 ) GO TO 990   ! EOF read
      IF ( IQUEST(1).GE.-6 .AND. IQUEST(1).LT.0 ) GO TO 950
      IF ( IQUEST(1).LE.-7 ) GO TO 970
      IF ( LHEAD.LE.0 ) GOTO 950        ! Problem? or unrecognised structure
C
C Close IRDUNI for MicroVax
C&IF VAXELN
C&      CLOSE(IRDUNI,DISPOSE=DISP)  !Microvax specific
C&ENDIF
      IF ( LQ(LHEAD-IZGEAN).GT.0 ) THEN
        CALL MZDROP(IXCOM,LQ(LHEAD-IZGEAN),' ')  ! Drop GEAN from ISAJET
      ENDIF
C
C ****  Save Event and Run numbers in Event ID field
      IF ( NGET.GT.0 ) THEN
        IF( (IUCOMP(HITS,LGET,NGET).EQ.0) .AND.
     &      (IUCOMP(DIGI,LGET,NGET).EQ.0) .AND.
     &      (IUCOMP(JXYZ,LGET,NGET).EQ.0) ) THEN

          IQ(LHEAD+7)  = IQ(LHEAD+9)      ! Isajet event # -> input id
          IQ(LHEAD+8)  = IQ(LHEAD+6)      ! Isajet run #   -> input id
        ENDIF
      ELSE
        IQ(LHEAD+7)  = IQ(LHEAD+9)      ! Isajet event # -> input id
        IQ(LHEAD+8)  = IQ(LHEAD+6)      ! Isajet run #   -> input id
      ENDIF
C
C ****  Fill in Geant data
      CALL TIME(BINTIM)
      IDEVT = IQ(LHEAD+9)		! set event # to isajet #
      IQ(LHEAD+4)  = BINTIM(1)          ! time
      IQ(LHEAD+5)  = BINTIM(2)          ! time
      IQ(LHEAD+6)  = IDRUN              ! new local run #
      IQ(LHEAD+9)  = IDEVT              ! new event #
      IQ(LHEAD+12) = IDRUN              ! new global run # = local run #
C
C&IF VAXELN
C&      CALL SETSTAT(IQ(LHEAD+7))
C&ENDIF
C
C ****  Check for Begin Run Record
      ISTATS = CHKBRR()
      IF ( ISTATS.NE.0 ) GOTO 10        ! Found BRR or ERR, read another
      GO TO 999
C
C  user header record is miss match...
C
  960 CONTINUE
      ISTATS = -1
      GO TO 999
C
C  Three consequtive errors were detected, fatal error...
C
  970 CONTINUE
      ISTATS = 2
      WRITE(LOUT,67) IRDUNI,ISTATS,IQUEST(1)
   67 FORMAT('0***** FATAL ERROR (S/R RDZEB) *****'
     1/10X,'THREE CONSECUTIVE READ ERROR WERE DETECTED IN UNIT',I3,'.'
     2/15X,'STATUS CODE=',I3,5X,'ERROR CODE (IQUEST(1))=',I3)
      GO TO 999
C
C ****  ERROR ON OPEN
  980 ISTATS = 1
      WRITE(LOUT,68) IRDUNI,ISTATS,IQUEST(1)
   68 FORMAT('0***** FATAL ERROR (S/R RDZEB) *****'
     1/10X,'COULD NOT OPEN UNIT',I3,'.'
     2/15X,'STATUS CODE=',I3,5X,'ERROR CODE (IQUEST(1))=',I3)
      GO TO 999
C
C  End Of File mark was detected...
C
  990 CONTINUE
      ISTATS = 1
C
C&IF UFFARM
C&C                                                             ! Multi
C&C--- CHECK FOR END OF CURRENT INPUT FILE                      ! Multi
C&C                                                             ! Multi
C&      IF(ISTATS.EQ.1) THEN                                    ! Multi
C&C--- CLOSE THE CURRENT INPUT FILE IN ZEBRA                    ! Multi
C&        CLOSE(IRDUNI)                                         ! Multi
C&        CALL FZENDN(IRDUNI,'T')                               ! Multi
C&C--- GET NEW DATA SET NAME FOR INPUT                          ! Multi
C&        CALL MAP_DATADONE                                     ! Multi
C&        CALL MAP_GET_DATASET(NAMIN)                           ! Multi
C&        WRITE(LOUT,702) NAMIN                                 ! Multi
C&  702 FORMAT(1X,'RDZEB INFO 702, NAMIN',1X,A60)               ! Multi
C&C--- CHECK FOR END OF INPUT FILE LIST                         ! Multi
C&        IF(NAMIN.EQ.'    ') THEN                              ! Multi
C&          MFLAG(3)=1                                          ! Multi
C&          IEOTRI=1                                            ! Multi
C&          IEORUN=1                                            ! Multi
C&          GO TO 999                                           ! Multi
C&C--- OTHERWISE READ RECORD FROM NEW FILE                      ! Multi
C&        ELSE                                                  ! Multi
C&C--- OPEN THE NEW INPUT DATASET AND DECLARE IT TO ZEBRA       ! Multi
C&          WRITE(LOUT,704) IRDUNI,IRDREC,IRDCHR                ! Multi
C&  704     FORMAT(1X,'RDZEB INFO 704, IRDUNI,IRDREC,IRDCHR'    ! Multi
C&     C           ,2I10,A4)                                    ! Multi
C&C--- POINT TO THE NEW FILE NAME                               ! Multi
C&          TABLE='LNM$JOB'                                     ! Multi
C&          CALL MLB_SETLOGICAL('FOR031',TABLE, NAMIN, LERROR)  ! Multi 
C&          IF(LERROR) THEN                                     ! Multi
C&            WRITE(LOUT,*) ' RDZEB ERROR SETLOGICAL'           ! Multi
C&          ENDIF                                               ! Multi
C&          CALL MLB_SEELOGICAL('FOR031',TABLE,NAMCHK,LERROR)   ! Multi
C&          WRITE(LOUT,705) NAMCHK                              ! Multi
C&  705     FORMAT(1X,'RDZEB SEELOGICAL NAMCHK ',A60)           ! Multi
C&          OPEN(UNIT=IRDUNI, FILE=NAMIN, STATUS='OLD',         ! Multi
C&     C          FORM='UNFORMATTED', READONLY)                 ! Multi
C&          CALL FZFILE(IRDUNI,IRDREC,IRDCHR)                   ! Multi
C&          MFLAG(1)=2                                          ! Multi
C&          GO TO 10                                            ! Multi
C&        ENDIF                                                 ! Multi
C&      ENDIF                                                   ! Multi
C&ENDIF
  999 CONTINUE
C&IF UFFARM
C&C--   check if error                                          ! Multi
C&C                                                             ! Multi
C&      IF (ISTATS.NE.0.AND.ISTATS.NE.1)   THEN                 ! Multi
C&C                                                             ! Multi
C&C--   read error or mismatch was found...                     ! Multi
C&C                                                             ! Multi
C&        WRITE(LOUT,701) ISTATS,IQ(LHEAD+1)                    ! Multi
C&  701   FORMAT(1X,'RDZEB ERROR701 , ISTATS,IQ(LHEAD+1)',      ! Multi
C&     C         2I10)                                          ! Multi
C&        MFLAG(3)=1                                            ! Multi
C&      ENDIF                                                   ! Multi
C&ENDIF
C
      RETURN
      END

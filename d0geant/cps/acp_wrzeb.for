      SUBROUTINE WRZEB(ISTATS)
C-----------------------------------------------------------------------
C-    This program outputs LHEAD zebra bank structure to the output
C-  file specified by common block /ZEBIO/.    An input variable
C-  through arguments is specify the Begin run record,  Event record
C-  and End run record.
C-
C-  INPUT(A):   IOHEAD =  user record header word.
C-                             =1  Begin run record
C-                             =2  End run record
C-                             =3  Event record
C-  INPUT(C):   logical unit number of the output file.  from /ZEBIO/
C-  INPUT(B):   LHEAD bank structure to be output in this program.
C-
C-  OUTPUT(A):  ISTATS =  status word for output,
C-                             =0  normal output
C-                             =1  write error
C-
C-  S.Kunori     Mar.,1986
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:GCFLAG.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBIO.INC'
      INCLUDE 'D0$INC:ZEBIOG.INC'
C&IF UFFARM
C&      INCLUDE 'D0$INC:GCUNIT.INC'                         ! Multi
C&      INCLUDE 'D0$INC:MIOFLG.INC'                         ! Multi
C&      INTEGER ID, MLT_SEE_UNIQUEID,I                      ! Multi
C&      INTEGER IWWREC                                      ! Multi
C&      CHARACTER*60 FILIN, FILOUT, TABLE                   ! Multi
C&      LOGICAL LERROR                                      ! Multi
C&      DATA IWWREC/0/,IWRCHR/'O'/                          ! Multi
C&ENDIF
C&IF VAXELN
C&      INCLUDE 'D0$INC:GETCOM.INC'
C&ENDIF
      INTEGER IOHEAD(14),IWREC,NUHEAD,ISTATS,DUMMY
      INTEGER PCLASS,NALIVE,NDEAD,MAXDEAD,DEADHEADS(20),NPROC
      INTEGER NSEVENT,CFIRST,ACP_PROCESS_NUMBER
      DATA MAXDEAD,NSEVENT /20,1/
      CHARACTER*10 CHOPT
      EXTERNAL ACP_FZIO
      LOGICAL FIRST,OK
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
C
      IF(IWRUNI.EQ.0) GO TO 999
C&IF VAXELN
C&C Farm version: Open file for IWRUNI
C&      ISANUM = IQ(LHEAD+7)            ! Fill ISAJET event #
C&      CALL FARM_OUT(IWRUNI)
C&ENDIF
      IF ( FIRST ) THEN
        IF(IWRFLN.EQ.' ') THEN
C&IF VAXVMS
          WRITE(IWRFLN,201) IWRUNI
201       FORMAT('FOR',I3.3)
C&ELSE
C&          IF(IWRUNI.LT.10) WRITE(IWRFLN,202) IWRUNI
C&          IF(IWRUNI.GE.10) WRITE(IWRFLN,203) IWRUNI
C&202       FORMAT('fort.',I1.1)
C&203       FORMAT('fort.',I2.2)
C&ENDIF
        ENDIF
C&IF VAXVMS,ETA10,VAXELN,SIUNIX,ULTRIX,IBMAIX
C          CALL D0OPEN(IWRUNI,IWRFLN,IWRCHR,OK)
C          CALL XZRECL(IWREC,CHOPT)
C          CALL FZFILE(IWRUNI,IWREC,CHOPT)
         IWRREC = 8190
         IWRCHR = 'CSO'
         CALL FTYPE('WRZEB: IWRREC = ',IWRREC,0)
         CALL FZFILE(IWRUNI,IWRREC,IWRCHR)
         CALL FZHOOK(IWRUNI,ACP_FZIO,DUMMY)
C
C         NSEVENT = ACP_PROCESS_NUMBER(NSEVENT)
C         CALL ACP_CLASS_INFO(PCLASS,CFIRST,NALIVE)
C         CALL ACP_DEAD_PROCESS_INFO(PCLASS,MAXDEAD,DEADHEADS,NDEAD)
C         NPROC = NALIVE + NDEAD
C         write(6,*) 'Class,process,nalive,ndead,nproc: ',pclass,
C     &     nsevent,nalive,ndead,nproc
C&ENDIF
          FIRST = .FALSE.
      ENDIF
C&IF UFFARM
C&C                                                         ! Multi
C&C--- CHECK IF THIS IS 1ST EVENT TO BE WRITTEN FOR         ! Multi
C&C--- 1ST INPUT FILE OR AT END OF CURRENT INPUT FILE.      ! Multi
C&C                                                         ! Multi
C&      WRITE(LOUT,101) (MFLAG(I),I=1,3)                    ! Multi
C&  101 FORMAT(1X,'WRZEB MFLAG(1-3) ',3I5)                  ! Multi
C&      IF(MFLAG(1).NE.0) THEN                              ! Multi
C&C--- CREATE OUTPUT FILENAME FROM INPUT FILENAME           ! Multi
C&        ID = MLT_SEE_UNIQUEID(0)                          ! Multi
C&        TABLE='LNM$JOB'                                   ! Multi
C&        CALL MLB_SEELOGICAL('FOR031',TABLE, FILIN, LERROR)! Multi
C&        IF(LERROR) THEN                                   ! Multi
C&          WRITE(LOUT,*) 'WRZEB ERROR SEELOGICAL'          ! Multi
C&        ENDIF                                             ! Multi
C&        WRITE(LOUT,102) FILIN                             ! Multi
C&  102   FORMAT(1X,'WRZEB SEELOGICAL FILIN ',A60)          ! Multi
C&        CALL MLB_FILECVT(FILIN, ID, FILOUT)               ! Multi
C&        WRITE(LOUT,104) FILOUT                            ! Multi
C&  104   FORMAT(1X,'WRZEB FILECVT FILOUT ',A60)            ! Multi
C&        CALL MLB_SETLOGICAL('FOR032',TABLE, FILOUT, LERROR)! Multi
C&        IF(LERROR) THEN                                   ! Multi
C&          WRITE(LOUT,*) ' WRZEB ERROR SETLOGICAL'         ! Multi
C&        ENDIF                                             ! Multi
C&C        CALL MLT_STORE_OUTFILE(FILOUT, 1, FILIN)         ! Multi
C&C--- CLOSE THE CURRENT OUTPUT FILE                        ! Multi
C&        IF(MFLAG(1).EQ.2) THEN                            ! Multi
C&          CLOSE(IWRUNI)                                   ! Multi
C&          CALL FZENDT(IWRUNI,'T')                         ! Multi
C&        ENDIF                                             ! Multi
C&C--- OPEN THE NEW OUTPUT FILE                              ! Multi
C&        OPEN(UNIT=IWRUNI, FILE=FILOUT, STATUS='NEW',      ! Multi
C&     *         FORM='UNFORMATTED')                        ! Multi
C&C--- DECLARE TO ZEBRA                                     ! Multi
C&        CALL FZFILE(IWRUNI,IWRREC,IWRCHR)                 ! Multi
C&        MFLAG(1)=0                                        ! Multi
C&      ENDIF                                               ! Multi
C&ENDIF
C
C  write out LHEAD banks...
      NUHEAD = 0
C      NSEVENT = NSEVENT + NPROC
C      write(6,*) 'NSEVENT : ',nsevent
C      IQ(LHEAD+9) = NSEVENT
      CALL FZOUT(IWRUNI,IXMAIN,LHEAD,1,' ',2,NUHEAD,IOHEAD)
C&IF VAXELN
C&      CLOSE(IWRUNI)
C&ENDIF
C
C  wipe out all banks in store IXCOM
  999 CALL MZWIPE(IXCOM+21)
      ISTATS=0
C
      RETURN
      END

      SUBROUTINE D0DAD_REPAIR(ECNAME,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Repair a corrupted event catalog.  
C-
C-   Inputs  : ECNAME - Event catalog name
C-   Outputs : IERR   - 0 ==> Repair succeeded.
C-   Controls:
C-
C-   Created  19-Dec-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INCLUDE 'D0$INC:d0dad.inc'
      CHARACTER *(*) ECNAME
      INTEGER  IERR
C
      CHARACTER*128 FILENAME
      INTEGER BAD_RUN,IRECEC,ILAST,I,J,K,IREC1,ILUN,BADRUN,FLEN
      INTEGER LTEMP,IRDATA(KRECEC),DELTA_REC
      LOGICAL FOUND
C
      INTEGER LENOCC
      LOGICAL ECRUN_CHECK
C-----------------------------------------------------------------------
C
      IF(LDDBG.GT.3) WRITE(*,1000) ECNAME(1:LENOCC(ECNAME))
 1000 FORMAT('***** Beginning REPAIR of ',A,' *****')
C
      CALL D0DAD_OPEN(JFEC,ECNAME,'A',ILUN,IERR)
      IF(IERR.NE.0 .AND. IERR.NE.(-6)) THEN
        WRITE(D0DAD_ERRTXT,9901) IERR,ECNAME(1:LENOCC(ECNAME))
 9901   FORMAT('Error ',I4,' opening: ',A)
CJDH        CALL ERRMSG('OPENFAIL','D0DAD_REPAIR',D0DAD_ERRTXT,'E')
        IF(LDDBG.GT.0 ) WRITE(*,*) D0DAD_ERRTXT(1:LENOCC(D0DAD_ERRTXT))
     >     //' from D0DAD_REPAIR'
        IERR = -1
        GOTO 999
      ENDIF
C
C  Check the run section for logical consistancy...
C
      IF( .NOT.ECRUN_CHECK() ) THEN
        WRITE(D0DAD_ERRTXT,9902) ECNAME(1:LENOCC(ECNAME))
 9902   FORMAT('Run section inconsistancy in: ',A)
CJDH        CALL ERRMSG('CHECKFAIL','D0DAD_REPAIR',D0DAD_ERRTXT,'E')
        IF(LDDBG.GT.0 ) WRITE(*,*) D0DAD_ERRTXT(1:LENOCC(D0DAD_ERRTXT))
     >     //' from D0DAD_REPAIR'
        IERR = -2
        GOTO 999
      ENDIF
C
C  Get the run record corresponding to the bad run in header..
C
      BADRUN=IQ(LECHD+NDEC+JEC_ISDIRT)
      IF( BADRUN.EQ.0 ) THEN
        WRITE(D0DAD_ERRTXT,9903) ECNAME(1:LENOCC(ECNAME))
 9903   FORMAT('There is no evidence to suggest corruption in: ',A)
CJDH        CALL ERRMSG('NOCORRUPTION','D0DAD_REPAIR',D0DAD_ERRTXT,'I')
        IF(LDDBG.GT.0) WRITE(*,*) D0DAD_ERRTXT(1:LENOCC(D0DAD_ERRTXT))
        IERR = -3
        GOTO 999
      ENDIF
C
      IRECEC=IQ(LECHD+NDEC+JEC_IRECEC)
      CALL ECFIND(IQ(LRUNS+1),IQ(LECHD+NDEC+JEC_IECRUN),IRECEC,
     +   BADRUN,1,IREC1,IRDATA,IERR)
      IF( IERR.LT.0 ) THEN
        WRITE(D0DAD_ERRTXT,9904) IERR
 9904   FORMAT('Error ',I4,' returned from ECFIND when looking for run')
CJDH        CALL ERRMSG('NORUNFOUND','D0DAD_REPAIR',D0DAD_ERRTXT,'E')
        IF(LDDBG.GT.0) WRITE(*,*) D0DAD_ERRTXT(1:LENOCC(D0DAD_ERRTXT))
        IERR = -4
        GOTO 999
      ENDIF
C
C  Neither the run section nor the header have the true length of a 
C  crashed file.  Use brute force to find the last record, ILAST...
C
      ILUN=IQ(LECHD+JLUN)
      FOUND=.FALSE.
      ILAST=IQ(LECHD+NDEC+JEC_IEVT1)-1
      DO WHILE( .NOT.FOUND ) 
        ILAST=ILAST+1
        CALL ECRRD(ILUN,ILAST,IQ(LPREC+1),IQ(LPREC-1),0,IERR)
        IF( IERR.NE.0 ) FOUND=.TRUE.
      ENDDO
      ILAST=ILAST-1
      DELTA_REC=ILAST-IQ(LECHD+NDEC+JEC_IEVT1)
      IF(LDDBG.GT.3) WRITE(*,1001) ILAST,DELTA_REC,
     >  IQ(LECHD+NDEC+JEC_IEVT1)
 1001 FORMAT(' Last physical record(',I8,') is ',I4,' records beyond ',
     > 'last record(',I8,') known to catalog header.')
      IF(DELTA_REC.LE.0 ) THEN
        IERR = -5
        GOTO 999
      ENDIF
C
C  Find the gap caused by pushing by looking for "earliest" two identical
C  LOGICAL RECORDS in the event section.  This can be confirmed by checking
C  all records in the gap for duplicates following the gap.  Then only
C  need to fix those actually overwritten...
C
      FOUND=.FALSE.
      I=IRDATA(2)
      CALL MZBOOK(IXDDAD,LTEMP,K,2,'TEMP',0,0,IQ(LPREC-1),2,0)
      IF( LTEMP.LE.0 ) THEN
        IERR = -6
        GOTO 999
      ENDIF
      DO WHILE( .NOT.FOUND .AND. I.LT.(ILAST-DELTA_REC) ) 
        I=I+1
        IF(LDDBG.GT.4) WRITE(*,1002) I
 1002   FORMAT('    Duplicate physical record scan. Checking',I8,20X)
        CALL ECRRD(ILUN,I,IQ(LPREC+1),IQ(LPREC-1),0,IERR)
        CALL ECRRD(ILUN,I+DELTA_REC,IQ(LTEMP+1),IQ(LTEMP-1),0,IERR)
        FOUND=.TRUE.
        K=1
        DO WHILE( K.LE.IQ(LPREC-1) .AND. FOUND )
          IF( IQ(LPREC+K).NE.IQ(LTEMP+K) ) FOUND=.FALSE.
          K=K+1
        ENDDO
      ENDDO
      IF( .NOT.FOUND ) THEN
        WRITE(D0DAD_ERRTXT,9907)
 9907   FORMAT(' No duplicate entry found.  Is catalog OK?')
CJDH        CALL ERRMSG('NOCORRUPTION','D0DAD_REPAIR',D0DAD_ERRTXT,'W')
        IF(LDDBG.GT.1)WRITE(*,*)D0DAD_ERRTXT(1:LENOCC(D0DAD_ERRTXT))
        IERR = -7
        GOTO 999 ! No gap found.  Catalog must be OK???
      ENDIF
      IF(LDDBG.GT.3) WRITE(*,1004) I,I+DELTA_REC
 1004 FORMAT(' Gap found.  Matched records are ',I8,' and ',I8)
C
C  For safety, check that matches continue to end of gap region.
C
      J=I
      DO WHILE( (J-I).LT.(DELTA_REC-1) )
        J=J+1
        CALL ECRRD(ILUN,J,IQ(LPREC+1),IQ(LPREC-1),0,IERR)
        CALL ECRRD(ILUN,J+DELTA_REC,IQ(LTEMP+1),IQ(LTEMP-1),0,IERR)
        FOUND=.TRUE.
        K=1
        DO WHILE( K.LE.IQ(LPREC-1) .AND. FOUND )
          IF( IQ(LPREC+K).NE.IQ(LTEMP+K) ) FOUND=.FALSE.
          K=K+1
        ENDDO
        IF( .NOT.FOUND ) THEN
          WRITE(D0DAD_ERRTXT,9908) J,J+DELTA_REC
 9908     FORMAT(' ***FATAL ERROR*** Records ',I8,' and ',I8,
     >       ' fail gap sanity check')
CJDH          CALL ERRMSG('NOSANITY','D0DAD_REPAIR',D0DAD_ERRTXT,'E')
          IF(LDDBG.GT.0) WRITE(*,*) D0DAD_ERRTXT(1:LENOCC(D0DAD_ERRTXT))
          IERR = -8
          GOTO 999
        ENDIF
      ENDDO
C
      CALL MZDROP(IXDDAD,LTEMP,' ')
C
C  Now that the gap has been found, close it to properly represent the 
C  run header.   The gap is closed by copying all physical records
C  starting at record I+DELTA_REC to consecutive records starting at I+1...
C
      DO J=I+1,IQ(LECHD+NDEC+JEC_IEVT1)
        IF(LDDBG.GT.4 .AND. MOD(J,10).EQ.0) WRITE(*,1005) J
 1005   FORMAT('      Pulling record ',I8)
        CALL ECRRD(ILUN,J+DELTA_REC,IQ(LPREC+1),IQ(LPREC-1),0,IERR)
        IF( IERR.NE.0 ) THEN
          WRITE(D0DAD_ERRTXT,9912) IERR
 9912     FORMAT(' Error',I8,' returned from ECRRD in PULL operation')
          IERR = -12
          IF( LDDBG.GT.0 ) WRITE(*,*) 'D0DAD_REPAIR'//':'//D0DAD_ERRTXT
          GOTO 999
        ENDIF
        CALL ECRWRT(ILUN,J,IQ(LPREC+1),IQ(LPREC-1),IERR)
        IF( IERR.NE.0 ) THEN
          WRITE(D0DAD_ERRTXT,9913) IERR
 9913     FORMAT(' Error',I8,' returned from ECRWRT in PULL operation')
          IERR = -13
          IF( LDDBG.GT.0 ) WRITE(*,*) 'D0DAD_REPAIR'//':'//D0DAD_ERRTXT
          GOTO 999
        ENDIF
      ENDDO
C
C  To maintain ability to repair corruptions, fill excess space with -1.
C  Also, for bookkeeping, store number of extra records in headers.
C
      IERR=0
      CALL VFILL(IQ(LPREC+1),IQ(LPREC-1),-1)
      DO J=IQ(LECHD+NDEC+JEC_IEVT1)+1,IQ(LECHD+NDEC+JEC_IEVT1)+DELTA_REC
        CALL ECRWRT(ILUN,J,IQ(LPREC+1),IQ(LPREC-1),IERR)
        IF( IERR.NE.0 ) THEN
          WRITE(D0DAD_ERRTXT,9909) IERR
 9909     FORMAT(' Error ',I8,' from ECRWRT when filling with -1')
          CALL ERRMSG('FILL_ERROR','D0DAD_REPAIR',D0DAD_ERRTXT,'E')
          IF( LDDBG.GT.0 ) WRITE(*,*) 'D0DAD_REPAIR'//':'//D0DAD_ERRTXT
          IERR = -9
          GOTO 999
        ENDIF
      ENDDO
C
C  Write the extra record count to the header
C
      IQ(LECHD+NDEC+JEC_EXTRA)=DELTA_REC
C
C  Close the catalog, and call the file/event catalog consistancy check.
C
      CALL D0DAD_CLOSE(ILUN,IERR)
      IF( IERR.NE.0 ) THEN
        WRITE(D0DAD_ERRTXT,9910) IERR
 9910   FORMAT(' Error ',I8,' from D0DAD_CLOSE for fixed catalog')
        CALL ERRMSG('CLOSE_ERROR','D0DAD_REPAIR',D0DAD_ERRTXT,'E')
        IF( LDDBG.GT.0 ) WRITE(*,*) 'D0DAD_REPAIR'//':'//D0DAD_ERRTXT
        IERR = -10
        GOTO 999
      ENDIF
C
  999 CONTINUE
      RETURN
      END

      SUBROUTINE D0DAD_STREAM(FNAME,IBMASK,CFDF,CDFOPT,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: 
C-
C-   Inputs  :
C-   Outputs : IERR   - I - Error return, 0 ==> All is OK.
C-   Controls:
C-
C-   Created   1-NOV-1994   John D. Hobbs
C-   Modified 16-JUN-1994   John D. Hobbs
C-     Change combination streams to "OR" in order to match stream_filter.
C-   Modified 30-SEP-1994   John D. Hobbs 
C-     Add comment field to stream defn file.
C-   Modified 11-OCT-1996   JDH - Treat events with negative FID's as
C-     deleted events ==> Do not stream
C-
C-------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INCLUDE 'D0$INC:d0dad.inc/NOLIST'
C
      CHARACTER*(*) FNAME,CFDF,CDFOPT,CINOPT*1,CINSTR*32,CTMP*32
      INTEGER IBMASK,IFTYPE,I,IFID,IERR,NEV,NALL,IKEY,IBTYPE,J,KBIT
      INTEGER IDATE(2),NSTREAMS,N
      CHARACTER STRTMP*128
C
      LOGICAL  LSRUN,LSEVT,LSELECT1,LSELECT2
      LOGICAL  D0DAD_RE_RANGE,OK
      INTEGER  LENOCC
      EXTERNAL LENOCC,D0DAD_RE_RANGE
C
      FDFNAM=CFDF
      IF( LDDBG.GT.5 ) THEN
         WRITE(*,9901) FNAME(1:LENOCC(FNAME)),IBMASK,
     +       FDFNAM(1:LENOCC(FDFNAM)),CDFOPT
 9901    FORMAT('  ** Entering D0DAD_STREAM ***',/,
     +          '      Input file: ',A,/,
     +          '      Bit number: ',I8,/,
     +          '      Output file: ',A,',  Option: ',A1)
         IF( ISELR(1).NE.0 ) WRITE(*,9904) 'Run',ISELR(1),ISELR(2)
         IF( ISELE(1).NE.0 ) WRITE(*,9904) 'Event',ISELE(1),ISELE(2)
 9904    FORMAT('      ',A,' range from ',I8,' to ',I10)
         WRITE(*,9903)
 9903    FORMAT(/,/)
      ENDIF
C
C  Open input event catalog...
C
      CALL D0DAD_OPEN(JFEC,FNAME,'R',IDADEC,IERR)
      IF( IERR.NE.0 ) THEN
         IF( LDDBG.GT.0 ) WRITE(*,9001) IERR,'EC'
 9001    FORMAT(' D0DAD_STREAM: Error ',I5,' from D0DAD_OPEN(',A2,')')
         IERR = -1
         GOTO 998
      ENDIF
C
C  Setup the output filenames for the streams...
C
      CDFTAG=CECTAG
      IF(IBMASK.EQ.0) THEN
C  Open a stream definition file...
         CALL D0OPEN(IDADOK,FDFNAM(1:LENOCC(FDFNAM)),'IF',OK)
         IF( .NOT.OK ) GOTO 997
CJDH         OPEN(IDADOK,FILE=FDFNAM,STATUS='OLD',READONLY,ERR=997)
         N=0
         CALL VZERO(MSKBITS,2*NSMAX)
 30      CONTINUE
            READ(IDADOK,'(A)',END=40,ERR=996) STRTMP
            IF( STRTMP(1:1).EQ.'!' ) GOTO 30
            IF( N.GE.NSMAX ) THEN
               IF( LDDBG.GT.1 ) WRITE(*,8002)
 8002          FORMAT(' D0DAD_STREAM: Stream count (',I5,') exceeded')
               IERR = -7
               GOTO 998
            ENDIF
            N=N+1
            CINSTR='                                '
            CALL GETWORD(STRTMP,1,CINSTR)
            I=LENOCC(CINSTR)
            IF(CINSTR(1:1).EQ.'$' ) THEN               ! Hex bitmask
              IF( I.GT.9 ) THEN   
                CTMP=CINSTR(1:I-8)
                CALL D0DAD_INTEGER(CTMP(1:I-8),MSKBITS(2,N),IBTYPE)
                CTMP='$'//CINSTR(2+I-9:I)
                CALL D0DAD_INTEGER(CTMP(1:I),MSKBITS(1,N),IBTYPE)
              ELSE
                CALL D0DAD_INTEGER(CINSTR(1:I),MSKBITS(1,N),IBTYPE)
              ENDIF
            ELSE                                       ! Integer bit number
              CALL D0DAD_INTEGER(CINSTR(1:I),KBIT,IBTYPE)
              IF(IBTYPE.EQ.0) THEN
                I=MOD(KBIT-1,32)
                J=(KBIT-1)/32 + 1
                MSKBITS(J,N)=ISHFT(1,I)
              ENDIF
            ENDIF
            CALL GETWORD(STRTMP,2,CDFILE(N))
            CALL GETWORD(STRTMP,3,COMMENT(N))
         GOTO 30
 40      CONTINUE
         CLOSE(IDADOK)
         NSTREAMS=N
      ELSE
C  Only one single-bit stream...
         NSTREAMS=1
         CDFILE(1)=FDFNAM
         IF( IBMASK.NE.(-1) ) THEN
           J=(IBMASK-1)/32 + 1
           I=MOD(IBMASK-1,32)
           MSKBITS(J,1)=ISHFT(1,I)
         ELSE
           MSKBITS(1,1)=IBMASK
           MSKBITS(2,1)=IBMASK
         ENDIF
      ENDIF
C
C  Open dad files for each stream
C
      IF( LDDBG.GT.4 ) WRITE(*,1001) NSTREAMS
 1001 FORMAT(/,' Defined ',I4,' streams',/,
     +         ' Stream          Mask        File')
      DO I=1,NSTREAMS
        IF(LDDBG.GT.4)WRITE(*,1002)I,MSKBITS(2,I),MSKBITS(1,I),
     +     CDFILE(I)(1:LENOCC(CDFILE(I))),
     +     COMMENT(I)(1:LENOCC(COMMENT(I)))
 1002   FORMAT(2X,I4,5X,2(Z8.8,' '),' ',A50,1X,A45)
        CALL D0DAD_GETFNO(CDFILE(I),CDFOPT)
        CALL D0DAD_OPEN(JFDF,CDFILE(I),CDFOPT,ISTLUN(I),IERR)
        IF( IERR.NE.0 ) THEN
          IF( LDDBG.GT.0 ) WRITE(*,9001) IERR,'DF'
          IERR = -2
          GOTO 998
        ENDIF
      ENDDO
      IF( LDDBG.GT.4 ) WRITE(*,1003)
 1003 FORMAT(/)
C
C  Do the streaming after skipping the header... 
C
      NEV=0
      NALL=0
      CALL VZERO(NSEL,NSMAX)
 10   CONTINUE
         IDRUN=-1
         CALL ECGET(IDADEC,IDRUN,IDEVNT,IstMSK,IFID,IDZRN,IDZBO,IERR)
         IF( IERR.EQ.1 ) GOTO 20
         IF( IERR.LT.0 ) THEN
            IF( LDDBG.GT.0 ) WRITE(*,9003) IERR,IDADEC
 9003       FORMAT(' D0DAD_STREAM: Error ',I3,' from ECGET on unit',I3)
            IERR = -3 
            GOTO 998
         ENDIF
C
C  Check for 'deleted' event.
C
         IF( IFID.LE.0 ) GOTO 10
C
C  Check run and event number range....
C
         LSRUN=ISELR(1).GT.0 .AND. ISELR(2).EQ.0 .AND. IDRUN.GT.ISELR(1)
         IF(LSRUN) THEN
           IERR=1
           GOTO 20
         ENDIF
C
         LSRUN=ISELR(1).GT.0 .AND. ISELR(2).GT.0 .AND. IDRUN.GT.ISELR(2)
         IF(LSRUN) THEN
           IERR=1
           GOTO 20
         ENDIF
C
         IF( .NOT.D0DAD_RE_RANGE(ISELR,ISELE,IDRUN,IDEVNT) ) GOTO 10
C
C  Check bitmask and write to output stream if OK.
C
         NEV=NEV+1
         CALL ECGET_TIMESTAMP(IDADEC,IDATE)
         DO I=1,NSTREAMS
            LSELECT1 = IAND(ISTMSK(1),MSKBITS(1,I)) .NE. 0
            LSELECT2 = IAND(ISTMSK(2),MSKBITS(2,I)) .NE. 0
CJDH "And"           LSELECT1 = IAND(ISTMSK(1),MSKBITS(1,I)) .EQ. MSKBITS(1,I)
CJDH     +         .AND. MSKBITS(1,I).NE.0
CJDH            LSELECT2 = IAND(ISTMSK(2),MSKBITS(2,I)) .EQ. MSKBITS(2,I)
CJDH     +         .AND. MSKBITS(2,I).NE.0
            IF( LSELECT1 .OR. LSELECT2 .OR. IBMASK.EQ.-1 ) THEN
               CALL DFPUT(ISTLUN(I),IDRUN,IDEVNT,IFID,IDZRN,IDZBO,
     >                                                    IDATE,IERR)
               IF( IERR.LT.0 ) THEN
                  IF( LDDBG.GT.0 ) WRITE(*,9004) IERR,CDFILE(I)
 9004             FORMAT(' D0DAD_STREAM: Error',I4,' from DFPUT, on ',A)
                  IERR = -4 
                  GOTO 998
               ENDIF
               NALL=NALL+1
               NSEL(I)=NSEL(I)+1
            ENDIF
         ENDDO
         IF( MOD(NEV,2500).EQ.0 .AND. LDDBG.GT.4) WRITE(*,9902) NEV,NALL
 9902    FORMAT('    Events checked:',I8,', Selected: ',I8)
      GOTO 10
C
 20   CONTINUE
      CALL D0DAD_CLOSE(IDADEC,IERR)
      DO I=1,NSTREAMS
         CALL D0DAD_CLOSE(ISTLUN(I),IERR)
      ENDDO
C
 999  CONTINUE
      IF( LDDBG.GT.4 ) THEN
        WRITE(*,9907) NEV,NALL
 9907   FORMAT('    Events checked:',I8,', Selected: ',I8,/)
        DO I=1,NSTREAMS
           WRITE(*,9906) I,NSEL(I)
 9906      FORMAT('        Stream ',I4,' contains ',I10,' events')
        ENDDO
      ENDIF
      RETURN
C
 996  CONTINUE
      CALL ECCLOS(IDADEC,IERR)
      IERR = -6
      RETURN
C
 997  CONTINUE
      CALL ECCLOS(IDADEC,IERR)
      IERR = -7
      RETURN
C
 998  CONTINUE
      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE DBMU_GTFILE (CDEF,VTIM,CHOP,FILN,LOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will return the name of the DBL3 file, which
C-    have a validity range there include VTIM.
C-    This routine is independent of the DBMU_FILE routines. And is 
C-    intended for use only to fecth the right dbmon dbl3 database for
C-    one time use.
C-
C-   Inputs  : CDEF  (C)    Standard D0 indentifier of a DBL3 data base
C-                          (e.g 'DBM', 'HVM', 'LUM', 'TRD', 'CAL' etc) 
C-             VTIM  (*I)   VMS system time or VTIM(1) = YYMMDD and 
C-                          VTIM(2) = DDMMSS or use it directly (run number
C-                           or dbl3 packed time)
C-             CHOP         ' ' use VTIM directly (dbl3 packed time or
C-                              run number)
C-                          'I' passed time has the format YYMMDD, HHMMSS
C-                          'V' passed time is a VMS system time.
C-   Outputs : FILN  (C)    Name of valid file
C-             LOK   (L)    .true.   it went ok
C-                          .false.  somthing went wrong
C-   Controls: 
C-
C-   Created  25-OCT-1992   Lars Rasmussen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL D3UPT
      INTEGER ICFNBL
C
      CHARACTER *(*)  CDEF,FILN,CHOP
      INTEGER VTIM(*)
      LOGICAL LOK
C
      INTEGER I,J,K,KTIM,IDUM,IUNI,IRET,SV,EV
      INTEGER*2 K2
      CHARACTER FILP*80, CHLP*12, FHLP*48
C----------------------------------------------------------------------
      FILN = ' '
      LOK = .FALSE.
      CALL UPCASE (CHOP,CHLP)
      IF (INDEX (CHLP,'I') .GT. 0) THEN
         CALL DBPKTS(VTIM(1),VTIM(2),KTIM)
      ELSE IF (INDEX(CHLP,'V') .GT. 0) THEN
         IF ( .NOT. D3UPT(VTIM,KTIM)) THEN
            CALL MSGO ('w','DBMU_GTFILE','Time is invalid',0)
            RETURN
         ENDIF
      ELSE
         KTIM = VTIM(1)
      END IF
C
      CHLP = CDEF
      K = ICFNBL(CHLP,1,LEN(CHLP))
      IF (K .LE. 0 .OR. K .GT. LEN(CHLP)) THEN
         CALL MSGO
     &      ('w','DBMU_GTFILE','DBL3 file pointer invalid',0)
         RETURN
      END IF
      CHLP = CHLP(K:)
      CALL STR$TRIM (CHLP,CHLP,K2)
      FILP = 'DBL3$'//CHLP(1:K2)//':'//CHLP(1:K2)//'_DBFILES'
C
      CALL GTUNIT(984,IUNI,IRET)
      IF (IRET .NE. 0) THEN
         CALL MSGO ('en','DBMU_GTFILE','Error from GTUNIT',IRET)
         RETURN
      END IF
      CALL D0OPEN (IUNI,FILP,'IF',LOK)
      IF (.NOT. LOK) RETURN
C
      DO WHILE (.TRUE.)
         READ(IUNI,801,END=11) SV,EV,IDUM,FHLP
         IF (KTIM .GE. SV .AND. KTIM .LE. EV) THEN
            FILN = FHLP
            LOK = .TRUE.
            GOTO 11
         END IF
      END DO
11    CLOSE (IUNI)
      CALL RLUNIT(984,IUNI,IRET)
C
      IF (.NOT. LOK)
     &  CALL MSGO ('w','DBMU_GTFILE','No valid file found',0)
C
  801 FORMAT (2I12,I10,A48)
C
  999 RETURN
      END

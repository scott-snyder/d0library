      SUBROUTINE FCDUPL(ILUN,CFNNEW,IFDUPL,IERR)
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : Check the file catalog on unit ILUN to see 
C-      if any file name entries match CFFNEW.  If one is found, return
C-      the file ID in IFDUPL.  Otherwise, IFDUPL = -1.  If multiple
C-      matches are found, the first is returned, and IERR is set to the
C-      number of matches.  The search is initially started at FILE 
C-      IFDUPL.
C-      
C-   INPUTS  : 
C-   OUTPUTS : 
C-   CONTROLS: 
C-
C-   CREATED  22-DEC-1993   John D Hobbs
C-   Modified 12-MAY-1995   John D Hobbs - Allow duplicate to include
C-     a newer timestamp and MDS-->MDC conversions...
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INTEGER ILUN,IFDUPL,IERR
      CHARACTER*(*) CFNNEW
      CHARACTER*200 CFN,CFO
      CHARACTER*128 CFNNAM,CFNEXT,CFONAM,CFOEXT ! New and Old name and extension
      INTEGER I,ISTART,NFILES,IFLAG,NF,J,K,LNEW,LOLD
      LOGICAL LSAME,LNEWER,LMDC
C
      INTEGER LENOCC
C----------------------------------------------------------------------
C
      CALL FCHRD(ILUN,NFILES,IERR)
      IF( IERR.NE.0 ) GOTO 901
C
      CFN=CFNNEW
      CALL CLTOU(CFN)
      CALL FILENAME_PARSE(CFN,'NAM',CFNNAM,I)
      CALL FILENAME_PARSE(CFN,'EXT',CFNEXT,J)
      CFN = CFNNAM(1:LENOCC(CFNNAM))//CFNEXT(1:LENOCC(CFNEXT))
      LNEW=LENOCC(CFN)
C
      ISTART=MAX(IFDUPL,1)
      IFDUPL = -1
      NF = 0
      DO I=ISTART,NFILES
         CALL FCGET(ILUN,I,CFNTMP,CGNTMP,CTPTMP,CFCTMP,IERR)
         IF( IERR.NE.0 ) GOTO 902
C      Check for already replaced files...
         IF( CFNTMP(1:8).EQ.FCREPL ) GOTO 101
C      Get the name and extension only...
         CALL CLTOU(CFNTMP)
         CALL FILENAME_PARSE(CFNTMP,'NAM',CFONAM,K)
         CALL FILENAME_PARSE(CFNTMP,'EXT',CFOEXT,K)
         CFNTMP=CFONAM(1:LENOCC(CFONAM))//CFOEXT(1:LENOCC(CFOEXT))
         LOLD=LENOCC(CFNTMP)
C     Test for same name/extension
         LSAME = CFNTMP.EQ.CFN    
C     Test for same name/extension w/newer timestamp
         LNEWER = CFNTMP(1:LOLD-8).EQ.CFN(1:LNEW-8)
     >     .AND.  CFNTMP(LOLD-7:LOLD).LT.CFN(LNEW-7:LNEW)
C     Test for MDS --> MDC w/newer timestamp.
         LMDC = CFOEXT(1:6).EQ.'.X_MDS'
         CFOEXT(6:6)='C'
         CFNTMP=CFONAM(1:LENOCC(CFONAM))//CFOEXT(1:LENOCC(CFOEXT))
         LMDC = CFNTMP(1:LNEW-8).EQ.CFN(1:LOLD-8)
     >     .AND. CFNTMP(LOLD-7:LOLD) .LE. CFN(LNEW-7:LNEW)
     >     .AND. LMDC
         IF( LSAME .OR. LNEWER .OR. LMDC) THEN
            IF( IFDUPL.EQ.(-1) ) IFDUPL=I
            NF=NF+1
         ENDIF
 101  ENDDO
      IERR=NF
C
 999  CONTINUE
      RETURN
C
 901  CONTINUE
      IERR = -1
      RETURN
C
 902  CONTINUE
      IERR = -2
      RETURN
      END

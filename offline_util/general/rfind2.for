      INTEGER FUNCTION RFIND2(RKEY,IFIELD,RDAT,NW,NELE,IDIR,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the first occurance of IKEY in field
C-     IFIELD of the sorted 2D array RDAT(NW,NELE).  If IDIR>0, RDAT is 
C-     assumed to be sorted in increasing order.  If IDIR<=0, RDAT is
C-     assumed to be sorted in decreasing order.
C-
C-   Inputs  : IEY    - search key 
C-             IFIELD - Field in catalog record
C-             RDAT(NW,NELE) - data
C-   Outputs : RFIND2 - Index of first occurance if found.  If an
C-               error occurs, RFIND2=0.  If the entry is not found,
C-               RFIND2<0 such that 
C-              RDAT(IFIELD,abs(RFIND2))<IKEY<RDAT(IFIELD,abs(RFIND2)+1)
C-               point if not found
C-             IERR   - 0==>OK, 1==>Not found.  Else ERROR
C-   Controls: 
C-
C-   Created   8-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IFIELD,NW,NELE,IREC,IERR,IDIR
      REAL    RDAT(NW,NELE),RKEY
      INTEGER IMAX,IMIN,ISREC,I
C----------------------------------------------------------------------
C
      IERR=0
      IMIN=1
      IMAX=NELE
C
C  Check bounds...
C
      IF( RDAT(IFIELD,1).GE.RKEY.AND.IDIR.GT.0
     > .OR. RDAT(IFIELD,1).LE.RKEY.AND.IDIR.LE.0
     >) THEN
         IF( RDAT(IFIELD,1).EQ.RKEY ) THEN
            IERR=0
            RFIND2=1
         ELSE
            IERR=1
            RFIND2=-1
         ENDIF
         GOTO 999
      ENDIF
C
      IF( RDAT(IFIELD,NELE).LE.RKEY.AND.IDIR.GT.0
     > .OR. RDAT(IFIELD,NELE).GE.RKEY.AND.IDIR.LE.0
     >) THEN
         IF( RDAT(IFIELD,NELE).EQ.RKEY ) THEN
            IERR=0
            RFIND2=NELE
         ELSE
            IERR=1
            IREC = -NELE
         ENDIF
         GOTO 998
      ENDIF
C
C  Binary search...
C
      ISREC=(IMAX-IMIN)/2 + IMIN
 10   CONTINUE
*
         IF( RDAT(IFIELD,ISREC).EQ.RKEY ) THEN
            IERR=0
            RFIND2=ISREC
            GOTO 998
         ENDIF
*
         IF( ISREC.EQ.IMIN ) THEN 
            RFIND2=-IMIN
            IERR=1
            GOTO 999
         ENDIF
*
         IF( RDAT(IFIELD,ISREC).GT.RKEY.AND.IDIR.GT.0
     >    .OR. RDAT(IFIELD,ISREC).LT.RKEY.AND.IDIR.LE.0
     >   ) THEN
            IMAX=ISREC
         ELSE
            IMIN=ISREC
         ENDIF
*
         ISREC=(IMAX-IMIN)/2 + IMIN
*
      GOTO 10
C
C  Make sure we have FIRST occurance
C
 998  CONTINUE
      IF( IERR.NE.0 ) GOTO 999
 20   CONTINUE
         IF( RDAT(IFIELD,RFIND2-1).NE.RDAT(IFIELD,RFIND2) ) GOTO 999
         IF( RFIND2-1.LE.0 ) GOTO 999
         RFIND2=RFIND2-1
      GOTO 20
C
 999  CONTINUE
      RETURN
      END

      INTEGER FUNCTION IFIND2(IKEY,IFIELD,IDAT,NW,NELE,IDIR,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the first occurance of IKEY in field
C-     IFIELD of the sorted 2D array IDAT(NW,NELE).  If IDIR>0, IDAT is 
C-     assumed to be sorted in increasing order.  If IDIR<=0, IDAT is
C-     assumed to be sorted in decreasing order.
C-
C-   Inputs  : IEY    - search key 
C-             IFIELD - search field 1<=IFIELD<=NW
C-             IDAT(NW,NELE) - data
C-             IDIR   - Ordering. >0 ==> increasing order.
C-   Outputs : IFIND2 - Index of first occurance if found.  If an
C-               error occurs, IFIND2=0.  If the entry is not found,
C-               IFIND2<0 such that 
C-              IDAT(IFIELD,abs(IFIND2))<IKEY<IDAT(IFIELD,abs(IFIND2)+1)
C-               point if not found
C-             IERR   - 0==>OK, 1==>Not found.  Else ERROR
C-   Controls: 
C-
C-   Created   8-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IKEY,IFIELD,NW,NELE,IDAT(NW,NELE),IREC,IERR,IDIR
      INTEGER IMAX,IMIN,ISREC,I
C----------------------------------------------------------------------
C
      IERR=0
      IMIN=1
      IMAX=NELE
C
C  Check bounds...
C
      IF( IDAT(IFIELD,1).GE.IKEY.AND.IDIR.GT.0
     >  .OR. IDAT(IFIELD,1).LE.IKEY.AND.IDIR.LE.0 ) 
     >THEN
         IF( IDAT(IFIELD,1).EQ.IKEY ) THEN
            IERR=0
            IFIND2=1
         ELSE
            IERR=1
            IFIND2=-1
         ENDIF
         GOTO 999
      ENDIF
C
      IF( IDAT(IFIELD,NELE).LE.IKEY.AND.IDIR.GT.0
     >  .OR. IDAT(IFIELD,NELE).GE.IKEY.AND.IDIR.LE.0 ) 
     >THEN
         IF( IDAT(IFIELD,NELE).EQ.IKEY ) THEN
            IERR=0
            IFIND2=NELE
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
         IF( IDAT(IFIELD,ISREC).EQ.IKEY ) THEN
            IERR=0
            IFIND2=ISREC
            GOTO 998
         ENDIF
*
         IF( ISREC.EQ.IMIN ) THEN 
            IFIND2=-IMIN
            IERR=1
            GOTO 999
         ENDIF
*
         IF( IDAT(IFIELD,ISREC).GT.IKEY.AND.IDIR.GT.0
     >     .OR. IDAT(IFIELD,ISREC).LT.IKEY.AND.IDIR.LE.0
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
         IF( IDAT(IFIELD,IFIND2-1).NE.IDAT(IFIELD,IFIND2) ) GOTO 999
         IF( IFIND2-1.LE.0 ) GOTO 999
         IFIND2=IFIND2-1
      GOTO 20
C
 999  CONTINUE
      RETURN
      END

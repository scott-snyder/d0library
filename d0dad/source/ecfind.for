      SUBROUTINE ECFIND(IARR,NMAX,NELE,IKEY,IFIELD,IREC,IDATA,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find a given record within a data array
C-
C-   Inputs  : IARR(NELE,NMAX) - Integer array to search
C-             IKEY   - Integer key (eg, event number)
C-             IFIELD - Field in catalog record
C-   Outputs : IREC   - Index of record found or insertion point if
C-               not found
C-             IDATA  - Data record matching key.
C-             IERR   - 0==>OK, 1==>Not found.  Else ERROR
C-   Controls: 
C-
C-   Created   8-NOV-1993   John D Hobbs
C-   Modified 11-JAN-1994   John D Hobbs - Convert to memory based
C-      search
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NELE,NMAX,IARR(NELE,*),IKEY,IFIELD,IDATA(NELE),IREC
      INTEGER IERR,IMAX,IMIN,ISREC,ISOLD
C----------------------------------------------------------------------
C
      IERR=0
      IMAX=NMAX
      IMIN=1
C
C  Check range...
C
      IF( IMAX.LT.IMIN ) THEN
         IERR=1
         IREC=IMIN
         GOTO 999
      ENDIF
C
C  Check bounds...
C
      IF( IARR(IFIELD,IMIN).GE.IKEY ) THEN
         IF( IARR(IFIELD,IMIN).EQ.IKEY ) THEN
            IERR=0
            IREC=IMIN
         ELSE
            IERR=1
            IREC=IMIN
         ENDIF
         GOTO 999
      ENDIF
C
      IF( IERR.NE.0 ) GOTO 997
      IF( IARR(IFIELD,IMAX).LE.IKEY ) THEN
         IF( IARR(IFIELD,IMAX).EQ.IKEY ) THEN
            IERR=0
            IREC=IMAX
         ELSE
            IERR=1
            IREC=IMAX+1
         ENDIF
         GOTO 999
      ENDIF
C
C  Binary search...
C
      ISREC=(IMAX-IMIN)/2 + IMIN
 10   CONTINUE
*
         IF( IARR(IFIELD,ISREC).EQ.IKEY ) THEN
            IERR=0
            IREC=ISREC
            GOTO 999
         ENDIF
*
         IF( ISREC.EQ.IMIN ) THEN 
            IREC=IMIN+1
            IERR=1
            GOTO 999
         ENDIF
*
         IF( IARR(IFIELD,ISREC).GT.IKEY ) THEN
            IMAX=ISREC
         ELSE
            IMIN=ISREC
         ENDIF
*
         ISREC=(IMAX-IMIN)/2 + IMIN
*
      GOTO 10
C
 999  CONTINUE
      CALL UCOPY(IARR(1,IREC),IDATA,NELE)
      RETURN
C
 996  CONTINUE
      IERR = -1
      RETURN
C
 997  CONTINUE
      IERR = -2
      RETURN
C
 998  CONTINUE
      IERR = -3
      RETURN
      END

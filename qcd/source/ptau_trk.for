      SUBROUTINE PTAU_TRK(LPTAU, WORDS,XDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get tracking information associated with PTAU
C-            
C-    Input : 
C-            lptau         : pointer to ptau bank
C-            words         : dimension of array xdata(words). It should be
C-                            equal to or greater than 6. The argument
C-                            corresponding to words in the calling routine
C-                            should be a variable, e.g.,
C-                              INTEGER NN
C-                              NN = 6  
C-                              CALL PTAU_TRK(NN,X)
C-                        
C-                            CALL PTAU_TRK(6,X) will cause a run-time error. X
C-                            should be an array of at least NN elements.
C-    Output: 
C-            words         : actual # of words filled. if it is 0, then PTAU
C-                            does not exist or error occurs.
C-            Xdata         : If words > 0, then the following is true.
C-             word 1       : NTRK 
C-             word 2       : MINDIST
C-             word 3       : MINXYI
C-             word 4       : MINZI
C-             word 5       : MIP05
C-             word 6       : MIP01
C-                     
C-   Created   2-NOV-1993   Hailin LI   
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER LPTAU,LZTRK,LZFIT,GZPTAU
      REAL    SMALL
      PARAMETER(SMALL=1.0E-5)
      INTEGER NTRK,I,WORDS
      REAL    ETA,PHI,XDATA(WORDS)
      REAL    PHITMP,ETATMP,DIST,DXPHI
      REAL    MIN_DIST,MIN_IMPXY,MIN_IMPZ
      REAL    MIP05,MIP01,MIP
C---------------------------------------------------------------------
      CALL VFILL(XDATA,WORDS,-999.0) ! initialization

      WORDS = 0     ! Set to zero

c      LPTAU=GZPTAU()
      IF(LPTAU.LE.0) THEN
        CALL ERRMSG('No PTAU','PTAU_TRK',' ','W')
        GOTO 999
      ENDIF

C   sort banks so they are in increasing order of Et
C   NOTE: after each reordering of banks the pointer
C   LPTAU must be refetched
c      CALL ZSORT(IXCOM,LPTAU,7)
c      LPTAU=GZPTAU()
c      CALL ZTOPSY(IXCOM,LPTAU)
c      LPTAU=GZPTAU()

      PHI=Q(LPTAU+9)
      ETA=Q(LPTAU+10)

C   Number of tracks associated with PTAU
      NTRK=IQ(LPTAU-3)-IQ(LPTAU-2)-1
      XDATA(1)=NTRK

C   Get tracking information
      MIN_DIST = 999.0
      MIN_IMPXY = 999.0
      MIN_IMPZ = 999.0
      MIP05=0.0
      MIP01=0.0
      DO I=1, NTRK
        LZTRK=LQ(LPTAU-2-I)
        IF(LZTRK.GT.0) THEN
          LZFIT=LQ(LZTRK-1)
          IF(LZFIT.GT.0) THEN

            MIP=Q(LZFIT+26)     ! ionization of the track

C   Minimum distance between a track and the jet direction
            PHITMP=Q(LZFIT+10)
            ETATMP=-ALOG(TAN(Q(LZFIT+13)/2.0)+SMALL)
            DXPHI = ABS(PHITMP-PHI)
            IF ( DXPHI .GT. 7.0 ) THEN
              CALL ERRMSG('Wrong PhiTRK','PTAU_TRK',' ','W')
              GOTO 999
            ENDIF
            IF ( DXPHI .GT. PI ) DXPHI = TWOPI - DXPHI
            DIST = SQRT(DXPHI**2+(ETATMP-ETA)**2)
            IF ( DIST .LT. MIN_DIST ) MIN_DIST = DIST
            IF ( DIST .LE. 0.5 ) MIP05=MIP05+MIP
            IF ( DIST .LE. 0.1 ) MIP01=MIP01+MIP

C   Minimum impact parameter in X-Y plane
            IF ( Q(LZFIT+32) .LT. MIN_IMPXY ) MIN_IMPXY = Q(LZFIT+32)

C   Minimum distance to Vertex_Z along the Z-axis
            IF ( ABS(Q(LZFIT+33)) .LT. ABS(MIN_IMPZ) )
     &        MIN_IMPZ = Q(LZFIT+33)

          ENDIF
        ENDIF
      ENDDO

      XDATA(2) = MIN_DIST
      XDATA(3) = MIN_IMPXY
      XDATA(4) = MIN_IMPZ
      IF (MIP05.GT.0.0) XDATA(5)=MIP05
      IF (MIP01.GT.0.0) XDATA(6)=MIP01

      WORDS = 6
  999 RETURN
      END

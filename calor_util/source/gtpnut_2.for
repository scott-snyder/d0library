      SUBROUTINE GTPNUT_2(NUM, ETSCALAR, SIGEZ2,
     &  CORRXY, CORRXZ, CORRYZ, IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      get MORE contents of PNUT banks
C-
C-   Inputs  :
C-      NUM = particle number
C-   Outputs :
C-      ETSCALAL  [R]   Scalar ET
C-      SIGEZ2    [R]   (sigEz)**2
C-      CORRXY    [R]   <dExdEy>
C-      CORRXZ    [R]   <dExdEz>
C-      CORRYZ    [R]   <dEydEz>
C-      IER    = Error flag- 0: OK, <0: not OK
C-
C-   Created   6-FEB-1989   Serban D. Protopopescu
C-   Updated  19-JUN-1991   Andrew J. Milder
C-                        Modified for MicroDST format -- check PATH
C-   Updated  19-DEC-1992   Andrew J. Milder  New MDST format
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      CHARACTER*4 PATH
      REAL SIGEX2, SIGEY2, ETSCALAR, SIGEZ2, CORRXY, CORRXZ, CORRYZ
      REAL    ENUT(5),ET,TH,ETA,PHI,SIG(3)
      INTEGER NUM,GZPNUT,LPNUT,I,NUM_PNUT,IER,NZBANK,LMDST,GZMDST
      INTEGER NREP,IOFF,NUM_NUT, IVERS
C----------------------------------------------------------------------
      CALL PATHGT(PATH)
      IF (PATH.EQ.'MDST') THEN
        LMDST = GZMDST()
        IF (LMDST .LE. 0) THEN
          IER = -4
          GOTO 999
        ENDIF
        NUM_PNUT=IQ(LMDST+9)
        IF( NUM.GT.NUM_PNUT) THEN
          IER = -5
          GOTO 999
        ENDIF
        NREP=IQ(LMDST+8)
        IOFF=IQ(LMDST+10)+(NUM-1)*NREP-1
        LPNUT = LMDST + IOFF
        IF ( LPNUT .GT. 0 ) IVERS = NINT( Q( LPNUT + 1 ) )
      ELSE
        IER = 0
        LPNUT=GZPNUT(NUM)
        IF ( LPNUT .GT. 0 ) IVERS = IQ( LPNUT + 1 )
      ENDIF
      SIGEZ2 = -999.
      ETSCALAR= -999.
      CORRXY  = -999.
      CORRXZ  = -999.
      CORRYZ  = -999.
      IF(LPNUT.GT.0) THEN
        IF ( IVERS .GE. 3 ) THEN
          ETSCALAR= Q( LPNUT + 14 )
          SIGEZ2 = Q( LPNUT + 15 )
          CORRXY = Q( LPNUT + 16 )
          CORRXZ = Q( LPNUT + 17 )
          CORRYZ = Q( LPNUT + 18 )
        ELSEIF ( IVERS .GE. 2 ) THEN
          ETSCALAR= Q( LPNUT + 14 )
        ENDIF
      ELSE
        IER = -4
      ENDIF
  999 RETURN
      END

C DEC/CMS REPLACEMENT HISTORY, Element PRISP2.FOR
C *3     8-DEC-1987 13:36:54 JONCKHEERE "Add daughter vertex printout/use .LINKs"
C *2    19-MAY-1987 16:16:21 RAJA "fixed bug"
C *1    19-DEC-1986 16:24:11 JONCKHEERE "Moved from D0GEANT"
C DEC/CMS REPLACEMENT HISTORY, Element PRISP2.FOR
      SUBROUTINE PRISP2(PRUNIT,LISP2I,NISP2,CFL,IFL)
C------------------------------------------------------------------
C-
C-  Print out for ISP2 (particle) bank
C-
C-
C-  INPUT:
C-  PRUNIT = unit number for printout
C-  LISP2I = bank address
C-  NISP2  = bank number
C-  CFL    = flag to control printout
C-           'ALL' for all banks, 'LINEAR' for one linear structure
C-           'ONE' for one bank only
C-            LISP2I must be provided for 'LINEAR',
C-            LISP2I or NISP2 may be provided for 'ONE',
C-            LISP2I and NISP2 ignored for 'ALL'
C-  IFL    = not used
C-
C-     SDP  Jan,1986 , Rev. Feb,1986
C-     SK   Apr,1986 , modified from SDP's PRISP1
C-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZISAE.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISV2.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISP2.LINK/LIST'
      INTEGER PRUNIT,LISP2I,NISP2,IFL
      CHARACTER CFL*(*)
      INTEGER LZLOC,LISAE,LISV2,LISP2
      INTEGER LISAQ,LISAJ,IDAUV
      INTEGER NP,NV,K,K1,K2,IOR
      CHARACTER*8 NAME,LABEL
C
      LISP2 = LISP2I
C
C          Print titles
C
      WRITE(PRUNIT,100)
C
      IF ( CFL.EQ.'ONE' ) THEN
        IF ( LISP2.LE.0 .AND. NISP2.GT.0 )
     &     LISP2 = LZLOC(IXMAIN,'ISP2',NISP2)
      ENDIF
C
      IF ( CFL.EQ.'ALL' ) THEN
        IF ( LHEAD.NE.0 ) LISAE = LQ(LHEAD-IZISAE)
        IF ( LISAE.NE.0 ) LISV2 = LQ(LISAE-IZISV2)
        IF ( LISV2.NE.0 ) LISP2 = LQ(LISV2-IZISP2)
        IF ( LISAE.LE.0 .OR. LISV2.LE.0 ) RETURN
      ENDIF
C
    1 IF ( LISP2.GT.0 ) THEN
C
C               find daughter vertex
        IDAUV = LQ(LISP2-4)
        IF ( IDAUV.GT.0 ) IDAUV = IQ(IDAUV-5)
C          find the supporting vertex
        IOR = LQ(LISP2+1)
        NV = IQ(IOR-5)
C
C   Print contents of bank
C
        NP = IQ(LISP2-5)
        K1 = LISP2+2
        K2 = LISP2+9
        NAME = LABEL(IQ(LISP2+1))
        WRITE(PRUNIT,101) NP,NAME(1:4),NV,IDAUV,(Q(K),K=K1,K2)
C
        IF ( CFL.NE.'ONE' ) THEN  ! FIND NEXT BANK IN LINEAR STRUCTURE
          LISP2 = LQ(LISP2)
          GOTO 1
        ENDIF
C
      ENDIF
C
      IF ( CFL.EQ.'ALL' ) THEN    ! FIND NEXT VERTEX
        LISV2 = LQ(LISV2)
        IF ( LISV2.GT.0 ) THEN
          LISP2 = LQ(LISV2-IZISP2)
          GOTO 1
        ENDIF
      ENDIF
C
      RETURN
  100 FORMAT(/' PARTICLE BANKS (ISP2)'/
     1 '  NO. NAME   VTX# DAUV',7X,'PX',8X,'PY',8X,'PZ',9X,'E'
     2 ,6X,'MASS',7X,'PHI',5X,'THETA',7X,'ETA')
  101 FORMAT(I4,2X,A4,2X,2I5,2X,5F10.3,3F10.4)
      END

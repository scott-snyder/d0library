      SUBROUTINE ISREF(IT,ITY,LVREF,LPREF)
C-----------------------------------------------------------------------
C-    ISREF gets reference addresses in ISV1 (or ISV2) and ISP1 (or
C- ISP2,ISP3) for IT-th particle in JKINE bank.
C-
C- Input:  IT    = particle number for which reference addresses will
C-                 be requested.
C-
C- Output: ITY   = track type of IT-th particle
C-         LVREF = address of ISV1 or ISV2 or ISP3 for IT-th particle
C-                 in JKINE bank.
C-         LPREF = address of ISP1 or ISP2 for IT-th particle.
C-
C-  S.Kunori   09-Apr-86
C-   Updated   1-MAR-1988   Ghita Rahal-Callot  :  Change from 1000 to 10000
C-                          the maximum nuber of vertices in one interaction
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INCLUDE 'D0$LINKS:IZISAE.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISV1.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISV2.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISP1.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISP2.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISP3.LINK/LIST'
C
      INTEGER NVXALW
      DATA NVXALW / 10000 /
      INTEGER LZFIND
      INTEGER IT,LVREF,LPREF
      INTEGER LISAE
      INTEGER IPART,NVERT,NUBUF
      REAL    VERT(3),PVERT(4),UBUF(10)
      INTEGER JEV,JV,JT,II,ITY
C
C-----------------------------------------------------------------------
C  --get IT-th particle...
      CALL GFKINE(IT,VERT,PVERT,IPART,NVERT,UBUF,NUBUF)
C
      LVREF = 0
      LPREF = 0
C  --get interaction no.(JEV),   vertex number (JV), track number (JT).
      JV = NINT(UBUF(4))
      JEV = JV / NVXALW
      JV  = MOD(JV,NVXALW)
      JT  = NINT(UBUF(5))
      IF ( JV.EQ.0 .OR. JT.EQ.0 ) GO TO 900
C  --get pointer to ISAE bank...
      IF ( JEV.EQ.0 ) THEN
        LISAE = LQ(LHEAD-IZISAE)
      ELSE
        LISAE = LZFIND(IXCOM,LQ(LHEAD-IZISAE),JEV,-5)
      ENDIF
C  --get structual link number depending on track type (ITY)...
      ITY = NINT(UBUF(3))    ! Track creation type
      IF ( ITY.LT.10 ) THEN
        II = IZISV1
      ELSEIF ( ITY.GE.20 ) THEN
        II = IZISP3
        LVREF = 0
        LPREF = LZFIND(IXCOM,LQ(LISAE-II),JT,-5)
        GOTO 900
      ELSE
        II = IZISV2
      ENDIF
C  --now find address for reference link...
      LVREF = LZFIND(IXCOM,LQ(LISAE-II),JV,-5)
      IF ( LVREF.GT.0 ) THEN
        IF ( II.EQ.IZISV1 ) THEN
          LPREF = LZFIND(IXCOM,LQ(LVREF-IZISP1),JT,-5)
        ELSEIF ( II.EQ.IZISV2 ) THEN
          LPREF = LZFIND(IXCOM,LQ(LVREF-IZISP2),JT,-5)
        ENDIF
      ENDIF
C
  900 CONTINUE
      RETURN
      END

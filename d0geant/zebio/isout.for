      SUBROUTINE ISOUT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : retrieves vertices and particles generated in
C-                         Geant and creates ISV2, ISP2, ISP3 banks.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  xx-APR-1986   S. Kunori
C-   Updated  17-JUN-1988   Ghita Rahal-Callot  : stores PHI, THETA, ETA
C-                          for the vertices and particles created in GEANT
C-   Updated  28-NOV-1988   Alan M. Jonckheere  : Transfer Parton number to
C-                              daughter
C-   Updated  28-APR-1992   K. Wyatt Merritt  Rewritten for GEANT 3.14 
C-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCNUM.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBIO.INC'
      INCLUDE 'D0$INC:D0LOG.INC'
C
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$LINKS:IZISV2.LINK'
      INCLUDE 'D0$LINKS:IZISP2.LINK'
      INCLUDE 'D0$LINKS:IZISP3.LINK'
C
      LOGICAL FCALL
C
      INTEGER NUBUFF,NUBUF
      PARAMETER( NUBUFF=7 )
      REAL UBUF(NUBUFF),BBUF(NUBUFF)
      INTEGER I
      INTEGER LZFIND,LZLOC
      INTEGER IPCISA,IPTYPE,VIO2,PIO2,PIO3,ISTATS
      INTEGER ITRA,IPART,NVERT,IDAUV
      INTEGER LISAE,LISV1,LISV2,LISP1,LISP2,LISP3,II
      INTEGER LVREF,LPREF,JPTYPE
      INTEGER NEVNT1
      INTEGER NV,IV,JV,NT,IT,IVERT
      INTEGER NTBEAM,NTTARG
C
      REAL    VERT(3),PVERT(4),THETA,PHI,ETA,PP
      REAL    TOFG
C
      DATA NEVNT1/0/
      DATA FCALL /.TRUE./
C
C----------------------------------------------------------------------
C
      NEVNT1 = NEVNT1 + 1
      IF ( IWRUNI.EQ.0 ) GO TO 900
C
      IF ( FCALL ) THEN
        CALL MZFORM('ISV2','1I 8F 1I',VIO2)
        CALL MZFORM('ISP2','1I-F',PIO2)
        CALL MZFORM('ISP3','1I 8F 1I',PIO3)
        FCALL = .FALSE.
      ENDIF
C
      DO 200 I = 1 , 3
        BBUF(I) = -1.
  200 CONTINUE
      NV = NVERTX
      DO 290 IV = 1,NV
*
* ***   For each vertex in turn ..
*
        CALL GFVER1(IV,VERT,TOFG,NTBEAM,NTTARG,NT)
        IF (NT.LE.0) THEN
          GO TO 290
        ENDIF
*
*  **   Loop over tracks attached to current vertex
*
        DO 190 IT = 1,NT
          CALL GFTVER(IV,IT,ITRA)
          CALL GFKINE (ITRA, VERT, PVERT, IPART, IVERT, UBUF, NUBUF)
          IF (IVERT.NE.IV) THEN
            CALL ERRMSG('D0GEANT','ISOUT','Abnormal track/vertex link',
     &        'W')
            GO TO 190
          ENDIF
          IF ( NINT(UBUF(5)).NE.0 ) GO TO 190 ! No more banks needed for
                                              ! ISAJET track
          LISAE = LQ(LHEAD - IZISAE)
          PP = SQRT(PVERT(1)**2 + PVERT(2)**2 + PVERT(3)**2)
          IPTYPE = NINT(UBUF(3))
          IF (IPTYPE.GE.20 .AND. IPTYPE.LT.900) GO TO 185
C           Check if the secondary vertex associated with this track
C           already has created an ISV2 bank
          LISV2 = LZFIND(IXCOM,LQ(LISAE-IZISV2),IVERT,-5)
          IF (LISV2 .LE. 0) THEN  ! No, create the bank
            IQ(LISAE+9) = IQ(LISAE+9) + 1
C
            CALL MZBOOK(IXMAIN,LISV2,LISAE,-IZISV2,'ISV2',4,1,10,
     &          VIO2,0)
            IQ(LISV2-5) = IVERT                     ! set bank number.
            IQ(LISV2+1) = 0
            Q(LISV2+2) = PVERT(1)
            Q(LISV2+3) = PVERT(2)
            Q(LISV2+4) = PVERT(3)
            Q(LISV2+5) = PP
            Q(LISV2+6) = 0.
            Q(LISV2+7) = VERT(1)
            Q(LISV2+8) = VERT(2)
            Q(LISV2+9) = VERT(3)
            IQ(LISV2+10) = IPTYPE
C
C ****  create reference link...
            II = NINT(UBUF(2))
            IF ( II.NE.0 ) THEN
              CALL ISREF(II,JPTYPE,LVREF,LPREF)
              LQ(LISV2-2) = LVREF
              LQ(LISV2-3) = LPREF
              IF ( LPREF.GT.0 ) THEN
                IQ(LISV2+1) = IQ(LPREF+1)   ! ISAJET particle ID
                Q(LISV2+6) = Q(LPREF+6)     ! ISAJET particle mass
              ENDIF
            ENDIF
          ELSE
            Q(LISV2+2) = Q(LISV2+2) + PVERT(1) ! Running sum of secondary mom
            Q(LISV2+3) = Q(LISV2+3) + PVERT(2)
            Q(LISV2+4) = Q(LISV2+4) + PVERT(3)
            Q(LISV2+5) = SQRT(Q(LISV2+2)**2 + Q(LISV2+3)**2 +
     &        Q(LISV2+4)**2)
          ENDIF
          IF (NINT(UBUF(3)).GE.900) GO TO 190 ! No ISP2 for stopping track
  185     IQ(LISAE + 8) = IQ(LISAE + 8) + 1
          BBUF(4) = FLOAT(IVERT)
          BBUF(5) = FLOAT(ITRA)
          BBUF(6) = UBUF(6)
          BBUF(7) = UBUF(7)
          CALL ISUBFN(ITRA,NUBUFF,BBUF)
          IF (PP .LT. SSEC) GO TO 190
          IF ( IPTYPE.GE.20 .AND. IPTYPE.LT.900 ) THEN
C             Punchthrough, knock-on, or bremsstrahlung in MCEN
            CALL MZBOOK(IXMAIN,LISP3,LISAE,-IZISP3,'ISP3',4,1,10,PIO3,0)
            IQ(LISP3-5) = ITRA                      ! set bank number.
            CALL GEAISA(IPART,IPCISA)
            IQ(LISP3+1) = IPCISA
            Q(LISP3+2) = PVERT(1)
            Q(LISP3+3) = PVERT(2)
            Q(LISP3+4) = PVERT(3)
            Q(LISP3+5) = PVERT(4)
            Q(LISP3+6) = SQRT(PVERT(4)**2-PP**2)
            Q(LISP3+7) = VERT(1)
            Q(LISP3+8) = VERT(2)
            Q(LISP3+9) = VERT(3)
            IQ(LISP3+10) = IPTYPE
C
C ****  create parent reference links...
            II = NINT(UBUF(2))
            IF ( II.NE.0 ) THEN
              CALL ISREF(II,JPTYPE,LVREF,LPREF)
              LQ(LISP3-2) = LVREF
              LQ(LISP3-3) = LPREF
            ENDIF
          ELSE    ! Secondary from decay or interaction in MCEN
C
            CALL MZBOOK(IXMAIN,LISP2,LISV2,-IZISP2,'ISP2',4,1,9,PIO2,0)
            IQ(LISP2-5) = ITRA                      ! set bank number
            CALL GEAISA(IPART,IPCISA)               ! convert particle id.
            IQ(LISP2+1) = IPCISA
            Q(LISP2+2) = PVERT(1)
            Q(LISP2+3) = PVERT(2)
            Q(LISP2+4) = PVERT(3)
            Q(LISP2+5) = PVERT(4)
            Q(LISP2+6) = SQRT(PVERT(4)**2-PP**2)
            CALL SLOPAN ( PVERT, THETA, PHI )
            Q(LISP2+7) = PHI
            Q(LISP2+8) = THETA
            IF ( PVERT(4) .GT. ABS(PVERT(3)) ) THEN
              Q(LISP2+9) = .5 * ALOG ( (PVERT(4)+PVERT(3)) /
     &                     (PVERT(4)-PVERT(3)) )
            ENDIF

          ENDIF
  190   CONTINUE
C
  290 CONTINUE
C
C   Loop AGAIN over tracks to put in daughter references
      DO 390 IV = 1,NV
*
* ***   For each vertex in turn ..
*
        CALL GFVER1(IV,VERT,TOFG,NTBEAM,NTTARG,NT)
        IF (NT.LE.0) THEN
          GO TO 390
        ENDIF
*
*  **   Loop over tracks attached to current vertex
        DO 380 IT = 1 , NT
          CALL GFTVER(IV,IT,ITRA)
          CALL GFKINE (ITRA, VERT, PVERT, IPART, IVERT, UBUF, NUBUF)
          IDAUV = NINT(UBUF(1))
          IF (IDAUV .LE. 0) GO TO 380
          IPTYPE = NINT(UBUF(3))
          IF ( IPTYPE.LT.10 ) THEN
            LPREF = LZLOC(IXMAIN,'ISP1',ITRA)
          ELSEIF ( IPTYPE.LT.20 ) THEN
            LPREF = LZLOC(IXMAIN,'ISP2',ITRA)
          ELSEIF ( IPTYPE.LT.900 ) THEN
            LPREF = LZLOC(IXMAIN,'ISP3',ITRA)
          ELSE
            GO TO 380                  ! stopping track - no daughter
          ENDIF
          LVREF = LZLOC(IXMAIN,'ISV2',IDAUV)
          IF ( LPREF.GT.0 ) LQ(LPREF-4) = LVREF
  380   CONTINUE
*
  390 CONTINUE
C
C
C ****  Sort by vertex and track #'s
      LISAE = LQ(LHEAD-IZISAE)
      LISV1 = LQ(LISAE-IZISV1)
      IF ( LISV1.GT.0 ) THEN
        CALL ZSORTI(IXCOM,LISV1,-5)
        LISV1 = LQ(LISAE-IZISV1)
  300   IF ( LISV1.GT.0 ) THEN
          LISP1 = LQ(LISV1-IZISP1)
  310     IF ( LISP1.GT.0 ) THEN
            CALL ZSORTI(IXCOM,LISP1,-5)
          ENDIF
          LISV1 = LQ(LISV1)
          GOTO 300
        ENDIF
      ENDIF
C
      LISV2 = LQ(LISAE-IZISV2)
      IF ( LISV2.GT.0 ) THEN
        CALL ZSORTI(IXCOM,LISV2,-5)
        LISV2 = LQ(LISAE-IZISV2)
  400   IF ( LISV2.GT.0 ) THEN
C
C ****  DROP Vertices where the sum of secondary momenta is less than SSEC and
C ****  where the parent track is missing (has energy less than SSEC)
          IF ( Q(LISV2+5).LT.SSEC .AND. LQ(LISV2-3).LE.0 ) THEN
            CALL MZDROP(IXCOM,LISV2,' ')
            LISV2 = LQ(LISV2)
            GOTO 400
          ENDIF
c
          LISP2 = LQ(LISV2-IZISP2)
  410     IF ( LISP2.GT.0 ) THEN
            CALL ZSORTI(IXCOM,LISP2,-5)
          ENDIF
          LISV2 = LQ(LISV2)
          GOTO 400
        ENDIF
      ENDIF
C
      LISP3 = LQ(LISAE-IZISP3)
      IF ( LISP3.GT.0 ) THEN
        CALL ZSORTI(IXCOM,LISP3,-5)
      ENDIF
C
C  .............debug.......................................................
      IF ( IZBOPT(4).NE.0 .AND. NEVNT1.LE.IZBOPT(2) ) THEN
        WRITE(LOUT,*)
     &      ' ===== EVENT DUMP IN S/R ISOUT ===== NEVNT1 = ',NEVNT1
        CALL DZSURV('IN ISOUT',IXCOM,LHEAD)
        LISAE = LQ(LHEAD-IZISAE)
        IF ( IZBOPT(4).GE.2 ) THEN
          IF (LISAE.GT.0) CALL PRTEVZ(6,0,LISAE,0)
          IF ( IZBOPT(4).GE.3 ) THEN
            CALL GPVERT(0)
            CALL GPKINE(0)
          ENDIF
        ENDIF
      ENDIF
C  .........................................................................
  900 CONTINUE
C ****  Drop all banks except raw data banks
      IF ( SD0(2).NE.0 ) THEN
        CALL ISRAW(SD0(2))
      ENDIF
C
C  write out event data...
      CALL WRZEB(ISTATS)
C
  999 RETURN
 1002 FORMAT (' ISOUT : Abnormal track/vertex connection',2I8)
      END

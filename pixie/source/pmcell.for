      SUBROUTINE PMCELL(NMURAW,IVIEW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To display the raw hit cells 
C-
C-   Inputs  : IVIEW---SEE PMEVNT
C-
C-   Created  26-MAR-1990   Carol C. Francis
C-   DH   4/90 ALLOW VIEWS 7-12
C    DH   4/90 use MUPACK
C    DH   5/90 FIX ODD CELL
C    TMcK 16-DEC-1993 GTMUD1 calls removed for Run 1B compatibility
C
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C    Variable declarations
C    =====================
      INTEGER I,K,II,JJ,IHIT,JHIT
      INTEGER NMURAW,IVIEW,IWADDO,NRAW
C      INTEGER IWADD,IEPHA,IEPHB,IDR1,IDR2,IOPHA,IOPHB   !GTMUD1
C      INTEGER IDELT1,IDELT2             ! GTMUD1
      INTEGER LMUD1,NLAT,MUNMOD
      INTEGER NMOD,NPLN,NCEL,JERR,NWIR,IADC(8) ! MUADD
      INTEGER NSPAR,NBUF,IBUF(10)              ! MUCELL
      REAL SPAR(3),XPAR(3),ROTM(3,3),VOFF      ! MUCELL
      REAL SPARM(3)
      REAL XCWIR,YCWIR,ZCWIR,DX,DY,DZ
      CHARACTER*4 HSHAPE                       ! MUCELL
      
C
C    Executable code
C    ===============
C FIRST LOOP OVER ALL MODULES THEN OVER HITS WITHIN MODULES
C TMcK
C
      CALL MUDMOD(0,NRAW,JHIT,LMUD1)     !INITIALIZE MUDMOD
      CALL MUDHIT(0,JHIT,NCEL,NLAT,IADC) !INITIALIZE MUDHIT
      DO 81 K = 1,164
C
C CALL TO GTMUD1 MUST BE REMOVED FOR COMPATIBILITY WITH NEW MUD1 FORMAT
C           CALL GTMUD1(K,IWADD,IEPHA,IEPHB,IDR1,IDR2,IOPHA,IOPHB,
C     X                 IDELT1,IDELT2)
C           CALL MUADD(IWADD,NMOD,NPLN,NCEL,JERR)
C
        NMOD = MUNMOD(1,K)  ! index -> module number
        IF (NMOD .EQ. -1) RETURN  !bad module number
        CALL MUDMOD(NMOD,NRAW,JHIT,LMUD1)
        DO 70 I = 1,NRAW
          IHIT=JHIT
          CALL MUDHIT(IHIT,JHIT,NCEL,NLAT,IADC)
          NWIR = NCEL/4
          NPLN = NCEL - NWIR*4

C           IF (JERR .EQ. 0) THEN
          CALL MUCELL(NMOD,NPLN,NCEL,HSHAPE,NSPAR,SPAR,
     X                   XPAR,ROTM,VOFF,NBUF,IBUF)
          IF ((NSPAR .NE. 0) .AND. (HSHAPE .NE. '    ')) THEN
            DO 25 II = 1,3
              SPARM(II) = 0.
              DO 40 JJ = 1,3
                SPARM(II) = SPARM(II) + SPAR(JJ)*ROTM(II,JJ)
   40         CONTINUE
   25       CONTINUE
            XCWIR = XPAR(1)
            YCWIR = XPAR(2)
            ZCWIR = XPAR(3)
            DX = ABS(SPARM(1))
            DY = ABS(SPARM(2))
            DZ = ABS(SPARM(3))
          ENDIF
          IF (IVIEW .EQ. 1)
     X         CALL PXRECT('RED',ZCWIR,YCWIR,XCWIR,DZ,DY)
          IF (IVIEW .EQ. 2)
     X         CALL PXRECT('RED',XCWIR,YCWIR,ZCWIR,DX,DY)
          IF (IVIEW .EQ. 3)
     X         CALL PXRECT('RED',XCWIR,ZCWIR,YCWIR,DX,DZ)
          IF (IVIEW .EQ. 4) 
     X         CALL PXRECT('RED',YCWIR,ZCWIR,XCWIR,DY,DZ)
          IF (IVIEW .EQ. 5)
     X         CALL PXRECT('RED',YCWIR,XCWIR,ZCWIR,DY,DX)
          IF (IVIEW .EQ. 6)
     X         CALL PXRECT('RED',ZCWIR,XCWIR,YCWIR,DZ,DX)
          IF (IVIEW .EQ. 7)
     X         CALL PXRECT('RED',ZCWIR,-YCWIR,XCWIR,DZ,DY)
          IF (IVIEW .EQ. 8)
     X         CALL PXRECT('RED',XCWIR,-YCWIR,ZCWIR,DX,DY)
          IF (IVIEW .EQ. 9)
     X         CALL PXRECT('RED',XCWIR,-ZCWIR,YCWIR,DX,DZ)
          IF (IVIEW .EQ. 10) 
     X         CALL PXRECT('RED',YCWIR,-ZCWIR,XCWIR,DY,DZ)
          IF (IVIEW .EQ. 11)
     X         CALL PXRECT('RED',YCWIR,-XCWIR,ZCWIR,DY,DX)
          IF (IVIEW .EQ. 12)
     X         CALL PXRECT('RED',ZCWIR,-XCWIR,YCWIR,DZ,DX)
C           ENDIF
          CALL MUPACK(NMOD,NPLN,NCEL+1,IWADDO,JERR)
          CALL MUADD(IWADDO,NMOD,NPLN,NCEL,JERR)
          IF (JERR .EQ. 0) THEN
            CALL MUCELL(NMOD,NPLN,NCEL,HSHAPE,NSPAR,SPAR,
     X                   XPAR,ROTM,VOFF,NBUF,IBUF)
            IF ((NSPAR .NE. 0) .AND. (HSHAPE .NE. '    ')) THEN
              DO 26 II = 1,3
                SPARM(II) = 0.
                DO 41 JJ = 1,3
                  SPARM(II) = SPARM(II) + SPAR(JJ)*ROTM(II,JJ)
   41           CONTINUE
   26         CONTINUE
              XCWIR = XPAR(1)
              YCWIR = XPAR(2)
              ZCWIR = XPAR(3)
              DY = ABS(SPARM(2))
              DZ = ABS(SPARM(3))
            ENDIF
            IF (IVIEW .EQ. 1)
     X         CALL PXRECT('RED',ZCWIR,YCWIR,XCWIR,DZ,DY)
            IF (IVIEW .EQ. 2)
     X         CALL PXRECT('RED',XCWIR,YCWIR,ZCWIR,DX,DY)
            IF (IVIEW .EQ. 3)
     X         CALL PXRECT('RED',XCWIR,ZCWIR,YCWIR,DX,DZ)
            IF (IVIEW .EQ. 4) 
     X         CALL PXRECT('RED',YCWIR,ZCWIR,XCWIR,DY,DZ)
            IF (IVIEW .EQ. 5)
     X         CALL PXRECT('RED',YCWIR,XCWIR,ZCWIR,DY,DX)
            IF (IVIEW .EQ. 6)
     X         CALL PXRECT('RED',ZCWIR,XCWIR,YCWIR,DZ,DX)
            IF (IVIEW .EQ. 7)
     X         CALL PXRECT('RED',ZCWIR,-YCWIR,XCWIR,DZ,DY)
            IF (IVIEW .EQ. 8)
     X         CALL PXRECT('RED',XCWIR,-YCWIR,ZCWIR,DX,DY)
            IF (IVIEW .EQ. 9)
     X         CALL PXRECT('RED',XCWIR,-ZCWIR,YCWIR,DX,DZ)
            IF (IVIEW .EQ. 10) 
     X         CALL PXRECT('RED',YCWIR,-ZCWIR,XCWIR,DY,DZ)
            IF (IVIEW .EQ. 11)
     X         CALL PXRECT('RED',YCWIR,-XCWIR,ZCWIR,DY,DX)
            IF (IVIEW .EQ. 12)
     X         CALL PXRECT('RED',ZCWIR,-XCWIR,YCWIR,DZ,DX)
          ENDIF
   70   CONTINUE  !loop over hits in each module
   81 CONTINUE  !loop over each module

  999 RETURN
      END

      SUBROUTINE MPAD_END
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determine mean pad-wire resolutions
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-FEB-1992   c.r.murphy
C-   1/93 DH remove ezpick
C-   7/94 MF change name from PADSUMRY
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER EMOD,IMOD,NUMMODS,NTRK
      INTEGER NMODUL,MODNMS(400),MUNMOD3,LMUNMOD,MLIST0(164)
      INTEGER I,J,L,IERR,DUM,LUN,SSUNIT
      REAL C,AV,RES,CHISQ,SIG(3)
      REAL RES_3MISS
      INTEGER HID
      REAL V(164)
      CHARACTER*80 TITL
      DATA V/164*0./
      DATA MLIST0/10,11,12,13, 15,16, 20,21,22,23, 25,26,
     &           30,31,32,33, 35,36, 61,62, 64,65, 67, 91,92, 94,95, 97,
     &           100,101,102,103,104,105,106,107,
     &           110,111,112,113,114,115,116,117,
     &           120,121,122,123,124, 127,
     &           130,131,132,133,134,135,136,137,
     &           140,141,142,143,144,145,146,147, 150, 153,
     &           160,161,162,163,164,165,166,167, 180, 183,
     &           190,191,192,193,194,195,196,197,
     &           200,201,202,203,204,205,206,207,
     &           210,211,212,213,214,215,216,217,
     &           220,221,222,223,224, 227,
     &           230,231,232,233,234,235,236,237,
     &           240,241,242,243,244,245,246,247, 250,251, 253, 255,
     &           260,261,262,263,264,265,266,267,
     &           270,271,272,273,274,275,276,277, 280,281, 283, 285,
     &           290,291,292,293,294,295,296,297,
     &           300,301,302,303,304,305,306,307/
C
C
C
      CALL EZGET('LMUMOD',LMUNMOD,IERR)
      IF (LMUNMOD.EQ.4) THEN
        CALL EZGSET('KMUMOD()',NMODUL,1)   !# of entries in array KMUMOD.
        CALL EZGSET('KMUMOD',MODNMS,1)     !local array containing mods.
        NUMMODS=MUNMOD3(-NMODUL,MODNMS)    !initializes MUNMOD3 so that-
      ENDIF                                !NUMMODS=NMODUL
      NUMMODS=MUNMOD3(0,DUM)
      LUN=SSUNIT()
C
      WRITE(LUN,400)
  400 FORMAT(//,16X,'**********PAD RESOLUTIONS FOLLOW**********',/)
      WRITE(LUN,500)
  500 FORMAT(' MOD',5X,'RESOLUTION(CM)',5X,' # TRACKS USED',/)
C
      DO 100 I=1,NUMMODS
        EMOD=MUNMOD3(1,I)
        DO J=1,164
          IF (EMOD.EQ.MLIST0(J)) IMOD=J
        ENDDO  
C
C
        HID=40000+EMOD
        CALL HNOENT(HID,NTRK)
        IF (NTRK.LT.100) THEN
          WRITE(LUN,601)EMOD,NTRK
        ELSE
          CALL HFITGA(HID,C,AV,RES,CHISQ,102,SIG)
          RES_3MISS=SQRT(2./3.)*RES
          V(IMOD)=RES_3MISS
          WRITE(LUN,600) EMOD,RES_3MISS,NTRK
        ENDIF
C
C                                       
  100 CONTINUE
C
      CALL HPAK(49999,V)
  600 FORMAT(1X,I3,8X,F4.2,18X,I5,/)
  601 FORMAT(1X,I3,8X,'INSUFFICIENT STATISTICS',5X,I5,/)
C
  999 RETURN
      END

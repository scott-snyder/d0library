      SUBROUTINE PLCASV
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw a calorimeter side view outline.
C-
C-   Updated  17-JAN-1992   N. Oshima ( correct ECMH module )
C-   Updated  10-OCT-1991   N. Oshima ( Add Pbar & P labels )
C-   Created  11-APR-1990   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER I
      LOGICAL CONLY,EZERROR
C
      REAL YPF(5), ZPF(5)
      REAL YCEM(4),YCFH(4),YCCH(4),YEIF(4),YEIC(4),YOHM(4)
      REAL ZCEM(4),ZCFH(4),ZCCH(4),ZEIF(4),ZEIC(4),ZOHM(4)
      REAL YEMF(4),ZEMF(4),YEMC(4),ZEMC(4), YEEM(5),ZEEM(5)
C-
      DATA YCEM /84.14, 105.35, 105.35, 84.14/
      DATA ZCEM /-132., -132., 132., 132./
      DATA YCFH /105.35, 170.43, 170.43, 105.35/
      DATA ZCFH /-131.5, -131.5, 131.5, 131.5/
      DATA YCCH /170.43, 220., 220., 170.43/
      DATA ZCCH /-128.1, -110.5, 110.5, 128.1/
      DATA YEIF /4.89, 84.34, 84.34, 4.89/
      DATA ZEIF /198., 198., 296.51, 296.51/
      DATA YEIC /4.89, 84.34, 84.34, 4.89/
      DATA ZEIC /297.01, 297.01, 368.86, 368.86/
      DATA YEMF /88.44, 159.06, 159.06, 88.44/
      DATA ZEMF /197.58, 197.58, 268.51, 268.51/
      DATA YEMC /88.44, 159.06, 159.06, 88.44/
      DATA ZEMC /268.97, 268.97, 347., 347./
      DATA YOHM /161.83, 225., 225., 161.83/
      DATA ZOHM /192., 157., 304.,339./
C
      DATA YEEM /4.76, 93., 106., 106., 4.76/
      DATA ZEEM /170.12, 170.12, 180.00, 193.88, 193.88/
C----------------------------------------------------------------------
      CALL PXCOLR('FOR')
C-
C- CCEM
      CALL JPOLGN(ZCEM,YCEM,4)
      DO 10 I=1,4
   10 YPF(I) = -1.*YCEM(I)
      CALL JPOLGN(ZCEM,YPF,4)
C- CCFH
      CALL JPOLGN(ZCFH,YCFH,4)
      DO 20 I=1,4
   20 YPF(I) = -1.*YCFH(I)
      CALL JPOLGN(ZCFH,YPF,4)
C- CCCH
      CALL JPOLGN(ZCCH,YCCH,4)
      DO 30 I=1,4
   30 YPF(I) = -1.*YCCH(I)
      CALL JPOLGN(ZCCH,YPF,4)
C- ECIF
      CALL JPOLGN(ZEIF,YEIF,4)
      DO 40 I=1,4
   40 YPF(I) = -1.*YEIF(I)
      CALL JPOLGN(ZEIF,YPF,4)
      DO 41 I=1,4
   41 ZPF(I) = -1.*ZEIF(I)
      CALL JPOLGN(ZPF,YEIF,4)
      CALL JPOLGN(ZPF,YPF,4)
C- ECIC
      CALL JPOLGN(ZEIC,YEIC,4)
      DO 50 I=1,4
   50 YPF(I) = -1.*YEIC(I)
      CALL JPOLGN(ZEIC,YPF,4)
      DO 51 I=1,4
   51 ZPF(I) = -1.*ZEIC(I)
      CALL JPOLGN(ZPF,YEIC,4)
      CALL JPOLGN(ZPF,YPF,4)


C- ECMF
      CALL JPOLGN(ZEMF,YEMF,4)
      DO 60 I=1,4
   60 YPF(I) = -1.*YEMF(I)
      CALL JPOLGN(ZEMF,YPF,4)
      DO 61 I=1,4
   61 ZPF(I) = -1.*ZEMF(I)
      CALL JPOLGN(ZPF,YEMF,4)
      CALL JPOLGN(ZPF,YPF,4)
C- ECMC
      CALL JPOLGN(ZEMC,YEMC,4)
      DO 62 I=1,4
   62 YPF(I) = -1.*YEMC(I)
      CALL JPOLGN(ZEMC,YPF,4)
      DO 63 I=1,4
   63 ZPF(I) = -1.*ZEMC(I)
      CALL JPOLGN(ZPF,YEMC,4)
      CALL JPOLGN(ZPF,YPF,4)
C- ECOHM
      CALL JPOLGN(ZOHM,YOHM,4)
      DO 70 I=1,4
   70 YPF(I) = -1.*YOHM(I)
      CALL JPOLGN(ZOHM,YPF,4)
      DO 71 I=1,4
   71 ZPF(I) = -1.*ZOHM(I)
      CALL JPOLGN(ZPF,YOHM,4)
      CALL JPOLGN(ZPF,YPF,4)
C- ECEM
      CALL JPOLGN(ZEEM,YEEM,5)
      DO 80 I=1,5
   80 YPF(I) = -1.*YEEM(I)
      CALL JPOLGN(ZEEM,YPF,5)
      DO 81 I=1,5
   81 ZPF(I) = -1.*ZEEM(I)
      CALL JPOLGN(ZPF,YEEM,5)
      CALL JPOLGN(ZPF,YPF,5)
C-
C--- DRAW A LABEL FOR CAL. SIDE VIEW
      CALL PUGETV('CAL ONLY',CONLY)
      IF ( CONLY ) THEN
        CALL PXCOLR('GRE')
        CALL JJUST(1,2)
        CALL JSIZE(10.,16.)
        CALL JFONT(5)
        CALL J3MOVE( 363., 0., 0. )
        CALL JHSTRG('<-P')
        CALL J3MOVE( 387., 12., 0. )
        CALL J3DRAW( 396., 12., 0. )
        CALL J3MOVE(-397., 0., 0. )
        CALL JHSTRG('P->')
        CALL PXCOLR('FOR')
      ELSE
        CALL PXCOLR('GRE')
        CALL JJUST(1,2)
        CALL JSIZE(15.,18.)
        CALL JFONT(5)
        CALL J3MOVE( 363., 0., 0. )
        CALL JHSTRG('<-P')
        CALL J3MOVE( 399., 12., 0. )
        CALL J3DRAW( 410., 12., 0. )
        CALL J3MOVE(-410., 0., 0. )
        CALL JHSTRG('P->')
        CALL PXCOLR('FOR')
      ENDIF
C---
C-
  999 RETURN
      END

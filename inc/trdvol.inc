C TRDVOL.INC
      INTEGER NBVOL
      PARAMETER( NBVOL = 21 )
      COMMON /TRDVOL/TRDVL1(3),TRDVL2(3,3),TRDVL3(3,NBVOL,3),
     &        ZCENT(NBVOL),ZACT(3)
      REAL           TRDVL1   ,TRDVL2     ,TRDVL3           ,
     &        ZCENT       ,ZACT

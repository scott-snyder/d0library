      INTEGER PXMUHT             
      INTEGER PXMUDT             
      INTEGER PXMUTD             
      INTEGER PXMUTK             
      INTEGER PXMHTT             
      INTEGER PXMUTC             
      INTEGER PXMAG              
      INTEGER CHNUM              
      INTEGER PXMUD1             
C
C  Set up defaults... want everything on except Drift Time solutions -
C  (only seen when zooming..) and trigger counters - not well implemented
C  yet...
C  ======================================================================
C

      COMMON/PXMHTK/ PXMUHT,PXMUDT,PXMUTD,PXMUTK,PXMHTT,PXMUTC,PXMAG
     X ,CHNUM,PXMUD1

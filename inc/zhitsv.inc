C
C  COMMON /ZHITSV/ is for communication between z-strip digitization routines.
C  Contents:
C     NZHITS is total number of z-strip hits.
C     NZHTMX is maximum number of z-strip hits which can be stored.
C          If NZHITS > NZHTMX, extra hits are lost.
C     IZSORT is an integer on which the zhits are to be sorted.
C     IZINDX is index array giving order of hits sorted on IZSORT.
C     IZADR is z-strip address.
C     ZTIME is hit time in nanoseconds.
C     ZPH is pulse height in counts.
C     ZPW is pulse width in counts.
C     IDTRAK is GEANT track ID = secondary tk.#*2**11 + primary track #
C        T. Trippe, Sep. 22, 1986
C
      INTEGER NZHITS,MZHTMX,NZHTMX
      PARAMETER (MZHTMX=16000)
      INTEGER IZSORT(MZHTMX),IZINDX(MZHTMX),IZADR(MZHTMX),IDTRAK(MZHTMX)
      REAL ZTIME(MZHTMX),ZPH(MZHTMX),ZPW(MZHTMX)
      COMMON/ZHITSV/NZHITS,NZHTMX,IZSORT,IZINDX,IZADR,ZTIME,ZPH,ZPW,
     +              IDTRAK
C

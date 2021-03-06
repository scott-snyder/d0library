C----------------------------------------------------------------------
C-
C-   Created  18-JUL-1993   Brent J. May
C-   Updated  23-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      INTEGER MAXH
      PARAMETER (MAXH = 100)
      INTEGER IHOT_CELLS(3,MAXH), NHOT
      integer*4 LHOT(2,2000,-32:32,0:1)
      INTEGER LRUN(2000), NLRUN
      COMMON /HOTCOM/IHOT_CELLS,NHOT,LHOT,LRUN,NLRUN
C----------------------------------------------------------------------
      INTEGER IHOT_LIST(3,40,2) 
      INTEGER NVER, NLIST(2), HRUN(3)
      COMMON /HOTLIST/IHOT_LIST,NLIST,HRUN
      DATA NVER/2/                    
      DATA HRUN/0,59390,66000/        
      DATA NLIST/37,26/               
      DATA IHOT_LIST/
     &               -26,  11, 362,   
     &               -26,  12, 372,
     &               -25,  44, 527,
     &               -12,  31, 566,
     &               -10,  14, 338,
     &                -6,  61, 522,
     &                -5,  55, 773,
     &                -3,  63, 455,
     &                 1,  44, 490,
     &                 2,  23, 774,
     &                 3,  44, 604,
     &                 4,  44, 588,
     &                 5,  44, 486,
     &                 5,  46, 438,
     &                 6,  31, 710,
     &                 7,  44, 508,
     &                 7,  46, 433,
     &                 8,  44, 452,
     &                 8,  46, 409,
     &                 9,  46, 417,
     &                10,  46, 522,
     &                11,  46, 441,
     &                12,   9, 452,
     &                12,  46, 512,
     &                14,  14, 379,
     &                21,  49, 637,
     &                22,  49, 648,
     &                23,  49, 638,
     &                24,  49, 647,
     &                25,  49, 624,
     &                26,  49, 664,
     &                27,  49, 662,
     &                28,  49, 712,
     &                29,  49, 726,
     &                30,  49, 729,
     &                31,  49, 747,
     &                32,  49, 759,
     &                 0,   0,   0,
     &                 0,   0,   0,
     &                 0,   0,   0,
     &               -32,   4, 670,    
     &               -12,  31, 440,
     &               -21,  21, 350,    
     &               -10,  52, 518,
     &                -6,  26, 772,
     &                -3,  43, 609,
     &                -3,  44, 321,
     &                 4,  25, 775,
     &                 6,  31, 786,
     &                 9,  44, 581,
     &                12,   9, 563,
     &                14,  14, 413,
     &                17,  42, 372,
     &                21,  49, 563,
     &                22,  49, 574,
     &                23,  49, 495,
     &                24,  49, 513,
     &                25,  49, 457,
     &                26,  49, 487,
     &                27,  49, 423,
     &                28,  49, 461,
     &                29,  49, 512,
     &                30,  34, 330,
     &                30,  49, 440,
     &                31,  49, 363,
     &                32,  49, 301,
     &                 0,   0,   0,
     &                 0,   0,   0,
     &                 0,   0,   0,
     &                 0,   0,   0,
     &                 0,   0,   0,
     &                 0,   0,   0,
     &                 0,   0,   0,
     &                 0,   0,   0,
     &                 0,   0,   0,
     &                 0,   0,   0,
     &                 0,   0,   0,
     &                 0,   0,   0,
     &                 0,   0,   0,
     &                 0,   0,   0/
CC
CC Old list 
CC
C     &                6,  31,  780,
C     &                4,  25,  772,
C     &               -6,  26,  754,
C     &                9,  44,  622,
C     &               -3,  43,  565,
C     &               21,  49,  488,
C     &              -10,  52,  462,
C     &               22,  49,  451,
C     &               17,  42,  441,
C     &               12,   9,  370,
C     &              -21,  21,  350,
C     &               14,  14,  350,
C     &               -3,  44,  323,
C     &              -12,  19,  298,
C     &              -20,  38,  297,
C     &               18,   8,  282,
C     &              -18,  21,  268,
C     &                3,  31,  268,
C     &              -12,  31,  265,
C     &              -22,  60,  257,
C     &                5,  43,  256,
C     &               18,   9,  255,
C     &               -9,  21,  253,
C     &                2,  41,  253,
C     &               12,  26,  252
C     &                            /


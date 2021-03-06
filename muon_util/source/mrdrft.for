      FUNCTION MRDRFT(DRIFT,ANGLE1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Inverse of Function MSDRFT.
C-                         Calculate X from given drift time and angle.
C-
C-   Returned value  : MRDRFT ; X Distance between trach and wire (cm).
C-   Inputs  : DRIFT ; Drift time (nsec).
C-   Inputs  : ANGLE1 ; Incident angle of a track(deg).
C-
C-   Created   31-MAY-1991   Susumu Igarashi
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,ND_00,ND_05,ND_15,ND_25,ND_35,ND_45
      REAL    DRIFT,ANGLE1,ANGLE,X1,X2,MRDRFT
      PARAMETER(ND_00=99)
      REAL X_00(ND_00)
      REAL DRIFT_00(ND_00)
      PARAMETER(ND_05=101)
      REAL X_05(ND_05)
      REAL DRIFT_05(ND_05)
      PARAMETER(ND_15=106)
      REAL X_15(ND_15)
      REAL DRIFT_15(ND_15)
      PARAMETER(ND_25=111)
      REAL X_25(ND_25)
      REAL DRIFT_25(ND_25)
      PARAMETER(ND_35=118)
      REAL X_35(ND_35)
      REAL DRIFT_35(ND_35)
      PARAMETER(ND_45=125)
      REAL X_45(ND_45)
      REAL DRIFT_45(ND_45)
      DATA X_00/
     & 0.000, 0.040, 0.090, 0.140, 0.190, 0.240, 0.290, 0.330,
     & 0.380, 0.430, 0.480, 0.530, 0.580, 0.630, 0.680, 0.730,
     & 0.780, 0.830, 0.880, 0.930, 0.980, 1.030, 1.080, 1.130,
     & 1.180, 1.230, 1.280, 1.330, 1.380, 1.430, 1.480, 1.530,
     & 1.580, 1.630, 1.680, 1.730, 1.780, 1.830, 1.880, 1.930,
     & 1.980, 2.030, 2.080, 2.130, 2.180, 2.230, 2.280, 2.330,
     & 2.380, 2.430, 2.480, 2.530, 2.580, 2.630, 2.680, 2.730,
     & 2.780, 2.830, 2.880, 2.930, 2.980, 3.030, 3.080, 3.130,
     & 3.180, 3.230, 3.280, 3.330, 3.380, 3.430, 3.480, 3.530,
     & 3.590, 3.640, 3.690, 3.740, 3.790, 3.840, 3.890, 3.940,
     & 3.990, 4.040, 4.090, 4.140, 4.190, 4.240, 4.290, 4.340,
     & 4.390, 4.440, 4.490, 4.540, 4.590, 4.640, 4.690, 4.740,
     & 4.790, 4.840, 4.900/
      DATA DRIFT_00/
     & 0.00, 8.76, 8.76, 17.51, 26.27, 35.03, 43.78, 52.54,
     & 61.36, 70.22, 79.39, 88.37, 97.78, 107.42, 117.02, 127.39,
     & 136.52, 146.85, 157.18, 167.66, 178.19, 188.58, 199.23, 210.25,
     & 221.01, 232.14, 242.64, 253.68, 264.68, 275.67, 286.66, 297.63,
     & 308.52, 319.35, 330.29, 341.05, 351.87, 362.55, 373.22, 383.95,
     & 394.54, 405.18, 415.71, 426.21, 436.78, 447.24, 457.72, 468.14,
     & 478.54, 488.97, 499.35, 509.76, 520.12, 530.47, 540.86, 551.24,
     & 561.63, 572.05, 582.49, 592.90, 603.39, 613.87, 624.46, 634.92,
     & 645.54, 656.24, 667.00, 677.63, 688.55, 699.59, 710.90, 724.66,
     & 738.60, 748.41, 760.40, 774.78, 787.19, 798.05, 811.01, 824.10,
     & 837.26, 850.79, 864.08, 877.85, 891.78, 905.90, 920.19, 934.64,
     & 949.26, 963.84, 978.72, 994.04,1012.96,1028.11,1043.66,1058.65,
     &1070.07,1091.75,9999.00/
      DATA X_05/
     & 0.000, 0.040, 0.090, 0.140, 0.190, 0.240, 0.289, 0.339,
     & 0.389, 0.439, 0.489, 0.539, 0.588, 0.638, 0.688, 0.738,
     & 0.788, 0.837, 0.887, 0.937, 0.987, 1.037, 1.086, 1.136,
     & 1.186, 1.236, 1.286, 1.335, 1.385, 1.435, 1.485, 1.535,
     & 1.585, 1.634, 1.684, 1.734, 1.784, 1.834, 1.883, 1.933,
     & 1.983, 2.033, 2.083, 2.132, 2.182, 2.232, 2.282, 2.332,
     & 2.381, 2.431, 2.481, 2.531, 2.581, 2.631, 2.680, 2.730,
     & 2.780, 2.830, 2.880, 2.929, 2.979, 3.029, 3.079, 3.129,
     & 3.178, 3.228, 3.278, 3.328, 3.378, 3.428, 3.477, 3.527,
     & 3.577, 3.627, 3.677, 3.726, 3.776, 3.826, 3.876, 3.926,
     & 3.975, 4.025, 4.075, 4.125, 4.175, 4.224, 4.274, 4.324,
     & 4.374, 4.424, 4.474, 4.523, 4.573, 4.623, 4.673, 4.723,
     & 4.772, 4.822, 4.872, 4.922, 4.972/
      DATA DRIFT_05/
     & 0.00, 8.76, 8.76, 17.51, 26.27, 35.03, 43.78, 52.54,
     & 61.30, 70.10, 78.95, 88.01, 97.27, 106.68, 116.32, 126.16,
     & 136.20, 146.40, 156.73, 167.17, 177.72, 188.36, 199.06, 209.82,
     & 220.66, 231.58, 242.57, 253.58, 264.64, 275.73, 286.82, 297.78,
     & 308.82, 319.73, 330.58, 341.38, 352.08, 362.84, 373.50, 384.18,
     & 394.77, 405.43, 415.98, 426.48, 436.99, 447.51, 457.95, 468.37,
     & 478.76, 489.14, 499.51, 509.88, 520.26, 530.61, 540.96, 551.33,
     & 561.71, 572.15, 582.58, 593.04, 603.46, 613.94, 624.49, 635.02,
     & 645.64, 656.28, 666.94, 677.74, 688.49, 699.41, 710.54, 721.83,
     & 735.64, 747.27, 759.12, 771.11, 783.27, 795.68, 808.27, 821.06,
     & 834.05, 847.21, 860.58, 874.13, 887.90, 901.83, 915.95, 930.24,
     & 944.69, 959.31, 974.06, 988.94,1003.95,1019.06,1034.21,1049.45,
     &1064.74,1079.99,1096.88,1130.22,1319.77/
      DATA X_15/
     & 0.000, 0.036, 0.084, 0.132, 0.180, 0.229, 0.277, 0.325,
     & 0.374, 0.422, 0.470, 0.519, 0.567, 0.615, 0.663, 0.712,
     & 0.760, 0.808, 0.857, 0.905, 0.953, 1.002, 1.050, 1.098,
     & 1.146, 1.195, 1.243, 1.291, 1.340, 1.388, 1.436, 1.484,
     & 1.533, 1.581, 1.629, 1.678, 1.726, 1.774, 1.823, 1.871,
     & 1.919, 1.967, 2.016, 2.064, 2.112, 2.161, 2.209, 2.257,
     & 2.306, 2.354, 2.402, 2.450, 2.499, 2.547, 2.595, 2.644,
     & 2.692, 2.740, 2.788, 2.837, 2.885, 2.933, 2.982, 3.030,
     & 3.078, 3.127, 3.175, 3.223, 3.271, 3.320, 3.368, 3.416,
     & 3.465, 3.513, 3.561, 3.610, 3.658, 3.706, 3.754, 3.803,
     & 3.851, 3.899, 3.948, 3.996, 4.044, 4.092, 4.141, 4.189,
     & 4.237, 4.286, 4.334, 4.382, 4.431, 4.479, 4.527, 4.575,
     & 4.624, 4.672, 4.720, 4.769, 4.817, 4.865, 4.914, 4.962,
     & 5.010, 5.058/
      DATA DRIFT_15/
     & 0.00, 8.76, 8.76, 8.76, 17.51, 26.27, 35.03, 43.78,
     & 52.54, 61.36, 70.22, 79.39, 88.37, 97.78, 107.42, 117.02,
     & 127.39, 136.52, 146.85, 157.18, 160.70, 171.39, 182.19, 193.07,
     & 204.01, 215.05, 226.14, 237.28, 248.44, 259.62, 270.91, 282.17,
     & 293.41, 304.44, 315.56, 326.62, 337.42, 348.37, 359.26, 369.94,
     & 380.72, 391.31, 401.88, 415.71, 426.21, 436.78, 447.24, 457.72,
     & 468.14, 478.54, 488.97, 499.35, 509.76, 520.12, 530.47, 540.86,
     & 551.24, 561.63, 572.05, 582.49, 592.90, 603.39, 613.87, 624.46,
     & 634.92, 641.50, 652.00, 662.57, 673.10, 683.66, 693.95, 705.04,
     & 716.41, 727.72, 748.41, 760.40, 774.35, 786.55, 798.05, 811.01,
     & 823.75, 835.71, 848.39, 852.97, 866.41, 882.62, 896.60, 910.78,
     & 925.18, 942.91, 959.51, 977.38, 992.31,1007.51,1022.71,1038.05,
     &1053.49,1068.97,1084.85,1098.92,1119.69,1136.33,1184.18,1257.14,
     &1319.77,1451.43/
      DATA X_25/
     & 0.000, 0.029, 0.075, 0.120, 0.165, 0.211, 0.256, 0.301,
     & 0.347, 0.392, 0.437, 0.482, 0.528, 0.573, 0.618, 0.664,
     & 0.709, 0.754, 0.800, 0.845, 0.890, 0.936, 0.981, 1.026,
     & 1.072, 1.117, 1.162, 1.208, 1.253, 1.298, 1.343, 1.389,
     & 1.434, 1.479, 1.525, 1.570, 1.615, 1.661, 1.706, 1.751,
     & 1.797, 1.842, 1.887, 1.933, 1.978, 2.023, 2.069, 2.114,
     & 2.159, 2.204, 2.250, 2.295, 2.340, 2.386, 2.431, 2.476,
     & 2.522, 2.567, 2.612, 2.658, 2.703, 2.748, 2.794, 2.839,
     & 2.884, 2.930, 2.975, 3.020, 3.065, 3.111, 3.156, 3.201,
     & 3.247, 3.292, 3.337, 3.383, 3.428, 3.473, 3.519, 3.564,
     & 3.609, 3.655, 3.700, 3.745, 3.790, 3.836, 3.881, 3.926,
     & 3.972, 4.017, 4.062, 4.108, 4.153, 4.198, 4.244, 4.289,
     & 4.334, 4.380, 4.425, 4.470, 4.516, 4.561, 4.606, 4.651,
     & 4.697, 4.742, 4.787, 4.833, 4.878, 4.923, 4.969/
      DATA DRIFT_25/
     & 0.00, 8.76, 8.76, 8.76, 17.51, 26.27, 35.03, 43.78,
     & 52.54, 52.54, 61.36, 70.25, 79.38, 88.69, 98.16, 107.85,
     & 117.75, 127.83, 138.08, 148.45, 158.93, 169.53, 180.19, 190.92,
     & 201.72, 212.61, 223.60, 234.63, 245.69, 256.75, 267.93, 279.02,
     & 290.09, 301.18, 312.22, 323.06, 333.98, 344.80, 355.49, 366.24,
     & 376.88, 387.47, 398.13, 408.66, 419.18, 429.75, 440.21, 450.63,
     & 461.05, 471.55, 481.95, 492.30, 502.65, 512.98, 523.33, 533.70,
     & 544.08, 554.47, 564.76, 575.15, 585.57, 596.03, 606.45, 617.00,
     & 627.57, 638.05, 648.66, 659.41, 669.98, 680.61, 691.25, 702.33,
     & 713.79, 727.08, 738.24, 748.05, 758.10, 772.00, 779.65, 791.34,
     & 803.34, 815.67, 828.68, 841.81, 852.97, 866.41, 882.62, 896.60,
     & 905.80, 916.24, 931.14, 943.07, 959.51, 977.21, 992.66,1015.09,
     &1030.90,1047.06,1054.68,1070.80,1087.24,1103.57,1120.70,1136.33,
     &1163.17,1184.18,1218.50,1265.91,1319.77,1373.57,1451.43/
      DATA X_35/
     & 0.000, 0.021, 0.062, 0.103, 0.144, 0.185, 0.226, 0.267,
     & 0.308, 0.349, 0.390, 0.431, 0.471, 0.512, 0.553, 0.594,
     & 0.635, 0.676, 0.717, 0.758, 0.799, 0.840, 0.881, 0.922,
     & 0.963, 1.004, 1.045, 1.086, 1.127, 1.168, 1.209, 1.250,
     & 1.291, 1.332, 1.373, 1.414, 1.454, 1.495, 1.536, 1.577,
     & 1.618, 1.659, 1.700, 1.741, 1.782, 1.823, 1.864, 1.905,
     & 1.946, 1.987, 2.028, 2.069, 2.110, 2.151, 2.192, 2.233,
     & 2.274, 2.315, 2.356, 2.396, 2.437, 2.478, 2.519, 2.560,
     & 2.601, 2.642, 2.683, 2.724, 2.765, 2.806, 2.847, 2.888,
     & 2.929, 2.970, 3.011, 3.052, 3.093, 3.134, 3.175, 3.216,
     & 3.257, 3.298, 3.339, 3.379, 3.420, 3.461, 3.502, 3.543,
     & 3.584, 3.625, 3.666, 3.707, 3.748, 3.789, 3.830, 3.871,
     & 3.912, 3.953, 3.994, 4.035, 4.076, 4.117, 4.158, 4.199,
     & 4.240, 4.281, 4.321, 4.362, 4.403, 4.444, 4.485, 4.526,
     & 4.567, 4.608, 4.649, 4.690, 4.731, 4.772/
      DATA DRIFT_35/
     & 0.00, 8.76, 8.76, 8.76, 17.51, 26.27, 35.03, 35.03,
     & 43.78, 52.54, 61.36, 61.50, 70.53, 79.72, 89.12, 98.55,
     & 99.44, 109.37, 119.41, 129.65, 140.11, 143.99, 154.64, 166.22,
     & 176.91, 188.63, 200.66, 211.57, 223.58, 235.77, 247.57, 259.73,
     & 271.87, 283.63, 295.76, 298.68, 310.44, 321.75, 333.84, 345.45,
     & 362.80, 373.59, 384.82, 395.48, 406.43, 416.98, 427.58, 438.30,
     & 448.74, 459.15, 469.71, 480.06, 490.39, 500.96, 511.25, 521.54,
     & 531.88, 542.15, 552.38, 562.69, 573.02, 583.09, 593.44, 603.84,
     & 614.05, 624.49, 634.97, 645.18, 655.69, 665.97, 676.11, 686.48,
     & 696.96, 707.09, 717.61, 728.77, 732.20, 742.85, 753.81, 764.43,
     & 775.32, 791.34, 802.05, 810.15, 821.45, 832.93, 845.16, 856.28,
     & 867.53, 880.98, 895.00, 905.80, 920.50, 934.67, 947.54, 962.13,
     & 977.07, 992.32,1007.34,1021.25,1036.50,1052.47,1068.85,1086.50,
     &1102.51,1119.74,1137.22,1155.18,1172.74,1192.78,1218.50,1243.12,
     &1265.91,1319.77,1349.18,1420.48,1451.43,1496.82/
      DATA X_45/
     & 0.000, 0.035, 0.071, 0.106, 0.141, 0.177, 0.212, 0.247,
     & 0.283, 0.318, 0.354, 0.389, 0.424, 0.460, 0.495, 0.530,
     & 0.566, 0.601, 0.636, 0.672, 0.707, 0.742, 0.778, 0.813,
     & 0.849, 0.884, 0.919, 0.955, 0.990, 1.025, 1.061, 1.096,
     & 1.131, 1.167, 1.202, 1.237, 1.273, 1.308, 1.344, 1.379,
     & 1.414, 1.450, 1.485, 1.520, 1.556, 1.591, 1.626, 1.662,
     & 1.697, 1.732, 1.768, 1.803, 1.838, 1.874, 1.909, 1.945,
     & 1.980, 2.015, 2.051, 2.086, 2.121, 2.157, 2.192, 2.227,
     & 2.263, 2.298, 2.333, 2.369, 2.404, 2.440, 2.475, 2.510,
     & 2.546, 2.581, 2.616, 2.652, 2.687, 2.722, 2.758, 2.793,
     & 2.828, 2.864, 2.899, 2.934, 2.970, 3.005, 3.041, 3.076,
     & 3.111, 3.147, 3.182, 3.217, 3.253, 3.288, 3.323, 3.359,
     & 3.394, 3.429, 3.465, 3.500, 3.536, 3.571, 3.606, 3.642,
     & 3.677, 3.712, 3.748, 3.783, 3.818, 3.854, 3.889, 3.924,
     & 3.960, 3.995, 4.031, 4.066, 4.101, 4.137, 4.172, 4.207,
     & 4.243, 4.278, 4.313, 4.349, 4.384/
      DATA DRIFT_45/
     & 0.00, 8.76, 8.76, 17.51, 17.51, 26.27, 35.03, 35.03,
     & 43.78, 43.81, 52.57, 61.40, 61.50, 70.53, 79.72, 89.12,
     & 90.59, 99.44, 109.37, 119.41, 120.83, 130.94, 141.73, 143.99,
     & 154.64, 166.22, 176.91, 184.34, 196.66, 208.30, 220.22, 232.47,
     & 245.39, 256.98, 269.18, 281.10, 294.30, 298.68, 310.44, 321.75,
     & 333.84, 345.45, 364.74, 376.91, 388.25, 400.58, 411.86, 422.79,
     & 433.19, 444.07, 455.35, 466.61, 477.43, 487.76, 498.65, 509.03,
     & 521.29, 531.88, 542.15, 552.38, 562.69, 573.02, 583.09, 593.44,
     & 602.76, 611.46, 621.67, 631.58, 641.43, 651.12, 660.71, 671.12,
     & 681.24, 689.93, 700.14, 709.55, 719.16, 730.69, 732.20, 742.85,
     & 753.81, 764.43, 775.32, 786.99, 798.39, 810.15, 816.91, 826.23,
     & 836.91, 849.43, 861.92, 873.10, 886.65, 898.55, 905.80, 920.50,
     & 934.67, 947.54, 952.33, 964.49, 979.51, 998.24,1010.81,1021.25,
     &1036.50,1052.47,1068.12,1084.27,1102.35,1117.39,1127.96,1144.35,
     &1161.95,1180.99,1203.42,1222.79,1243.12,1265.91,1299.97,1319.77,
     &1349.18,1373.57,1420.48,1451.43,1496.82/
C----------------------------------------------------------------------
      ANGLE = ABS(ANGLE1)
      IF(ANGLE.GT.90..AND.ANGLE.LT.270.) ANGLE=180.-ANGLE
C
      IF(ANGLE.LT.5.)THEN
        I=0
   10   I=I+1
        IF(I.LT.ND_00) THEN
          IF(DRIFT.GE.DRIFT_00(I) .AND. DRIFT.LT.DRIFT_00(I+1))THEN
            X1=X_00(I)+(X_00(I+1)-X_00(I))
     &            *(DRIFT-DRIFT_00(I))/(DRIFT_00(I+1)-DRIFT_00(I))
            GOTO 11
          ELSE 
            GOTO 10
          ENDIF
        ELSEIF(I.GE.ND_00) THEN 
          MRDRFT=9999.00
          GOTO 999
        ENDIF

   11   I=0
   12   I=I+1
        IF(I.LT.ND_05) THEN
          IF(DRIFT.GE.DRIFT_05(I) .AND. DRIFT.LT.DRIFT_05(I+1))THEN
            X2=X_05(I)+(X_05(I+1)-X_05(I))
     &            *(DRIFT-DRIFT_05(I))/(DRIFT_05(I+1)-DRIFT_05(I))
            GOTO 13
          ELSE 
            GOTO 12
          ENDIF
        ELSEIF(I.GE.ND_05) THEN 
          MRDRFT=9999.00
          GOTO 999
        ENDIF

   13   MRDRFT=X1+(X2-X1)*(ANGLE-0.)/5.

      ELSEIF(ANGLE.GE.5. .AND. ANGLE.LT.15.) THEN
        I=0
   20   I=I+1
        IF(I.LT.ND_05) THEN
          IF(DRIFT.GE.DRIFT_05(I) .AND. DRIFT.LT.DRIFT_05(I+1))THEN
            X1=X_05(I)+(X_05(I+1)-X_05(I))
     &            *(DRIFT-DRIFT_05(I))/(DRIFT_05(I+1)-DRIFT_05(I))
            GOTO 21
          ELSE 
            GOTO 20
          ENDIF
        ELSEIF(I.GE.ND_05) THEN
          MRDRFT=9999.00
          GOTO 999
        ENDIF

   21   I=0
   22   I=I+1
        IF(I.LT.ND_15) THEN
          IF(DRIFT.GE.DRIFT_15(I) .AND. DRIFT.LT.DRIFT_15(I+1))THEN
            X2=X_15(I)+(X_15(I+1)-X_15(I))
     &            *(DRIFT-DRIFT_15(I))/(DRIFT_15(I+1)-DRIFT_15(I))
            GOTO 23
          ELSE 
            GOTO 22
          ENDIF
        ELSEIF(I.GE.ND_15) THEN
          MRDRFT=9999.00
          GOTO 999
        ENDIF

   23   MRDRFT=X1+(X2-X1)*(ANGLE-5.)/10.

      ELSEIF(ANGLE.GE.15. .AND. ANGLE.LT.25.) THEN
        I=0
   30   I=I+1
        IF(I.LT.ND_15) THEN
          IF(DRIFT.GE.DRIFT_15(I) .AND. DRIFT.LT.DRIFT_15(I+1))THEN
            X1=X_15(I)+(X_15(I+1)-X_15(I))
     &            *(DRIFT-DRIFT_15(I))/(DRIFT_15(I+1)-DRIFT_15(I))
            GOTO 31
          ELSE 
            GOTO 30
          ENDIF
        ELSEIF(I.GE.ND_15) THEN
          MRDRFT=9999.00
          GOTO 999
        ENDIF

   31   I=0
   32   I=I+1
        IF(I.LT.ND_25) THEN
          IF(DRIFT.GE.DRIFT_25(I) .AND. DRIFT.LT.DRIFT_25(I+1))THEN
            X2=X_25(I)+(X_25(I+1)-X_25(I))
     &            *(DRIFT-DRIFT_25(I))/(DRIFT_25(I+1)-DRIFT_25(I))
            GOTO 33
          ELSE 
            GOTO 32
          ENDIF
        ELSEIF(I.GE.ND_25) THEN
          MRDRFT=9999.00
          GOTO 999
        ENDIF

   33   MRDRFT=X1+(X2-X1)*(ANGLE-15.)/10.

      ELSEIF(ANGLE.GE.25. .AND. ANGLE.LT.35.) THEN
        I=0
   40   I=I+1
        IF(I.LT.ND_25) THEN
          IF(DRIFT.GE.DRIFT_25(I) .AND. DRIFT.LT.DRIFT_25(I+1))THEN
            X1=X_25(I)+(X_25(I+1)-X_25(I))
     &            *(DRIFT-DRIFT_25(I))/(DRIFT_25(I+1)-DRIFT_25(I))
            GOTO 41
          ELSE 
            GOTO 40
          ENDIF
        ELSEIF(I.GE.ND_25) THEN
          MRDRFT=9999.00
          GOTO 999
        ENDIF

   41   I=0
   42   I=I+1
        IF(I.LT.ND_35) THEN
          IF(DRIFT.GE.DRIFT_35(I) .AND. DRIFT.LT.DRIFT_35(I+1))THEN
            X2=X_35(I)+(X_35(I+1)-X_35(I))
     &            *(DRIFT-DRIFT_35(I))/(DRIFT_35(I+1)-DRIFT_35(I))
            GOTO 43
          ELSE 
            GOTO 42
          ENDIF
        ELSEIF(I.GE.ND_35) THEN
          MRDRFT=9999.00
          GOTO 999
        ENDIF

   43   MRDRFT=X1+(X2-X1)*(ANGLE-25.)/10.

      ELSEIF(ANGLE.GE.35. .AND. ANGLE.LT.45.) THEN
        I=0
   50   I=I+1
        IF(I.LT.ND_35) THEN
          IF(DRIFT.GE.DRIFT_35(I) .AND. DRIFT.LT.DRIFT_35(I+1))THEN
            X1=X_35(I)+(X_35(I+1)-X_35(I))
     &            *(DRIFT-DRIFT_35(I))/(DRIFT_35(I+1)-DRIFT_35(I))
            GOTO 51
          ELSE 
            GOTO 50
          ENDIF
        ELSEIF(I.GE.ND_35) THEN
          MRDRFT=9999.00
          GOTO 999
        ENDIF

   51   I=0
   52   I=I+1
        IF(I.LT.ND_45) THEN
          IF(DRIFT.GE.DRIFT_45(I) .AND. DRIFT.LT.DRIFT_45(I+1))THEN
            X2=X_45(I)+(X_45(I+1)-X_45(I))
     &            *(DRIFT-DRIFT_45(I))/(DRIFT_45(I+1)-DRIFT_45(I))
            GOTO 53
          ELSE 
            GOTO 52
          ENDIF
        ELSEIF(I.GE.ND_45) THEN
          MRDRFT=9999.00
          GOTO 999
        ENDIF

   53   MRDRFT=X1+(X2-X1)*(ANGLE-35.)/10.

      ELSEIF(ANGLE.GE.45.)THEN 
        I=0
   60   I=I+1
        IF(I.LT.ND_45) THEN
          IF(DRIFT.GE.DRIFT_45(I) .AND. DRIFT.LT.DRIFT_45(I+1))THEN
            MRDRFT=X_45(I)+(X_45(I+1)-X_45(I))
     &            *(DRIFT-DRIFT_45(I))/(DRIFT_45(I+1)-DRIFT_45(I))
            GOTO 61
          ELSE 
            GOTO 60
          ENDIF
        ELSEIF(I.GE.ND_45) THEN
          MRDRFT=9999.00
          GOTO 999
        ENDIF
   61   CONTINUE

      ENDIF

  999 RETURN
      END

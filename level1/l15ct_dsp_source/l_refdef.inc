******************************************************************************
*                                                                            *
*   File  L_RefDef.Inc                                  Rev.   22-APR-1994   *
*                                                                            *
* This is an include file which defines the external symbol references and   *
* definitions for the Local DSP Node-specific Initialization files.          *
*                                                                            *
* It is stored in an include file to allow easy changes to be made.          *
*                                                                            *
* It cannot be a separate file because it uses constants which must be       *
* defined in the Node-Specific Initialization module.                        *
*                                                                            *
* This file is included in each Node-Specific Initialization module.         *
*                                                                            *
******************************************************************************

         .def    Local_Mem_Ctrl_Reg_Value_Loc           ; Define these
         .def    Eta_Nplus2_Coord_Loc                   ; Symbols as external
         .def    Eta_Nplus3_Coord_Loc                   ; for use in other
         .def    Eta_Nplus4_Coord_Loc                   ; modules.
         .def    Eta_Nplus5_Coord_Loc
         .def    Eta_Nplus0_EM_Et_Zeresp_Loc
         .def    Eta_Nplus1_EM_Et_Zeresp_Loc
         .def    Eta_Nplus2_EM_Et_Zeresp_Loc
         .def    Eta_Nplus3_EM_Et_Zeresp_Loc
         .def    Eta_Nplus4_EM_Et_Zeresp_Loc
         .def    Eta_Nplus5_EM_Et_Zeresp_Loc
         .def    Eta_Nplus6_EM_Et_Zeresp_Loc
         .def    Eta_Nplus7_EM_Et_Zeresp_Loc
         .def    Eta_Nplus0_Tot_Et_Zeresp_Loc
         .def    Eta_Nplus1_Tot_Et_Zeresp_Loc
         .def    Eta_Nplus2_Tot_Et_Zeresp_Loc
         .def    Eta_Nplus3_Tot_Et_Zeresp_Loc
         .def    Eta_Nplus4_Tot_Et_Zeresp_Loc
         .def    Eta_Nplus5_Tot_Et_Zeresp_Loc
         .def    Eta_Nplus6_Tot_Et_Zeresp_Loc
         .def    Eta_Nplus7_Tot_Et_Zeresp_Loc
         .def    This_LDSP_Node_ID_Loc
         .def    This_LDSP_ObjectList_Handle
         .def    Mask_of_TT_Data_Blocks_Loc
         .def    Initialize_Node
         .def    This_LDSP_Ref_Set_Block_Handle
         .def    This_LDSP_to_TCC_Status_Handle
         .def    This_LDSP_to_68K_Status_Handle
         .def    LG_Next_DMA_Control_Reg_Handle
         .def    Setup_Mark_and_Force_Pass_DMA
         .def    Wait_for_Previous_DSP_Data



         .ref    Rack_1_EM_DMA_Finished_ISR_Loc         ; Symbols referenced
         .ref    Rack_1_Tot_DMA_Finished_ISR_Loc        ; in this module but
         .ref    Rack_2_EM_DMA_Finished_ISR_Loc         ; defined in external
         .ref    Rack_2_Tot_DMA_Finished_ISR_Loc        ; modules
         .ref    LG_Xfr_fr_Pr_1_Complete_ISR_Loc
         .ref    LG_Xfr_fr_Pr_3_Complete_ISR_Loc
         .ref    LG_Xfr_fr_Pr_4_Complete_ISR_Loc
         .ref    LG_Xfr_to_Next_Complete_ISR_Loc
         .ref    LG_Xfr_from_Prev_Status_Loc
         .ref    Rack_1_EM_Et_TT_Data_Loc
         .ref    Rack_1_Tot_Et_TT_Data_Loc
         .ref    Rack_2_EM_Et_TT_Data_Loc
         .ref    Rack_2_Tot_Et_TT_Data_Loc
         .ref    Ref_Set_Data_Loc
         .ref    MFP_Derived_Const_Length
         .ref    MFP_Derived_Const_Data_Loc
         .ref    Zero_Loc

         .ref    Com_Port_Status_Loc

         .ref    Start_LG_Xfr_to_Next_DSP

         .ref    SEK_to_all_DSP_Status_Handle
         .ref    DMA_Halt_Word_Loc
         .ref    DMA_Start_Initialize_Loc
         .ref    DMA_Start_LG_to_Next_w_MFP_Loc
         .ref    DMA_Start_LG_fr_Prev_w_MFP_Loc

         .ref    MFP_TT_Et_Header_Loc
         .ref    MFP_Ref_Set_Header_Loc

         .ref    Enable_DMAINT0_Loc
         .ref    ICRDY0_Read_Synch_Loc
         .ref    OCRDY0_Write_Synch_Loc
         .ref    Enable_DMAINT1_Loc
         .ref    ICRDY1_Read_Synch_Loc
         .ref    OCRDY1_Write_Synch_Loc
         .ref    Enable_DMAINT2_Loc
         .ref    ICRDY2_Read_Synch_Loc
         .ref    OCRDY2_Write_Synch_Loc
         .ref    Enable_DMAINT3_Loc
         .ref    ICRDY3_Read_Synch_Loc
         .ref    OCRDY3_Write_Synch_Loc
         .ref    Enable_DMAINT4_Loc
         .ref    ICRDY4_Read_Synch_Loc
         .ref    OCRDY4_Write_Synch_Loc
         .ref    Enable_DMAINT5_Loc
         .ref    ICRDY5_Read_Synch_Loc
         .ref    OCRDY5_Write_Synch_Loc

         .ref    Dummy_DMA_List_Loc
         .ref    Dummy_DMA_Control_Loc
         .ref    Dummy_DMA_Source_Loc
         .ref    Dummy_DMA_Source_Index_Loc
         .ref    Dummy_DMA_Counter_Loc
         .ref    Dummy_DMA_Destination_Loc
         .ref    Dummy_DMA_Dest_Index_Loc

         .ref    LDSP_A2_ObjectList_Loc
         .ref    LDSP_A3_ObjectList_Loc
         .ref    LDSP_A4_ObjectList_Loc
         .ref    LDSP_A1_ObjectList_Loc
         .ref    LDSP_B3_ObjectList_Loc
         .ref    LDSP_B4_ObjectList_Loc
         .ref    LDSP_B1_ObjectList_Loc
         .ref    LDSP_C3_ObjectList_Loc
         .ref    LDSP_C4_ObjectList_Loc
         .ref    LDSP_C1_ObjectList_Loc
         .ref    LDSP_C2_ObjectList_Loc

         .ref    LDSP_A2_Mark_Pass_Data_Loc
         .ref    LDSP_A3_Mark_Pass_Data_Loc
         .ref    LDSP_A4_Mark_Pass_Data_Loc
         .ref    LDSP_A1_Mark_Pass_Data_Loc
         .ref    LDSP_B3_Mark_Pass_Data_Loc
         .ref    LDSP_B4_Mark_Pass_Data_Loc
         .ref    LDSP_B1_Mark_Pass_Data_Loc
         .ref    LDSP_C3_Mark_Pass_Data_Loc
         .ref    LDSP_C4_Mark_Pass_Data_Loc
         .ref    LDSP_C1_Mark_Pass_Data_Loc
         .ref    LDSP_C2_Mark_Pass_Data_Loc

         .ref    LDSP_A2_Ref_Set_Block_Loc
         .ref    LDSP_A3_Ref_Set_Block_Loc
         .ref    LDSP_A4_Ref_Set_Block_Loc
         .ref    LDSP_A1_Ref_Set_Block_Loc
         .ref    LDSP_B3_Ref_Set_Block_Loc
         .ref    LDSP_B4_Ref_Set_Block_Loc
         .ref    LDSP_B1_Ref_Set_Block_Loc
         .ref    LDSP_C3_Ref_Set_Block_Loc
         .ref    LDSP_C4_Ref_Set_Block_Loc
         .ref    LDSP_C1_Ref_Set_Block_Loc
         .ref    LDSP_C2_Ref_Set_Block_Loc


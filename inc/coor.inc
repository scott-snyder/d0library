CONST
   Max_card_slot = 25;
   Max_comm = 63;
   Max_cable_seq = 7;
   Max_clock =24;
{   Max_crate = 255;}
   Max_crate = 15;
   Max_filter_bit = 127;
{   Max_filter_proc = 63;}
   Max_filter_proc = 49;
   Max_geo_sect = 31;
{   Max_HV_unit = 63;}
   Max_HV_unit = 7;
   Max_L2_param = 7;
   Max_L2_param_set = 15;
   Max_L2_tool = 19;
   Max_filter_line = Max_L2_tool;
   Max_L2_assigned = 19;
   Max_L2_type = 7;
   Max_mode_bit = 7;
   Max_pause_cntrl = 1;
{   Max_pulser = 63;}
   Max_pulser = 7;
{   Max_rec_stream = 31;}
   Max_rec_stream = 7;
{   Max_task = 63;}
   Max_task = 15;
   Max_trig_bit = 63;
   Max_trig_param = 63;
   Max_trig_term = 255;
   Max_trans_item = Max_filter_proc + Max_rec_stream + Max_trig_bit
                    + Max_filter_bit + Max_trig_param + Max_trig_term;
   Max_item = Max_trans_item + Max_clock + Max_cable_seq + Max_crate
             + Max_geo_sect + Max_pulser + Max_rec_stream + Max_HV_unit;

TYPE
   Activity_type = (NONE,COMMAND,CONFIG_REQ,UTILITY,INFO_REQ);
   Assigned_tool_index = 0..Max_L2_assigned;
   Cable_seq_index = 0..Max_cable_seq;
   Card_slot_index = 0..Max_card_slot;
   Clock_index = 0..Max_clock;
   Comm_index = 0..Max_comm;
   Crate_index = 0..Max_crate;
   Enable_disable = (Enabled,Disabled);
   Filter_bit_index = 0..Max_filter_bit;
   Filter_line_index = 0..Max_filter_line;
   Filter_proc_index = 0..Max_filter_proc;
   Geo_sect_index = 0..Max_geo_sect;
   Hardware_type = (MT,Cable_seq,Clock,Crate,Filter_bit,Filter_proc,
                    Geo_sect,HV_unit,Level_1_5,Pulser,Rec_stream,
                    Trig_bit,Trig_param,Trig_term,Trig_group,
                    End_group,End_file,Scale_ratio,Scale_ctl,
                    Auto_disa_ctl,Mode,Limit,Pedes,Gains,Cards,
                    Pause_cntrl,L2_type,Read_time);
   HV_unit_index = 0..Max_HV_unit;
   Item_index = 0..Max_item;
   L_name_types = (MT_type,Unique,Generic,Non_referent);
   L2_param_index = 0..Max_L2_param;
   L2_param_set_index = 0..Max_L2_param_set;
   L2_tool_index = 0..Max_L2_tool;
   L2_type_index = 0..Max_L2_type;
   Mode_bit_index = 0..Max_mode_bit;
   Node_name = (MTN,NUL,MON_0,MON_1,MON_2,TRIGGER,LOGGER,FILTER);
   On_Off = (On,Off);
   Param_types = (Undefined,A_File,A_Integer,A_Real);
   Pause_cntrl_index = 0..Max_pause_cntrl;
   Pulser_index = 0..Max_pulser;
   Rec_stream_index = 0..Max_rec_stream;
   Require_Veto = (Require,Veto);
   Run_states = (BEGUN,ENDED);
   Slot = (Empty,Active,Idle);
   Stat = (MTS,Missing,Free,Committed);
   Structure_stat = (Absent,Present);
   Task_index = 0..Max_task;
   Trans_item_index = 0..Max_trans_item;
   Trig_bit_index = 0..Max_trig_bit;
   Trig_param_index = 0..Max_trig_param;
   Trig_term_index = 0..Max_trig_term;
   Types_of_length = (Fixed,Infinite);
   Types_of_file = (Spanned, Non_spanned);
   Zero_one = 0..1;

   Param_value = RECORD
      Value_type : Param_types;
      CASE Param_types OF
        A_Real :
                (Real_value : REAL);
        A_Integer :
                (Integer_value : INTEGER);
        A_File :
                (File_name : VARYING[64] OF CHAR);
      END;

   L2_param = RECORD
      Name : VARYING[16] OF CHAR;
      Value : Param_value; 
      END;

   List_of_L2_params = RECORD
      Count : L2_param_index;
      List : ARRAY[L2_param_index] OF L2_param;
      END;

   Script_lines = RECORD
      Tool_name : Varying[32] OF CHAR;
      Tool_ptr : L2_tool_index;
      Tool_index : Assigned_tool_index;
      Param_set_ptr : L2_param_set_index;
      END;

   Filter_script = RECORD
      Line_count : Filter_line_index;
      Script_line : ARRAY[Filter_line_index] OF Script_lines;
      END;

   List_of_cable_seqs = RECORD
      Count : Cable_seq_index;
      List : ARRAY[Cable_seq_index] OF Cable_seq_index;
      END;

   List_of_cards = RECORD
      Count : Card_slot_index;
      List : ARRAY[Card_slot_index] OF Card_slot_index;
      END;

   List_of_clocks = RECORD
      Count : Clock_index;
      List : ARRAY[Clock_index] OF Clock_index;
      END;

   List_of_crates = RECORD
      Count : Crate_index;
      List : ARRAY[Crate_index] OF Crate_index;
      END;

   List_of_filter_bits = RECORD
      Count : Filter_bit_index;
      List : ARRAY[Filter_bit_index] OF Filter_bit_index;
      END;

   List_of_filter_procs = RECORD
      Count : Filter_proc_index;
      List : ARRAY[Filter_proc_index] OF Filter_proc_index;
      END;

   List_of_geo_sects = RECORD
      Count : Geo_sect_index;
      List : ARRAY[Geo_sect_index] OF Geo_sect_index;
      END;

   List_of_HV_units = RECORD
      Count : HV_unit_index;
      List : ARRAY[HV_unit_index] OF HV_unit_index;
      END;

  List_of_L2_tool_ptrs = RECORD
      Count : L2_tool_index;
      List : ARRAY[L2_tool_index] OF L2_tool_index;
      END;

   List_of_L2_type_ptrs = RECORD
      Count : L2_type_index;
      List : ARRAY[L2_type_index] OF L2_type_index;
      END;

   List_of_pause_cntrls = RECORD
      Count : Pause_cntrl_index;
      List : ARRAY[Pause_cntrl_index] OF Pause_cntrl_index;
      END;

   List_of_pulsers = RECORD
      Count : Pulser_index;
      List : ARRAY[Pulser_index] OF Pulser_index;
      END;

   List_of_rec_streams = RECORD
      Count : Rec_stream_index;
      List : ARRAY[Rec_stream_index] OF Rec_stream_index;
      END;

   List_of_trig_bits = RECORD
      Count : Trig_bit_index;
      List : ARRAY[Trig_bit_index] OF Trig_bit_index;
      END;

   List_of_trig_params = RECORD
      Count : Trig_param_index;
      List : ARRAY[Trig_param_index] OF Trig_param_index;
      END;

   List_of_trig_terms = RECORD
      Count : Trig_term_index;
      List : ARRAY[Trig_term_index] OF Trig_term_index;
      END;

   Cable_seq_descriptor = RECORD
      Status : Stat;
      Owners : List_of_trig_bits;
      Static_geo_sects : List_of_geo_sects;
      END;

   Clock_descriptor = RECORD
      Status : Stat;
      Owners : List_of_trig_bits;
      Pattern_ref : VARYING[64] OF CHAR;
      END;

   Crate_descriptor = RECORD                         
      Status : Stat;
      Log_name : VARYING[16] OF CHAR;
      Geo_sect_in : Geo_sect_index;
      Owners : List_of_trig_bits;
      Mode_ref : VARYING[16] OF CHAR;
      Limit_ref : VARYING[64] OF CHAR;
      Ped_ref : VARYING[64] OF CHAR;
      Gain_ref : VARYING[64] OF CHAR;
      Card_slot : ARRAY[Card_slot_index] OF Slot;
      END;

   Filter_bit_descriptor = RECORD
      Status : Stat;
      Owner_bit : Trig_bit_index;
      Name : VARYING[64] OF CHAR;
      Script : Filter_script;
      Force_pass : INTEGER;
      Must : BOOLEAN;
      Speed : REAL;
      Rec_streams : List_of_rec_streams;
      END;

   Filter_proc_descriptor = RECORD
      Status : Stat;
      Name : VARYING[8] OF CHAR;
      Type_ptr : L2_type_index;
      Owners : List_of_trig_bits;
      END;

   Geo_sect_descriptor = RECORD
      Status : Stat;
      Cable_seq_in : Cable_seq_index;
      Owners : List_of_trig_bits;
      Static_crates : List_of_crates;
      END;

   HV_unit_descriptor = RECORD
      Status : Stat;
      Owners : List_of_trig_bits;
      HV_value : INTEGER;
      END;

   Level_1_5_descriptor = RECORD
      Status : Stat;
      Owners : List_of_trig_bits;
      State : Enable_disable;
      Prescale_ratio : INTEGER;
      Prescale_cntrl : On_Off;
      And_or_bits : ARRAY[Require_veto,Trig_term_index] OF On_Off;
      END;      

   L2_parameter_set_descriptor = RECORD
      Script_owner : Filter_bit_index;
      Line_of_script : Filter_line_index;
      L2_params : List_of_L2_params;
      END;

   List_of_L2_param_sets = RECORD
      Count : L2_param_set_index;
      List : ARRAY[L2_param_set_index] OF L2_parameter_set_descriptor;
      END;

   L2_tool_descriptor = RECORD
      Defined : Boolean;
      Name : VARYING[32] OF CHAR;
      Index : Assigned_tool_index;
      Type_ptrs : List_of_L2_type_ptrs;
      Params : List_of_L2_params;
      L2_param_sets : List_of_L2_param_sets;
      END;

   L2_type_descriptor = RECORD
      Defined : Boolean;
      Name : VARYING[16] OF CHAR;
      Proc_ptrs : List_of_filter_procs;
      Tool_ptrs : List_of_L2_tool_ptrs;
      END;

   Pause_cntrl_descriptor = RECORD
      Status : Stat;
      Owners : List_of_trig_bits;
      Pause_switch : On_Off;
      END;

   Pulser_descriptor = RECORD
      Status : Stat;
      Owners : List_of_trig_bits;
      State : On_Off;
      Atten : INTEGER;
      DAC : INTEGER;
      Pattern : INTEGER;
      Count_limit : INTEGER;
      END;

   Rec_stream_descriptor = RECORD
      Status : Stat;
      Owner_filter_bits : List_of_filter_bits;
      Owner_task : Task_index;
      Logical_name : VARYING[12] OF CHAR;
      Physical_name : VARYING[14] OF CHAR;
      Spooled : BOOLEAN;
      End;         

   Task_descriptor = RECORD
      Status : Stat;
      Task_ID : INTEGER;
      Trig_bits : List_of_trig_bits;
      Streams : List_of_rec_streams;
      Run_state : Run_states;
      Run_number : INTEGER;
      Taping : ON_OFF;
      File_type : Types_of_file;
      Length_type : Types_of_length;
      Run_length : INTEGER;
      Description : VARYING[24] OF CHAR;
      END;

   Trig_bit_descriptor = RECORD
      Status : Stat;
      Owner : Task_index;
      Name : VARYING[64] OF CHAR;
      State : Enable_disable;
      Readout_msec : INTEGER;
      Prescale_ratio : INTEGER;
      Prescale_cntrl : On_Off;
      And_or_bits : ARRAY[Require_veto,Trig_term_index] OF On_Off;
      Trig_terms : List_of_trig_terms;
      Trig_params : List_of_trig_params;
      Clocks : List_of_clocks;
      Level_1_5s : List_of_trig_bits;                 
      Auto_disable_cntrl : On_Off;
      Crates : List_of_crates;
      Geo_sects : List_of_Geo_sects;
      Cable_seqs : List_of_cable_seqs;
      HV_units : List_of_HV_units;
      Pause_cntrls : List_of_pause_cntrls;
      Pulsers : List_of_pulsers;
      L2_types : List_of_L2_type_ptrs;
      Filter_procs : List_of_filter_procs;
      Filter_bits : List_of_filter_bits;
      END;      

   Trig_param_descriptor = RECORD
      Status : Stat;
      Owners : List_of_trig_bits;
      Value : Param_value;
      END;

   Trig_term_descriptor = RECORD
      Status : Stat;
      Owners : List_of_trig_bits;
      END;

   System_config = RECORD
      Cable_seq_table : ARRAY[Cable_seq_index] OF Cable_seq_descriptor;
      Clock_table : ARRAY[Clock_index] OF Clock_descriptor;
      Crate_table : ARRAY[Crate_index] OF Crate_descriptor;
      Filter_bit_table : ARRAY[Filter_bit_index] OF Filter_bit_descriptor;
      Filter_proc_table : ARRAY[Filter_proc_index] OF Filter_proc_descriptor;
      Geo_sect_table : ARRAY[Geo_sect_index] OF Geo_sect_descriptor;
      HV_unit_table : ARRAY[HV_unit_index] OF HV_unit_descriptor;
      L2_tool_table : ARRAY[L2_tool_index] OF L2_tool_descriptor;
      L2_type_table : ARRAY[L2_type_index] OF L2_type_descriptor;
      Level_1_5_table :ARRAY[Trig_bit_index] OF Level_1_5_descriptor;
      Pause_cntrl_table : ARRAY[Pause_cntrl_index] OF Pause_cntrl_descriptor;
      Pulser_table : ARRAY[Pulser_index] OF Pulser_descriptor;
      Rec_stream_table : ARRAY[Rec_stream_index] OF Rec_stream_descriptor;
      Task_table : ARRAY[Task_index] OF Task_descriptor;
      Trig_bit_table : ARRAY[Trig_bit_index] OF Trig_bit_descriptor;
      Trig_param_table : ARRAY[Trig_param_index] OF Trig_param_descriptor;
      Trig_term_table : ARRAY[Trig_term_index] OF Trig_term_descriptor;
      END;

   Trans_entry = RECORD
      Logical_name : VARYING[16] OF CHAR;
      Dev_type : Hardware_type;
      Dev_index : INTEGER;
      END;

   Trans_list = RECORD
      Count : Trans_item_index;
      Items : ARRAY[Trans_item_index] OF Trans_entry;
      END;

   Task_list = ARRAY[Task_index] OF Trans_list;

   Master_entry = RECORD
      Logical_name : VARYING[16] OF CHAR;
      Dev_type : Hardware_type;
      Dev_index :INTEGER;
      L_name_type : L_name_types;
      Gen_iden : INTEGER;
      Group : BOOLEAN;
      Grp_iden : INTEGER;
      END;

   Master_table = RECORD
      Count : Item_index;
      Items : ARRAY[Item_index] OF Master_entry;
      END;

   Comm_entry = RECORD
      Chan_valid : BOOLEAN;
      Proc_ID : INTEGER;
      Task_valid : BOOLEAN;
      Task : Task_index;
      END;

   Comm_list = ARRAY[Comm_index] OF Comm_entry;

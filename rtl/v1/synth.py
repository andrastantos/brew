# This file contains helpers to drive the synthesis of a top-level module all the way from Silicon source to gates and reports

from typing import Sequence, Tuple

class QuartusException(Exception):
    pass

class QuartusFlow(object):
    def __init__(self, top_level: str, source_files: Sequence[str], clocks: Sequence[Tuple[str, int]], *, target_dir="quartus", project_name=None):
        from pathlib import Path

        self.source_files = source_files
        self.clocks = clocks
        self.target_dir = target_dir
        self.project_name = project_name if project_name is not None else top_level.lower()
        self.top_level: str = top_level
        self.project_file_name = Path(self.target_dir) / f"{self.project_name}.qpf"

    def generate(self):
        from datetime import datetime
        import os
        from pathlib import Path

        now_str = datetime.now().strftime('%H:%M:%S  %B %d, %Y')
        file_base_name = self.project_name

        # Step 0: create output directory
        target_dir = Path(self.target_dir)
        target_dir.mkdir(parents=True, exist_ok=True)

        # Step 1: generate SDC files
        constraint_file_name = None
        if self.clocks is not None:
            constraint_file_name = target_dir / f"{file_base_name}.sdc"
            with open(constraint_file_name, "wt") as clock_file:
                for clock, speed in self.clocks:
                    clock_file.write(f"create_clock -name {clock} -period {1000/speed:.2f} [get_ports {{{clock}}}]\n")
                clock_file.write(f"\nderive_clock_uncertainty\n")

        # Step 2: generate project file
        with open(self.project_file_name, "wt") as project_file:
            project_file.write(f"QUARTUS_VERSION = \"21.1\"\n")
            project_file.write(f"DATE = \"{now_str}\"\n")
            project_file.write(f"PROJECT_REVISION = \"{file_base_name}\"\n")

        output_directory = "output_files"
        qsf_file_name = target_dir / f"{file_base_name}.qsf"
        with open(qsf_file_name, "wt") as project_file:
            project_file.write(f"set_global_assignment -name FAMILY \"MAX 10\"\n")
            project_file.write(f"set_global_assignment -name DEVICE 10M04SAE144A7G\n")
            project_file.write(f"set_global_assignment -name TOP_LEVEL_ENTITY {self.top_level}\n")
            project_file.write(f"set_global_assignment -name ORIGINAL_QUARTUS_VERSION 21.1.0\n")
            project_file.write(f"set_global_assignment -name PROJECT_OUTPUT_DIRECTORY {output_directory}\n")
            project_file.write(f"set_global_assignment -name MIN_CORE_JUNCTION_TEMP \"-40\"\n")
            project_file.write(f"set_global_assignment -name MAX_CORE_JUNCTION_TEMP 125\n")
            project_file.write(f"set_global_assignment -name ERROR_CHECK_FREQUENCY_DIVISOR 256\n")
            #project_file.write(f"set_global_assignment -name EDA_SIMULATION_TOOL "Questa Intel FPGA (Verilog)"\n")
            #project_file.write(f"set_global_assignment -name EDA_TIME_SCALE "1 ps" -section_id eda_simulation\n")
            #project_file.write(f"set_global_assignment -name EDA_OUTPUT_DATA_FORMAT "VERILOG HDL" -section_id eda_simulation\n")
            #project_file.write(f"set_global_assignment -name EDA_GENERATE_FUNCTIONAL_NETLIST OFF -section_id eda_board_design_timing\n")
            #project_file.write(f"set_global_assignment -name EDA_GENERATE_FUNCTIONAL_NETLIST OFF -section_id eda_board_design_symbol\n")
            #project_file.write(f"set_global_assignment -name EDA_GENERATE_FUNCTIONAL_NETLIST OFF -section_id eda_board_design_signal_integrity\n")
            #project_file.write(f"set_global_assignment -name EDA_GENERATE_FUNCTIONAL_NETLIST OFF -section_id eda_board_design_boundary_scan\n")
            #project_file.write(f"set_global_assignment -name POWER_PRESET_COOLING_SOLUTION ""23 MM HEAT SINK WITH 200 LFPM AIRFLOW""\n")
            #project_file.write(f"set_global_assignment -name POWER_BOARD_THERMAL_MODEL ""NONE (CONSERVATIVE)""\n")
            project_file.write(f"set_global_assignment -name PHYSICAL_SYNTHESIS_REGISTER_RETIMING ON\n")
            project_file.write(f"set_global_assignment -name FLOW_ENABLE_POWER_ANALYZER ON\n")
            #project_file.write(f"set_global_assignment -name POWER_DEFAULT_INPUT_IO_TOGGLE_RATE ""12.5 %""\n")
            for source in self.source_files:
                relative_source = Path(os.path.relpath(str(Path(source).absolute()), str(target_dir.absolute())))
                project_file.write(f"set_global_assignment -name SYSTEMVERILOG_FILE {relative_source}\n")
            if constraint_file_name is not None:
                relative_constraint = Path(os.path.relpath(str(Path(constraint_file_name).absolute()), str(target_dir.absolute())))
                project_file.write(f"set_global_assignment -name SDC_FILE {relative_constraint}\n")
            #project_file.write(f"set_global_assignment -name PARTITION_NETLIST_TYPE SOURCE -section_id Top\n")
            #project_file.write(f"set_global_assignment -name PARTITION_FITTER_PRESERVATION_LEVEL PLACEMENT_AND_ROUTING -section_id Top\n")
            #project_file.write(f"set_global_assignment -name PARTITION_COLOR 16764057 -section_id Top\n")
            #project_file.write(f"set_instance_assignment -name PARTITION_HIERARCHY root_partition -to | -section_id Top\n")

        # Step 3: generate timing report TCL script
        if self.clocks is not None:
            self.tcl_name = target_dir / f"{file_base_name}.timing_report.tcl"
            timing_report_file = Path(output_directory) / f"{file_base_name}.timing_report.txt"
            with open(self.tcl_name, "wt") as tcl_file:
                tcl_file.write(f"set PRJ {self.project_file_name}\n")
                tcl_file.write(f"set OUT {str(timing_report_file)}\n")
                tcl_file.write(f"project_open $PRJ\n")
                tcl_file.write(f"create_timing_netlist\n")
                tcl_file.write(f"read_sdc\n")
                tcl_file.write(f"update_timing_netlist\n")
                tcl_file.write(f"report_sdc -file $OUT\n")
                tcl_file.write(f"report_clocks -file $OUT -append\n")
                tcl_file.write(f"report_clock_fmax_summary -file $OUT -append\n")
                for clock, speed in self.clocks:
                    tcl_file.write(f"report_timing -from_clock {{{clock}}} -to_clock {{{clock}}} -setup -npaths 10 -detail summary -multi_corner -file $OUT -append\n")
                for clock, speed in self.clocks:
                    tcl_file.write(f"report_timing -from_clock {{{clock}}} -to_clock {{{clock}}} -setup -npaths 10 -detail full_path -multi_corner -file $OUT -append\n")

    def run(self):
        # Step 1: run tools
        tools = ("quartus_map", "quartus_fit", "quartus_asm", "quartus_sta")
        from subprocess import run
        for tool in tools:
            cmd = (tool, str(self.project_file_name))
            print(f"")
            print(f"")
            print(f"*******************************************************************")
            print(f"* Running tool '{' '.join(cmd)}' ")
            print(f"*******************************************************************")
            result = run(cmd)
            if result.returncode != 0:
                raise QuartusException(f"Quartus tool {tool} failed with error code {result.returncode}")

        if self.clocks is not None:
            cmd = ("quartus_sta", "-t", str(self.tcl_name))
            print(f"")
            print(f"")
            print(f"*******************************************************************")
            print(f"* Running tool '{' '.join(cmd)}' ")
            print(f"*******************************************************************")
            result = run(cmd)
            if result.returncode != 0:
                raise QuartusException(f"Quartus tool {cmd[0]} failed with error code {result.returncode}")


    def timing_results(self):
        quartus_sta -s

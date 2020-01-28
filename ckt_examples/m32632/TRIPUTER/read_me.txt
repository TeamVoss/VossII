Welcome to TRIPUTER.

This is the test environment for the CPU M32632. It is based on the FPGA board Cyclone V GX
Starter Kit from Terasic. The kit uses an FPGA from Intel (former Altera).

Here comes a list of the files in the directory:

TRIPUTER.qws         the project file for Quartus
TRIPUTER.qsf         the definition file for the project
TRIPUTER.v           the top level of the design hierarchy for the FPGA
TRIPUTER.sdc         the timing definition file
TRIPUTER_SIMU.v      the top level of the design hierarchy for a simulation
TRIPUTER.sof         the configuration file for the FPGA
TRIPUTER_V01.pdf     a detailed description of the functionality of TRIPUTER
boot_rom.txt         the content of the boot rom
monitor.32K          the assembler source code of the boot rom
read_me.txt          this file

Please note: the communication over the serial link requires a carriage-return followed by
a line-feed to enter the command.

If you have any problems please write an email to fpga@cpu-ns32k.net

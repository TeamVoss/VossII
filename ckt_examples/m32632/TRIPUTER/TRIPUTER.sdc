## Copyright (C) 1991-2008 Altera Corporation
## Your use of Altera Corporation's design tools, logic functions 
## and other software and tools, and its AMPP partner logic 
## functions, and any output files from any of the foregoing 
## (including device programming or simulation files), and any 
## associated documentation or information are expressly subject 
## to the terms and conditions of the Altera Program License 
## Subscription Agreement, Altera MegaCore Function License 
## Agreement, or other applicable license agreement, including, 
## without limitation, that your use is for the sole purpose of 
## programming logic devices manufactured by Altera and sold by 
## Altera or its authorized distributors.  Please refer to the 
## applicable agreement for further details.


## VENDOR  "Altera"
## PROGRAM "Quartus II"
## VERSION "Version 8.1 Build 163 10/28/2008 SJ Web Edition"

## DATE    "Mon Mar 08 15:49:34 2010"

##
## DEVICE  "EP4CE22F17C6"
##

#**************************************************************
# Time Information
#**************************************************************

set_time_format -unit ns -decimal_places 3

#**************************************************************
# Create Clock
#**************************************************************

create_clock -name {clock_ref}  -period 20.000 -waveform { 0.000 10.000 } [get_ports {RCLK}]

#**************************************************************
# Create Generated Clock
#**************************************************************

#derive_pll_clocks
#create_generated_clock -name {clock_mclk}  -source [get_ports {CLK}]   -divide_by 1 -multiply_by  4 [get_pins {MPLL|altpll_component|auto_generated|pll1|clk[0]}]

#**************************************************************
# Set Clock Latency
#**************************************************************



#**************************************************************
# Set Clock Uncertainty
#**************************************************************

set_clock_uncertainty 0.1 -to clock_ref

#**************************************************************
# Set Input Delay
#**************************************************************

set_input_delay -clock clock_ref 5 [get_ports {RST_N}]
set_input_delay -clock clock_ref 5 [get_ports {SSW[*}]
set_input_delay -clock clock_ref 5 [get_ports {UA_RX}]
set_input_delay -clock clock_ref 5 [get_ports {SRDB[*}]

#**************************************************************
# Set Output Delay
#**************************************************************

set_output_delay -clock clock_ref  5 [get_ports {UA_TX}]
set_output_delay -clock clock_ref  5 [get_ports {SRAA[*}]
set_output_delay -clock clock_ref  5 [get_ports SRCO[4]]
set_output_delay -clock clock_ref  5 [get_ports SRCO[3]]
set_output_delay -clock clock_ref  5 -clock_fall [get_ports SRCO[2]]
set_output_delay -clock clock_ref  5 [get_ports SRCO[1]]
set_output_delay -clock clock_ref  5 [get_ports SRCO[0]]
set_output_delay -clock clock_ref  5 [get_ports {SRDB[*}]
set_output_delay -clock clock_ref  5 [get_ports {HEX*}]
set_output_delay -clock clock_ref  5 [get_ports {LED*}]

#**************************************************************
# Set Clock Groups
#**************************************************************


#**************************************************************
# Set False Path
#**************************************************************

# Control Signals from one clock domain to another clock domain

#**************************************************************
# Set Multicycle Path
#**************************************************************


#**************************************************************
# Set Maximum Delay
#**************************************************************


#**************************************************************
# Set Minimum Delay
#**************************************************************


#**************************************************************
# Set Input Transition
#**************************************************************


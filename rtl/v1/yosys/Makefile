DEVICE = hx8k
PNR_DEVICE = 8k
PNR_PACKAGE = tq144:4k

SOURCES = ./pipeline.v
PCF = pipeline.pcf

all: ice.bin

ice.blif: $(SOURCES) Makefile
	yosys -p "synth_ice40 -blif ice.blif" $<

ice.bin: ice.blif ice.pcf
#	arachne-pnr -d $(PNR_DEVICE) -P $(PNR_PACKAGE) -p $(PCF) ice.blif -o ice.asc
	nextpnr-ice40 -d $(PNR_DEVICE) -P $(PNR_PACKAGE) -p $(PCF) ice.blif -o ice.asc
	icebox_explain ice.asc > ice.ex
	icetime -d $(DEVICE) ice.asc
	icepack ice.asc ice.bin

# BSVTools library integration for BlueAXI.
ifndef BLUEAXI_MK_INCLUDED
BLUEAXI_MK_INCLUDED := 1

BLUEAXI_ROOT := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
BLUEAXI_SRC := $(BLUEAXI_ROOT)/src
EXTRA_BSV_LIBS += $(BLUEAXI_SRC)

$(info Adding BlueAXI from $(BLUEAXI_SRC))

endif

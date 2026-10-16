ifndef BLUEAXI_MK_INCLUDED
BLUEAXI_MK_INCLUDED := 1

BLUEAXI_ROOT := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
BLUEAXI_SRC := $(BLUEAXI_ROOT)/src

# Some helper packages use a BlueLib supplied by the parent project.
BLUEAXI_BLUELIB_MK := $(firstword $(wildcard $(CURDIR)/libraries/*/BlueLib.mk))
ifneq ($(BLUEAXI_BLUELIB_MK),)
include $(BLUEAXI_BLUELIB_MK)
endif

EXTRA_BSV_LIBS += $(BLUEAXI_SRC)

$(info Adding BlueAXI from $(BLUEAXI_SRC))

endif

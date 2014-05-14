
LLVM = -I$(shell llvm-config-3.3 --includedir) -L$(shell llvm-config-3.3 --libdir)

ifneq ($(Apple_PubSub_Socket_Render),)
	BREWPREFIX := $(shell brew --prefix 2>/dev/null || echo)

	LIBS := -L$(BREWPREFIX)/lib
	INCS := -I$(BREWPREFIX)/include
	DEFS := -DOSX -Dsincos=__sincos
	LLVM += -lLLVM-3.3
else
	LIBS :=
	INCS :=
	LLVM += -lLLVM-3.3
endif

#CXX = g++-4.8
CXX = clang++
CALCULON = -I$(HOME)/nonshared/calculon/include

DEFS += \
	-Wno-c++11-narrowing

LIBS += \
	$(CALCULON) \
	$(LLVM) \
	$(shell pkg-config libpng --libs) \
	-lnoise \
	-lboost_program_options \
	-lboost_iostreams

all: terrainmaker terrainmaker-dbg

terrainmaker: terrainmaker.cc Makefile $(wildcard *.h)
	$(CXX) -std=c++11 -O3 -DNDEBUG -fomit-frame-pointer -o $@ $< $(DEFS) $(INCS) $(LIBS)
	
terrainmaker-dbg: terrainmaker.cc Makefile $(wildcard *.h)
	$(CXX) -std=c++11 -g -o $@ $< $(DEFS) $(INCS) $(LIBS)
	
	

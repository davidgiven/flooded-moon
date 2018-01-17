
LLVM = -I$(shell llvm-config-5.0 --includedir) -L$(shell llvm-config-5.0 --libdir)

ifneq ($(Apple_PubSub_Socket_Render),)
	BREWPREFIX := $(shell brew --prefix 2>/dev/null || echo)

	LIBS := -L$(BREWPREFIX)/lib
	INCS := -I$(BREWPREFIX)/include
	DEFS := -DOSX -Dsincos=__sincos
	LLVM += -lLLVM-5.0
else
	LIBS :=
	INCS :=
	LLVM += -lLLVM-5.0
endif

#CXX = g++-4.8
CXX = clang++
CALCULON = -I$(HOME)/nonshared/calculon/include

DEFS += \

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
	
	

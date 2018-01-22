
#CXX = g++
CXX = clang++
INCS += -I$(HOME)/nonshared/calculon/include
INCS += -I$(shell llvm-config-5.0 --includedir)

CXXFLAGS = \
	-std=c++11 \
	-I$(HOME)/nonshared/calculon/include \
	-I/usr/include/llvm-5.0 \
	-I/usr/include/llvm-c-5.0

DEFS += \

LIBS += \
	$(shell pkg-config libpng --libs) \
	-L/usr/lib/llvm-5.0 \
	-lLLVM-5.0 \
	-lnoise \
	-lboost_program_options \
	-lboost_iostreams \
	-lpng

SRCS = \
	writer.cc \
	map.cc \
	pds.cc \
	pdslabel.cc \
	pdsset.cc \
	matrix.cc \
	variables.cc \
	functions.cc \
	terrainmaker.cc

RELEASEOBJS = $(patsubst %.cc,.obj/release/%.o,$(SRCS))
DEBUGOBJS = $(patsubst %.cc,.obj/debug/%.o,$(SRCS))
	
all: terrainmaker terrainmaker-dbg

terrainmaker: $(DEBUGOBJS) Makefile $(wildcard *.h)
	$(CXX) -O3 -DNDEBUG -fomit-frame-pointer -o $@ $(DEBUGOBJS) $(LIBS)
	
terrainmaker-dbg: $(RELEASEOBJS) Makefile $(wildcard *.h)
	$(CXX) -g -o $@ $(RELEASEOBJS) $(LIBS)
	
.obj/release/%.o: %.cc
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) -c -O3 -DNDEBUG -fomit-frame-pointer -o $@ $<

.obj/debug/%.o: %.cc
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) -c -g -o $@ $<

clean:
	rm -rf .obj


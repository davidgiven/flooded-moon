
#CXX = g++
CXX = clang++
INCS += -I$(HOME)/nonshared/calculon/include
INCS += -I$(shell llvm-config-5.0 --includedir)

CXXFLAGS = \
	-I$(HOME)/nonshared/calculon/include \
	-I$(shell llvm-config --includedir)

DEFS += \

LIBS += \
	$(shell pkg-config libpng --libs) \
	$(shell llvm-config --libs) \
	-lnoise \
	-lboost_program_options \
	-lboost_iostreams \
	-lpng

SRCS = \
	sphericalroam.cc \
	writer.cc \
	plywriter.cc \
	povwriter.cc \
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

DEPS = $(patsubst %.o,%.d,$(RELEASEOBJS) $(DEBUGOBJS))
	
all: terrainmaker terrainmaker-dbg

terrainmaker-dbg: $(DEBUGOBJS) Makefile $(wildcard *.h)
	$(CXX) -g -o $@ $(DEBUGOBJS) $(LIBS)
	
terrainmaker: $(RELEASEOBJS) Makefile $(wildcard *.h)
	$(CXX) -O3 -DNDEBUG -fomit-frame-pointer -o $@ $(RELEASEOBJS) $(LIBS)
	
.obj/release/%.o: %.cc
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) -MMD -MF $(patsubst %.o,%.d,$@) -c -O3 -DNDEBUG -fomit-frame-pointer -o $@ $<

.obj/debug/%.o: %.cc
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS)  -MMD -MF $(patsubst %.o,%.d,$@) -c -g -o $@ $<

clean:
	rm -rf .obj

-include $(DEPS)

